# app.R — versão “safe” para shinyapps.io e GitHub Pages (Shinylive)

# ---- Pacotes base (sem showtext/sysfonts aqui) ----
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(tibble)
library(withr)
library(readr)

is_shinylive <- isTRUE(getOption("shinylive.active"))
MAX_SAMPLES  <- if (is_shinylive) 200 else 2000
MAX_PERIODS  <- if (is_shinylive) 50  else 500

# na UI:
numericInput("n_samples", "Amostras (n)", value = 10, min = 1, max = MAX_SAMPLES, step = 1)
numericInput("n_periods", "Períodos",     value = 12, min = 1, max = MAX_PERIODS,  step = 1)

# ---- Fontes: só fora do Shinylive ----
# Usa showtext/sysfonts apenas em shinyapps.io / local nativo.
gg_family <- NULL
if (!is_shinylive) {
  # Carrega namespaces se disponíveis (sem exigir em Shinylive)
  have_sysfonts <- requireNamespace("sysfonts", quietly = TRUE)
  have_showtext  <- requireNamespace("showtext",  quietly = TRUE)

  add_font_safe <- function(family, regular, bold = NULL, italic = NULL, bolditalic = NULL) {
    if (!have_sysfonts || !have_showtext) return(invisible(FALSE))
    if (!file.exists(regular)) {
      message("Fonte não encontrada: ", regular)
      return(invisible(FALSE))
    }
    sysfonts::font_add(family,
      regular = regular, bold = bold, italic = italic, bolditalic = bolditalic
    )
    showtext::showtext_auto(enable = TRUE)
    TRUE
  }

  if (add_font_safe(
    family     = "timesnewroman",
    regular    = file.path("assets","fontes","timesnewroman-regular.ttf"),
    bold       = file.path("assets","fontes","timesnewroman-bold.ttf"),
    italic     = file.path("assets","fontes","timesnewroman-italic.ttf"),
    bolditalic = file.path("assets","fontes","timesnewroman-bolditalic.ttf")
  )) {
    gg_family <- "timesnewroman"
    message("✔️ 'timesnewroman' registrada via showtext/sysfonts.")
  } else {
    message("ℹ️ Sem fontes personalizadas (seguindo com padrão do sistema).")
  }
} else {
  message("Shinylive ativo: pulando showtext/sysfonts (use fontes web na UI).")
}

# ---- Seu código auxiliar/global (objetos, funções, dados, etc.) ----
# Deixe source() depois dos pacotes e do bloco de fontes
source("main.R")

# --- helpers visuais (ok manter acima) ---
theme_safe <- function(...) {
  if (requireNamespace("beautyxtrar", quietly = TRUE)) beautyxtrar::theme_xtra(...)
  else theme_minimal(...)
}

order_strats <- function(df, matrix_game = NULL) {
  if (!is.null(matrix_game) && is.list(matrix_game) && !is.null(matrix_game$strategy)) {
    ord <- unique(c(
      matrix_game$strategy$s1[[1]], matrix_game$strategy$s1[[2]],
      matrix_game$strategy$s2[[1]], matrix_game$strategy$s2[[2]]
    ))
  } else ord <- unique(df$estrategias)
  dplyr::mutate(df, estrategias = factor(as.character(estrategias), levels = ord, ordered = TRUE))
}

# --- padronização + proporção (idem) ---
.standardize_names <- function(df_long, model_tag) {
  rn <- tolower(names(df_long))
  rename_if <- function(target, alts){
    if (!(target %in% names(df_long))) {
      hit <- which(rn %in% tolower(alts))
      if (length(hit)) names(df_long)[hit[1]] <<- target
    }
  }
  rename_if("periodo", c("periodo","period"))
  rename_if("jogador", c("jogador","player"))
  rename_if("estrategias", c("estrategias","estrategia","strategy"))
  rename_if("estrategia_escolhida", c("estrategia_escolhida","chosen","choice"))
  if (!("type" %in% names(df_long)))   df_long$type   <- model_tag
  if (!("sim_id" %in% names(df_long))) df_long$sim_id <- 1L
  df_long$periodo <- suppressWarnings(as.integer(df_long$periodo))
  df_long
}

standardize_and_prop <- function(df_long, model_tag) {
  df_long <- .standardize_names(df_long, model_tag)
  need_cols <- c("estrategias","estrategia_escolhida","jogador","periodo")
  shiny::validate(shiny::need(all(need_cols %in% names(df_long)),
    sprintf("Não encontrei colunas para derivar proporção: preciso de %s.",
            paste0("'", need_cols, "'", collapse = ", "))))
  df_prop <- df_long %>%
    dplyr::mutate(hit = as.integer(estrategia_escolhida == estrategias)) %>%
    dplyr::group_by(type, sim_id, jogador, periodo, estrategias) %>%
    dplyr::summarise(prop = mean(hit, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(prop = pmin(pmax(prop, 0), 1))
  list(long = df_long, prop = df_prop)
}

# --- AQUI: defina run_sim ANTES do server ---
run_sim <- function(which_game = c("BSG","MEG"), model_tag = "EWA",
                    lambda = 0.5, n_samples = 10, n_periods = 12, seed = NULL) {
  which_game <- match.arg(which_game)
  matriz <- if (which_game == "BSG") if (exists("matriz_bsg")) matriz_bsg else NULL
            else                      if (exists("matriz_meg")) matriz_meg else NULL

  lt <- if (exists("learning_types")) learning_types else list(
    EWA = list(type = "EWA", delta = 0.75, rho = 0.31, phi = 0.62),
    RL  = list(type = "reinforcement", delta = 0,  rho = 0,   phi = 1),
    BL  = list(type = "belief",        delta = 1,  rho = 1,   phi = 1)
  )

  sim_call <- function() sim_lambda(
    matriz         = matriz,
    learning_types = lt,
    lambda_values  = c(lambda),
    game_label     = which_game,
    n_samples      = n_samples,
    n_periods      = n_periods
  )
  res <- if (is.null(seed)) sim_call() else withr::with_seed(as.integer(seed), sim_call())

  shiny::validate(shiny::need(exists("build_simulation_df"),
    "build_simulation_df() não encontrada no main.R"))
  df_long <- build_simulation_df(res, matriz = matriz, jogo = which_game)

  out <- standardize_and_prop(df_long, model_tag)
  out$prop <- order_strats(out$prop, matriz)
  out
}

# opcional: fail-fast no start
if (!exists("run_sim")) stop("run_sim não está carregada — verifique app.R/main.R")

plot_model <- function(df_prop, model_tag, titulo, palette = NULL) {
  d <- df_prop
  if ("type" %in% names(d) && (model_tag %in% unique(d$type))) d <- d %>% filter(type == !!model_tag)
  d <- d %>% filter(!is.na(prop), !is.na(periodo))
  validate(need(nrow(d) > 0, "Sem dados para plotar."))

  p <- d %>%
    ggplot(aes(periodo, prop, col = estrategias, group = interaction(estrategias, jogador))) +
    geom_line(linewidth = 1) +
    scale_y_continuous(limits = c(0,1), labels = label_comma(decimal.mark=",", big.mark=".")) +
    scale_x_continuous(breaks = breaks_pretty(n = 4)) +
    facet_wrap(~jogador) +
    labs(x="Período", y="Proporção", col="Estratégias", title=titulo) +
    theme_safe(base_size = 14) +
    theme(text = element_text(family = gg_family)) +
    guides(col = guide_legend(override.aes = list(linewidth = 1.2)))
  if (!is.null(palette)) p <- p + scale_color_manual(values = palette)
  p
}

# =================== UI ===================
ui <- page_fluid(
  theme = bs_theme(
    bootswatch   = "cosmo",
    # Fontes web seguras (funcionam no Shinylive/GitHub Pages)
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  title = "EWA Market Dynamics — Shiny",
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h4("Parâmetros"),
      sliderInput("lambda", HTML("λ (lambda)"), min = 0.1, max = 1.0, value = 0.5, step = 0.1),
      numericInput("n_samples", "Amostras (n)", value = 10, min = 1, step = 1),
      numericInput("n_periods", "Períodos", value = 12, min = 1, step = 1),
      actionButton("rerun", "Re-simular", class = "btn btn-primary"),
      checkboxInput("fix_seed", "Fixar semente", value = FALSE),
      numericInput("seed_val", "Semente", value = 1234, min = 1, step = 1),
      checkboxInput("debug", "Mostrar debug (head)", FALSE),
      hr(),
      h5("Baixar resultado (CSV)"),
      selectInput("dl_model", "Modelo", choices = c("EWA","RL","BL"), selected = "EWA"),
      selectInput("dl_game",  "Jogo",   choices = c("BSG","MEG"),    selected = "BSG"),
      radioButtons("dl_kind", "Tipo", choices = c("Agregado (proporções)" = "prop",
                                                  "Micro (linhas)"         = "long")),
      downloadButton("download_csv", "Baixar CSV", class = "btn btn-success"),
      helpText("A simulação só recalcula quando você clicar em Re-simular.")
    ),
    card(
      card_header("Modelos de Aprendizagem"),
      navset_pill(
        nav_panel("Experience-Weighted Attraction (EWA)",
          layout_columns(
            col_widths = c(6,6),
            card(card_header("Buyer–Seller Game (BSG)"),
                 plotOutput("ewa_bsg", height = "360px"),
                 conditionalPanel("input.debug", verbatimTextOutput("dbg_ewa_bsg"))),
            card(card_header("Market Entry Game (MEG)"),
                 plotOutput("ewa_meg", height = "360px"),
                 conditionalPanel("input.debug", verbatimTextOutput("dbg_ewa_meg")))
          )
        ),
        nav_panel("Reinforcement Learning (RL)",
          layout_columns(
            col_widths = c(6,6),
            card(card_header("Buyer–Seller Game (BSG)"),
                 plotOutput("rl_bsg", height = "360px"),
                 conditionalPanel("input.debug", verbatimTextOutput("dbg_rl_bsg"))),
            card(card_header("Market Entry Game (MEG)"),
                 plotOutput("rl_meg", height = "360px"),
                 conditionalPanel("input.debug", verbatimTextOutput("dbg_rl_meg")))
          )
        ),
        nav_panel("Belief-based Learning (BL)",
          layout_columns(
            col_widths = c(6,6),
            card(card_header("Buyer–Seller Game (BSG)"),
                 plotOutput("bl_bsg", height = "360px"),
                 conditionalPanel("input.debug", verbatimTextOutput("dbg_bl_bsg"))),
            card(card_header("Market Entry Game (MEG)"),
                 plotOutput("bl_meg", height = "360px"),
                 conditionalPanel("input.debug", verbatimTextOutput("dbg_bl_meg")))
          )
        )
      )
    ),
    card(
      collapsible = TRUE,
      card_header("Sobre mim"),
      card_body(
        HTML('
          <div style="text-align:center; margin-top:8px; margin-bottom:4px;">
            <img src="https://img.shields.io/badge/Maintained%3F-yes-green.svg" alt="maintained" style="height:20px;">
          </div>
          <h4 style="text-align:center; margin-top:6px;">
            Dinâmicas de Aprendizado em Cenários de Incertezas de Mercado:<br>
            uma aplicação de teoria dos jogos com Experience-Weighted Attraction
          </h4>
          <p style="text-align:center; margin-top:6px;">
            Trabalho de Conclusão de Curso — <em>data</em>
          </p>
        ')
      )
    )
  )
)

# =================== SERVER ===================
server <- function(input, output, session) {

  # Dispara apenas no botão; lê valores com isolate()
  params <- eventReactive(input$rerun, {
    list(
      lambda    = isolate(input$lambda),
      n_samples = isolate(input$n_samples),
      n_periods = isolate(input$n_periods),
      seed      = if (isTRUE(isolate(input$fix_seed)))
                    as.integer(isolate(input$seed_val))
                  else as.integer(sample.int(.Machine$integer.max, 1))
    )
  }, ignoreInit = TRUE)

  # ---- Simulações por aba/modelo/jogo (guardam long+prop) ----
  # EWA
  ewa_bsg <- eventReactive(input$rerun, { p <- params(); run_sim("BSG","EWA", p$lambda, p$n_samples, p$n_periods, seed = p$seed) }, ignoreInit = TRUE)
  ewa_meg <- eventReactive(input$rerun, { p <- params(); run_sim("MEG","EWA", p$lambda, p$n_samples, p$n_periods, seed = p$seed) }, ignoreInit = TRUE)
  # RL
  rl_bsg  <- eventReactive(input$rerun, { p <- params(); run_sim("BSG","RL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed) }, ignoreInit = TRUE)
  rl_meg  <- eventReactive(input$rerun, { p <- params(); run_sim("MEG","RL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed) }, ignoreInit = TRUE)
  # BL
  bl_bsg  <- eventReactive(input$rerun, { p <- params(); run_sim("BSG","BL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed) }, ignoreInit = TRUE)
  bl_meg  <- eventReactive(input$rerun, { p <- params(); run_sim("MEG","BL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed) }, ignoreInit = TRUE)

  # ---- Plots ----
  pal <- if (exists("color_main")) color_main else NULL

  output$ewa_bsg <- renderPlot({
    req(ewa_bsg())
    print(plot_model(
      ewa_bsg()[["prop"]], "EWA",
      sprintf("BSG — EWA (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim=TRUE))),
      palette = pal
    ))
  })

  output$ewa_meg <- renderPlot({
    req(ewa_meg())
    print(plot_model(
      ewa_meg()[["prop"]], "EWA",
      sprintf("MEG — EWA (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim=TRUE))),
      palette = pal
    ))
  })

  output$rl_bsg <- renderPlot({
    req(rl_bsg())
    print(plot_model(
      rl_bsg()[["prop"]], "RL",
      sprintf("BSG — RL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim=TRUE))),
      palette = pal
    ))
  })

  output$rl_meg <- renderPlot({
    req(rl_meg())
    print(plot_model(
      rl_meg()[["prop"]], "RL",
      sprintf("MEG — RL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim=TRUE))),
      palette = pal
    ))
  })

  output$bl_bsg <- renderPlot({
    req(bl_bsg())
    print(plot_model(
      bl_bsg()[["prop"]], "BL",
      sprintf("BSG — BL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim=TRUE))),
      palette = pal
    ))
  })

  output$bl_meg <- renderPlot({
    req(bl_meg())
    print(plot_model(
      bl_meg()[["prop"]], "BL",
      sprintf("MEG — BL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim=TRUE))),
      palette = pal
    ))
  })

  # ---- Debug (head do agregado) ---- 
  output$dbg_ewa_bsg <- renderPrint({ req(ewa_bsg()); utils::head(ewa_bsg()[["prop"]]) })
  output$dbg_ewa_meg <- renderPrint({ req(ewa_meg()); utils::head(ewa_meg()[["prop"]]) })
  output$dbg_rl_bsg  <- renderPrint({ req(rl_bsg());  utils::head(rl_bsg()[["prop"]]) })
  output$dbg_rl_meg  <- renderPrint({ req(rl_meg());  utils::head(rl_meg()[["prop"]]) })
  output$dbg_bl_bsg  <- renderPrint({ req(bl_bsg());  utils::head(bl_bsg()[["prop"]]) })
  output$dbg_bl_meg  <- renderPrint({ req(bl_meg());  utils::head(bl_meg()[["prop"]]) })

  # ---- Download CSV ----
  .pick_df <- function(model, game, kind) {
    rx <- switch(paste(model, game, sep = "_"),
      "EWA_BSG" = ewa_bsg(), "EWA_MEG" = ewa_meg(),
      "RL_BSG"  = rl_bsg(),  "RL_MEG"  = rl_meg(),
      "BL_BSG"  = bl_bsg(),  "BL_MEG"  = bl_meg(),
      NULL
    )
    validate(need(!is.null(rx), "Execute uma simulação (Re-simular) antes de baixar."))
    df <- rx[[kind]]
    validate(need(!is.null(df) && nrow(df) > 0, "Sem dados para exportar."))
    if (kind == "prop") {
      df %>% arrange(jogador, estrategias, periodo)
    } else {
      keep <- intersect(c("type","sim_id","amostra","jogador","periodo",
                          "estrategias","estrategia_escolhida"), names(df))
      df[, keep, drop = FALSE] %>% arrange(jogador, periodo)
    }
  }

  output$download_csv <- downloadHandler(
    filename = function() {
      sprintf("sim_%s_%s_%s_%s.csv",
              tolower(input$dl_model), tolower(input$dl_game),
              tolower(input$dl_kind), format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    content = function(file) {
      df <- .pick_df(input$dl_model, input$dl_game, input$dl_kind)
      readr::write_csv(df, file)
    }
  )
}

shinyApp(ui, server)