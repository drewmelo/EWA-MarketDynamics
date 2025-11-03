# app.R — seguro para shinyapps.io e GitHub Pages (Shinylive)

# ---- Pacotes base ----
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(tibble)
library(withr)
library(readr)
library(glue)
library(rgamer)
library(reactable)   # para tabela interativa
library(janitor)     # já usa em summary_tables()
library(plotly)
library(bsicons)   # ícones Bootstrap (para o hambúrguer)
# (opcional) Excel estilizado
library(openxlsx)
library(shinybusy)   # <<< ADICIONE

# ---- Scripts auxiliares ----
base::source("scripts/funcoes/funcao_dados_auxiliar.R")
base::source("scripts/funcoes/funcao_processar_simulacoes.R")
base::source("scripts/funcoes/funcao_sim_lambda.R")
#base::source("scripts/payoffs.R")

# ---- Configurações iniciais ----
color_main <- c("#0B86CA", "#566876", "#9AADB2", "#B1283AFF")

# Buyer–Seller Game (BSG)
matriz_bsg <- rgamer::normal_form(
  players  = c("Vendedor", "Comprador"),
  s1       = c("Preço Alto", "Preço Baixo"),
  s2       = c("Aceitar", "Rejeitar"),
  # ordem: (PA, A) | (PB, A) | (PA, R) | (PB, R)
  payoffs1 = c(20, 10, 0, 0),
  payoffs2 = c(-7.242103, 2.771034, 0, 0)
)

# Market Entry Game (MEG)
matriz_meg <- rgamer::normal_form(
  players  = c("Empresa A", "Empresa B"),
  s1       = c("Não Entrar", "Entrar"),
  s2       = c("Não Entrar", "Entrar"),
  payoffs1 = c(0, 5, 0, -1),
  payoffs2 = c(0, 0, 5, -1)
)

lambda_values <- seq(0.1, 1, by = 0.1)

learning_types <- list(
  EWA = list(type = "EWA",          delta = 0.75, rho = 0.31, phi = 0.62),
  RL  = list(type = "reinforcement", delta = 0,    rho = 0,    phi = 1),
  BL  = list(type = "belief",        delta = 1,    rho = 1,    phi = 1)
)

# ---- Flags de ambiente (apenas se outros scripts usarem) ----
is_shinylive <- Sys.getenv("SHINYLIVE") == "1" || nzchar(Sys.getenv("WEBR_ROOT"))
can_fonts    <- FALSE  # não usaremos fontes locais
assign("is_shinylive", is_shinylive, envir = .GlobalEnv)
assign("can_fonts",    can_fonts,    envir = .GlobalEnv)

# ---- Helpers visuais ----
theme_safe <- function(...) {
  ggplot2::theme_minimal(...)
}

order_strats <- function(df, matrix_game = NULL) {
  if (!is.null(matrix_game) && is.list(matrix_game) && !is.null(matrix_game$strategy)) {
    ord <- unique(c(
      matrix_game$strategy$s1[[1]],
      matrix_game$strategy$s1[[2]],
      matrix_game$strategy$s2[[1]],
      matrix_game$strategy$s2[[2]]
    ))
  } else ord <- unique(df$estrategias)
  dplyr::mutate(df, estrategias = factor(as.character(estrategias), levels = ord, ordered = TRUE))
}

# ---- Padronização + proporção ----
.standardize_names <- function(df_long, model_tag) {
  rn <- tolower(names(df_long))
  rename_if <- function(target, alts){
    if (!(target %in% names(df_long))) {
      hit <- which(rn %in% tolower(alts))
      if (length(hit)) names(df_long)[hit[1]] <<- target
    }
  }
  rename_if("periodo",              c("periodo","period"))
  rename_if("jogador",              c("jogador","player"))
  rename_if("estrategias",          c("estrategias","estrategia","strategy"))
  rename_if("estrategia_escolhida", c("estrategia_escolhida","chosen","choice"))
  if (!("type" %in% names(df_long))) df_long$type <- model_tag
  if (!("sim_id" %in% names(df_long))) df_long$sim_id <- 1L
  df_long$periodo <- suppressWarnings(as.integer(df_long$periodo))
  df_long
}

standardize_and_prop <- function(df_long, model_tag) {
  df_long <- .standardize_names(df_long, model_tag)
  need_cols <- c("estrategias","estrategia_escolhida","jogador","periodo")
  shiny::validate(shiny::need(all(need_cols %in% names(df_long)),
    sprintf("Não encontrei colunas para derivar proporção: preciso de %s.", paste0("'", need_cols, "'", collapse = ", "))
  ))
  df_prop <- df_long %>%
    dplyr::mutate(hit = as.integer(estrategia_escolhida == estrategias)) %>%
    dplyr::group_by(type, sim_id, jogador, periodo, estrategias) %>%
    dplyr::summarise(prop = mean(hit, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(prop = pmin(pmax(prop, 0), 1))
  # >>> acrescente isto:
  if ("jogo" %in% names(df_long)) {
    df_prop$jogo <- df_long$jogo[1]
  }
  list(long = df_long, prop = df_prop)
}

# ---- Simulação ----
run_sim <- function(which_game = c("BSG","MEG"), model_tag = "EWA", lambda = 0.5, n_samples = 10, n_periods = 12, seed = NULL) {
  which_game <- match.arg(which_game)
  matriz <- if (which_game == "BSG") if (exists("matriz_bsg")) matriz_bsg else NULL else if (exists("matriz_meg")) matriz_meg else NULL
  lt <- if (exists("learning_types")) learning_types else list(
    EWA = list(type = "EWA",          delta = 0.75, rho = 0.31, phi = 0.62),
    RL  = list(type = "reinforcement", delta = 0,    rho = 0,    phi = 1),
    BL  = list(type = "belief",        delta = 1,    rho = 1,    phi = 1)
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
  shiny::validate(shiny::need(exists("build_simulation_df"), "build_simulation_df() não encontrada no main.R"))
  df_long <- build_simulation_df(res, matriz = matriz, jogo = which_game)
  # --- NOVO: etiqueta do jogo em todas as linhas ---
  df_long$jogo <- which_game # >>>>>>> ADICIONAR ESTAS 2 LINHAS AQUI <<<<<<<
  if (!"lambda" %in% names(df_long)) df_long$lambda <- lambda
  df_long$lambda <- as.numeric(df_long$lambda)
  out <- standardize_and_prop(df_long, model_tag)
  out$prop <- order_strats(out$prop, matriz)
  out
}

if (!exists("run_sim")) stop("run_sim não está carregada — verifique app.R/main.R")

# ---- Plotagem ----
plot_model <- function(df_prop, model_tag, titulo, palette = NULL) {
  d <- df_prop
  if ("type" %in% names(d) && (model_tag %in% unique(d$type))) d <- d %>% dplyr::filter(type == !!model_tag)
  d <- d %>% dplyr::filter(!is.na(prop), !is.na(periodo))
  shiny::validate(shiny::need(nrow(d) > 0, "Sem dados para plotar."))
  p <- d %>% ggplot(aes(periodo, prop, col = estrategias, group = interaction(estrategias, jogador))) +
    geom_line(linewidth = 0.6) +
    scale_y_continuous(limits = c(0,1), labels = label_comma(decimal.mark=",", big.mark=".")) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
    facet_wrap(~jogador) +
    labs(
      x = "Período",
      y = "Proporção",
      color = NULL, # <- remove título da legenda (preferir "color" explícito)
      title = titulo
    ) +
    theme_safe(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),   # ↓ fonte da legenda
      legend.key.height = grid::unit(8,  "pt"), # ↓ altura do “key”
      legend.key.width  = grid::unit(12, "pt"), # ↓ largura do “key”
      legend.spacing.x  = grid::unit(3,  "pt")  # ↓ espaçamento horizontal
    ) +
    guides(
      color = guide_legend(
        title = NULL,
        nrow = 1, # mantém em 1 linha (opcional)
        keyheight = grid::unit(8,  "pt"),
        keywidth  = grid::unit(12, "pt"),
        override.aes = list(linewidth = 0.5) # ↓ linha do símbolo da legenda
      )
    )
  # Se usar paleta manual, zere o "name" da scale também:
  if (!is.null(palette)) p <- p + scale_color_manual(name = NULL, values = palette)
  p
}

short_legend <- function(p, legend_size = 10, itemwidth = 28){
  g <- plotly::ggplotly(p, dynamicTicks = FALSE)
  plotly::layout(
    g,
    legend = list(
      font = list(size = legend_size),
      itemsizing = "constant",
      itemwidth = itemwidth  # ↓ menor = traço mais curto na legenda
    )
  )
}

# ====== FUNÇÕES DE MEDIDAS-RESUMO ======
summary_statistics <- function(df, sufixo) {
  if (!"lambda" %in% names(df)) df$lambda <- NA_real_
  df |>
    dplyr::filter(estrategia_escolhida == estrategias) |>
    dplyr::group_by(type, lambda) |>
    dplyr::summarise(
      media   = round(mean(atracao,   na.rm = TRUE), 1),
      mediana = round(stats::median(atracao, na.rm = TRUE), 1),
      dp      = round(stats::sd(atracao,    na.rm = TRUE), 1),
      min     = round(min(atracao,         na.rm = TRUE), 1),
      max     = round(max(atracao,         na.rm = TRUE), 1),
      iqr      = round(IQR(atracao), 1),
      .groups = "drop"
    ) |>
    dplyr::rename_with(~ paste0(.x, "_", sufixo), .cols = 3:8)
}

summary_tables <- function(modelo, lista_resumo) {
  lista_resumo$mr_bsg |>
    dplyr::filter(type == modelo) |>
    dplyr::bind_cols(
      lista_resumo$mr_meg |>
        dplyr::filter(type == modelo)
    ) |>
    janitor::clean_names() |>
    dplyr::select(-type_9, -lambda_10) |>
    dplyr::rename(type = "type_1", lambda = "lambda_2")
}

# Função para calcular frequência e média de probabilidades
frequency_probabilities <- function(df) {
  df |>
    dplyr::filter(estrategia_escolhida == estrategias) |>
    dplyr::group_by(type, lambda, jogador, estrategia_escolhida) |>
    dplyr::reframe(
      media_prob = base::round(base::mean(probabilidade), 3),
      cont_n     = base::sum(dplyr::n())
    ) |>
    dplyr::group_by(type, lambda, jogador) |>
    dplyr::mutate(
      prop_n = base::round((cont_n / base::sum(cont_n) * 100), 2) # Proporção em %
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(lambda) |>
    dplyr::mutate(
      type = base::factor(type, levels = c("EWA", "RL", "BL")) # Ordenação correta de type
    )
}

# =================== UI ===================
# URLs (ajuste para os seus perfis)
github_url   <- "https://github.com/drewmelo"
linkedin_url <- "https://www.linkedin.com/in/seu-perfil/"

ui <- page_fluid(
  # Overlay simples acionado por body.shiny-busy (Shiny liga/desliga pra você)
  tags$head(tags$style(HTML(" 
    /* === Spinner do shinybusy sem “caixinha” + fundo esbranquiçado === */
    /* tira a caixa do modal do Shiny usada pelo shinybusy */
    #shiny-modal .modal-content{ background: transparent !important; border: 0 !important; box-shadow: none !important; }
    #shiny-modal .modal-dialog{ margin: 0; max-width: none; }
    #shiny-modal .modal-body{ display:flex; flex-direction:column; align-items:center; gap:10px; padding: 0 !important; }
    /* garante que qualquer wrapper interno não puxe fundo */
    #shiny-modal .modal-body > *{ background: transparent !important; border: 0 !important; box-shadow: none !important; }
    /* texto abaixo do spinner */
    #shiny-modal .shinybusy-text{ font: 600 .95rem/1.2 system-ui, -apple-system, Segoe UI, Roboto, sans-serif; color:#334155; text-shadow: 0 1px 0 rgba(255,255,255,.25); }
    /* backdrop do modal: branco translúcido com leve blur */
    .modal-backdrop.show{ background-color: rgba(255,255,255,.78) !important; backdrop-filter: blur(2px) saturate(1.05); }
    /* (opcional) spinner um tiquinho maior */
    #shiny-modal .spinner-border, #shiny-modal .spinner-grow{ width:2.25rem; height:2.25rem; }
  "))),

  # Pings só quando a aba está ativa
  tags$script(HTML(" 
    let keepAliveTimer;
    function startKeepAlive() {
      if (!keepAliveTimer) {
        keepAliveTimer = setInterval(function() {
          Shiny.onInputChange('keep_alive', Date.now());
        }, 60000);
      }
    }
    function stopKeepAlive() {
      if (keepAliveTimer) {
        clearInterval(keepAliveTimer);
        keepAliveTimer = null;
      }
    }
    document.addEventListener('visibilitychange', function() {
      if (document.visibilityState === 'visible') {
        startKeepAlive();
      } else {
        stopKeepAlive();
      }
    });
    if (document.visibilityState === 'visible') {
      startKeepAlive();
    }
  ")),

  tags$script(HTML(" 
    // quando o modal do shinybusy abrir/fechar, marca/desmarca o backdrop
    $(document).on('shown.bs.modal', '.shinybusy-modal', function(){
      $('.modal-backdrop').addClass('busy-backdrop');
    });
    $(document).on('hidden.bs.modal', '.shinybusy-modal', function(){
      $('.modal-backdrop').removeClass('busy-backdrop');
    });
  ")),

  # O elemento do overlay (fica no topo da página)
  div(class = "ewa-loader"),

  theme = bs_theme(
    bootswatch = "cosmo",
    version = 5,
    base_font = font_google("Poppins"),
    heading_font = font_google("Montserrat")
  ),

  title = "EWA Market Dynamics — Shiny",

  # ===== CSS =====
  tags$style(HTML(" 
    /* --- HEADER GLOBAL (limpo, sem cor) --- */
    .app-header{ background: transparent !important; color: #475569; padding: 8px 0 0; margin: 0 0 8px 0; box-shadow: none !important; border: 0; }
    .app-header .brand{ display:flex; align-items:center; gap:8px; }
    .app-header .brand-title{ font-weight:700; font-size:1.125rem; color:#475569; }
    /* Esconde o selo/pílula “EWA” */
    .app-header .brand-badge{ display:none !important; }
    /* Some com o botão hamburguer personalizado (3 tracinhos) */
    .app-header .sidebar-toggle, .app-header .sidebar-toggle::before, .app-header .sidebar-toggle::after, .app-header .sidebar-toggle .hamb{ display:none !important; }
    /* Header em duas colunas: título à esquerda, ícones à direita */
    .app-header{ display:flex; justify-content:space-between; align-items:center; }
    /* Contêiner dos ícones */
    .app-header .social{ display:flex; align-items:center; gap:10px; }
    /* Botões dos ícones – cinzas, com leve “pill” */
    .app-header .social a{ display:grid; place-items:center; width:36px; height:36px; border-radius:9999px; text-decoration:none; color:#94a3b8; background:#ffffff; border:1px solid #e6edf4; box-shadow:0 2px 8px rgba(16,36,56,.06); transition:all .15s ease-in-out; }
    .app-header .social a:hover{ color:#475569; background:#f8fafc; border-color:#dbe6f1; box-shadow:0 4px 12px rgba(16,36,56,.10); }
    .app-header .social svg{ width:20px; height:20px; }

    /* --- Cards suaves --- */
    .card, .bslib-card{ border:0; border-radius:16px !important; overflow:hidden; box-shadow:0 6px 18px rgba(16,36,56,.06); }
    .card .card-header, .bslib-card .card-header{ background:#1f3b57; color:#fff; border:0; padding:10px 16px; text-transform:uppercase; font-size:.85rem; letter-spacing:.02em; font-weight:700; }
    .card .card-body, .bslib-card .card-body{ padding:16px 18px; }

    /* --- KPI duplas --- */
    .kpi2 .box{ background:#fff; border:1px solid #e9eef3; border-radius:12px; padding:10px; text-align:center; box-shadow:0 4px 12px rgba(16,36,56,.05); }

    /* Esconde qualquer toggle padrão que venha DENTRO do sidebar */
    .bslib-sidebar [data-bslib-sidebar-toggle], .bslib-sidebar .sidebar-toggle, .bslib-sidebar .collapse-toggle, .sidebar .collapse-toggle{ display:none !important; }

    /* --- KPI: header branco, sem barra colorida --- */
    .kpi2 .card-header{ background: #fff !important; color: #334155 !important; border: 0 !important; text-transform: none !important; font-weight: 700 !important; font-size: 1rem !important; padding: 0 0 8px 0 !important; box-shadow: none !important; }
    .kpi2-group{ background:#fff; padding:6px 4px 0; border:0; border-radius:0; box-shadow:none; }
    .kpi2-title{ margin:0 0 8px 0; font-weight:800; color:#334155; }
    .kpi2-group + .kpi2-group{ margin-top:8px; padding-top:12px; border-top:1px solid #eef2f7; }
    .kpi2 .value{ font-size: 2rem; font-weight: 500; line-height: 1; color: #6b7280; }
    .kpi2 .label{ color:#7a8896; }
    .kpi2 .win{ color:#0B86CA; font-weight: 800; }

    /* --- Abas (navset_pill) --- */
    .nav.nav-pills{ gap: 8px; margin-bottom: 10px; flex-wrap: wrap; }
    .nav-pills .nav-link{ border-radius: 9999px; padding: 8px 14px; background: #f6f9fc; color: #334155; border: 1px solid #e6edf4; box-shadow: 0 2px 8px rgba(16,36,56,.06); transition: all .15s ease-in-out; }
    .nav-pills .nav-link:hover{ background: #eef4fb; border-color: #dbe6f1; }
    .nav-pills .nav-link.active{ background: #2f86b7; color: #fff; border-color: #2f86b7; box-shadow: 0 6px 14px rgba(47,134,183,.25); }
    .tab-content{ margin-top: 12px; }
    .nav-pills .nav-link:focus{ outline: none; box-shadow: 0 0 0 3px rgba(47,134,183,.25); border-color: #2f86b7; }

    /* Botões padrão */
    .btn{ border-radius: 9999px !important; padding: 10px 16px; font-weight: 600; border-width: 1px; }
    .btn-primary{ background: #2f86b7; border-color: #2f86b7; box-shadow: 0 6px 14px rgba(47,134,183,.25); }
    .btn-primary:hover{ background: #2978a5; border-color: #2978a5; }
    .btn:focus{ outline: none; box-shadow: 0 0 0 3px rgba(47,134,183,.25); }
    #rerun{ margin-bottom: 12px; }

    /* ===== Pills — inativas só texto + mousedown suave (fade) ===== */
    :root{ --ewa-primary: #0B86CA; --ewa-text: #334155; --ewa-text-rgb: 51,65,85; }
    .model-tabs > .nav, .model-tabs .nav.nav-pills{ justify-content:center; gap:14px; flex-wrap:wrap; margin:6px 0 14px; }
    .model-tabs .nav-pills .nav-link{ position: relative; border: 0 !important; background: transparent !important; box-shadow: none !important; color: var(--ewa-text); border-radius: 9999px; padding: 8px 14px; font-weight: 600; transition: color .12s ease; --pill-press-grad: linear-gradient(180deg, rgba(var(--ewa-text-rgb), .08) 0%, rgba(var(--ewa-text-rgb), .14) 100% ); }
    .model-tabs .nav-pills .nav-link::before{ content:''; position:absolute; inset:0; border-radius: inherit; background: var(--pill-press-grad); opacity: 0; transition: opacity .12s ease; pointer-events: none; }
    .model-tabs .nav-pills .nav-link:hover{ background: #eef2f6 !important; color: var(--ewa-text) !important; border: 0 !important; }
    .model-tabs .nav-pills .nav-link:active::before, .model-tabs .nav-pills .nav-link:hover:active::before{ opacity: 1; }
    .model-tabs .nav-pills .nav-link.active{ border: 0 !important; background: rgba(11,134,202,.18) !important; color: var(--ewa-primary) !important; box-shadow: none !important; --pill-press-grad: linear-gradient(180deg, rgba(11,134,202,.22) 0%, rgba(11,134,202,.30) 100% ); }
    .model-tabs .nav-pills .nav-link.active:hover{ background: rgba(11,134,202,.18) !important; color: var(--ewa-primary) !important; }
    .model-tabs .nav-pills .nav-link.active:active::before{ opacity: 1; }
    .model-tabs .nav-pills .nav-link:focus-visible{ outline: none; box-shadow: 0 0 0 3px rgba(11,134,202,.28); }

    /* === Sidebar realmente branco (corrige bg-body/secondary/tertiary) === */
    #main_sidebar.sidebar{ --sb-bg: #ffffff; background-color: var(--sb-bg) !important; border: 1px solid #eef2f7 !important; border-right: 0 !important; border-radius: 16px !important; box-shadow: 0 8px 22px rgba(16,36,56,.06) !important; }
    #main_sidebar.sidebar, #main_sidebar.sidebar .bg-body, #main_sidebar.sidebar .bg-light, #main_sidebar.sidebar .bg-body-secondary, #main_sidebar.sidebar .bg-body-tertiary, #main_sidebar.sidebar .card, #main_sidebar.sidebar .bslib-card{ background-color: var(--sb-bg) !important; }
    #main_sidebar.sidebar > .sidebar-content{ padding: 16px; }
    #main_sidebar.sidebar.sb-dark{ --sb-bg: #0f172a; color: #e5e7eb; }
    #main_sidebar.sidebar.sb-dark .form-label, #main_sidebar.sidebar.sb-dark .help-block{ color:#cbd5e1; }
    #main_sidebar.sidebar.sb-dark .form-control, #main_sidebar.sidebar.sb-dark .form-select{ background:#111827 !important; color:#e5e7eb !important; border-color:#1f2937 !important; }

    [data-bslib-sidebar-id='main_sidebar'] .collapse-toggle{ display:none !重要; }
    .bslib-sidebar-layout, .bslib-sidebar-layout .main, .bslib-sidebar-layout .sidebar{ border:0 !important; box-shadow:none !important; }
    .bslib-sidebar-layout .main.border-start, .bslib-sidebar-layout .sidebar.border-end{ border-left:0 !important; border-right:0 !important; }
    [data-bslib-sidebar-id='main_sidebar'] .sidebar > .sidebar-content{ padding:16px; }

    .stats-text p{ margin: 0 0 10px 0; color:#475569; font-size: .975rem; line-height: 1.45; }

    /* ===== Slider moderno (ionRangeSlider do Shiny) ===== */
    .irs--shiny { margin-top: 4px; }
    .irs--shiny .irs-line { top: 28px; height: 6px; border-radius: 9999px; background: #e9edf3; }
    .irs--shiny .irs-bar { top: 28px; height: 6px; border-radius: 9999px; background: #2f86b7; }
    .irs--shiny .irs-handle { top: 22px; width: 18px; height: 18px; border-radius: 50%; background: #2f86b7; border: 3px solid #fff; box-shadow: 0 0 0 3px rgba(47,134,183,.25); }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover { box-shadow: 0 0 0 4px rgba(47,134,183,.3); }
    .irs--shiny .irs-single { top: -8px; background: transparent; color: #94a3b8; border: 0; box-shadow: none; padding: 0; font-weight: 600; }
    .irs--shiny .irs-single:before { display: none; }
    .irs--shiny .irs-min, .irs--shiny .irs-max { top: 40px; background: transparent; color: #94a3b8; border: 0; box-shadow: none; padding: 0; font-weight: 500; }
    .irs--shiny .irs-grid { display: none !important; }

    /* ===== Inputs “pill” modernos (numericInput, selectInput, etc.) ===== */
    .form-control, .form-select { border-radius: 12px !important; border: 1px solid #e6edf4 !important; background: #fff !important; box-shadow: 0 2px 8px rgba(16,36,56,.05) !important; padding: 10px 12px !important; }
    .form-control:focus, .form-select:focus { border-color: #2f86b7 !important; box-shadow: 0 0 0 3px rgba(47,134,183,.20) !important; }
    .form-control::placeholder { color: #9aa4b2; }
    .form-label { margin-bottom: 4px; color:#475569; }

    /* ====== Alinhamento/legibilidade (checkbox & radio) ====== */
    .checkbox label, .radio label{ display:flex; align-items:center; gap:.5rem; margin:.25rem 0; }
    .checkbox label span, .radio label span{ color:#475569; font-weight:500; }

    /* TOGGLE para CHECKBOX (switch) */
    /* alvo: <input class='shiny-input-checkbox shiny-bound-input' type='checkbox'> */
    .checkbox label{ display:flex; align-items:center; gap:.5rem; cursor:pointer; user-select:none; margin:.25rem 0; }
    .checkbox label span{ color:#475569; font-weight:500; }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']{
      -webkit-appearance:none; appearance:none; background-image:none !important;
      width:44px; height:26px; border-radius:9999px; background:#e9edf3; border:1px solid #dbe6f1; position:relative; cursor:pointer; outline:0; transition:background .18s, box-shadow .18s, border-color .18s; box-shadow: inset 0 1px 0 rgba(16,36,56,.03); margin:0; flex:0 0 auto;
    }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']::before{
      content:''; position:absolute; top:3px; left:3px; width:18px; height:18px; border-radius:50%; background:#fff; box-shadow:0 2px 6px rgba(16,36,56,.20); transform:translateX(0); transition: transform .18s ease;
    }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']:checked{ background:#2f86b7; border-color:#2f86b7; box-shadow:0 0 0 3px rgba(47,134,183,.20); }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']:checked::before{ transform: translateX(20px); }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']:focus{ box-shadow:0 0 0 3px rgba(47,134,183,.22); }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']:hover{ border-color:#cfe1ef; }
    input.shiny-input-checkbox.shiny-bound-input[type='checkbox']:disabled{ opacity:.55; cursor:not-allowed; }

    /* RADIO minimalista (anel + miolo) */
    .radio label{ display:flex; align-items:center; gap:.5rem; cursor:pointer; user-select:none; margin:.25rem 0; }
    .radio label span{ color:#475569; font-weight:500; }
    .radio input[type='radio']{ -webkit-appearance:none; appearance:none; background-image:none !important; width:18px; height:18px; border-radius:50%; border:1.5px solid #cbd5e1; background:#fff; position:relative; cursor:pointer; outline:0; box-shadow:0 1px 3px rgba(16,36,56,.08); transition:border-color .15s, box-shadow .15s; margin:0; flex:0 0 auto; }
    .radio input[type='radio']:hover{ border-color:#a7b7c7; }
    .radio input[type='radio']:focus{ box-shadow:0 0 0 3px rgba(47,134,183,.20); }
    .radio input[type='radio']::after{ content:''; position:absolute; top:50%; left:50%; width:10px; height:10px; border-radius:50%; background:#2f86b7; transform:translate(-50%,-50%) scale(0); transition: transform .15s ease; }
    .radio input[type='radio']:checked{ border-color:#2f86b7; box-shadow:0 0 0 3px rgba(47,134,183,.20); }
    .radio input[type='radio']:checked::after{ transform:translate(-50%,-50%) scale(1); }
    .radio input[type='radio']:disabled{ opacity:.55; cursor:not-allowed; }
    #dl_kind .radio{ margin-bottom:.35rem; }

    /* Botão flutuante: voltar ao topo (inicialmente invisível) */
    .back-to-top{ position: fixed; right: 16px; top: 16px; width: 44px; height: 44px; border-radius: 9999px; display: grid; place-items: center; background: #ffffff; color: #94a3b8; border: 1px solid #e6edf4; box-shadow: 0 6px 18px rgba(16,36,56,.10); cursor: pointer; z-index: 9999; opacity: 0; transform: translateY(-6px); pointer-events: none; transition: opacity .18s ease, transform .18s ease; }
    .back-to-top:hover{ color:#475569; background:#f8fafc; border-color:#dbe6f1; }
    .back-to-top.show{ opacity: 1; transform: translateY(0); pointer-events: auto; }
    .back-to-top svg{ width: 20px; height: 20px; }
    @media (prefers-reduced-motion: reduce){ .back-to-top{ transition: none; } }

    /* ===== Selectize (selectInput padrão do Shiny) ===== */
    :root{ --ewa-primary:#2f86b7; --ewa-caret-light:rgb(123, 125, 126); --ewa-text-light: #5f7283; }
    .selectize-control.single .selectize-input{ position: relative; border-radius: 12px !important; border: 1px solid #e6edf4 !important; background: #fff !important; box-shadow: 0 2px 8px rgba(16,36,56,.05) !important; padding: 10px 36px 10px 12px; font-weight: 600; color: var(--ewa-text-light) !important; }
    .selectize-control.single .selectize-input.focus{ border-color: var(--ewa-primary) !important; box-shadow: 0 0 0 3px rgba(47,134,183,.20) !important; }
    .selectize-control.single .selectize-input:after{ content:''; position:absolute; right:12px; top:50%; width:0; height:0; margin-top:-3px; border-left:6px solid transparent; border-right:6px solid transparent; border-top:6px solid var(--ewa-primary); pointer-events:none; }
    .selectize-control.single .selectize-input.dropdown-active:after, .selectize-control.single .selectize-input.input-active:after{ transform: rotate(180deg); border-top-color: var(--ewa-caret-light) !important; }
    .selectize-dropdown{ border:1px solid #e6edf4 !important; box-shadow:0 8px 22px rgba(16,36,56,.10) !important; border-radius:12px !important; overflow:hidden; }
    .selectize-dropdown .option{ padding:8px 12px; }
    .selectize-dropdown .option:hover{ background: rgba(47,134,183,.12); }
    .selectize-dropdown .option.active{ background:#2f86b7; color:#fff; }
    .selectize-control.single .selectize-input .item{ color: var(--ewa-text-light) !important;}
    .selectize-control.single .selectize-input .placeholder{ color:#94a3b8; }
    #n_samples, #n_periods, #seed_val{ padding-right: 1rem; }

    /* ===== Evita “dois selecionados” no Selectize ===== */
    #main_sidebar .selectize-dropdown .option{ position: relative; padding: 8px 12px 8px 32px; }
    #main_sidebar .selectize-dropdown .option.active{ background: rgba(117, 169, 199, 0.1); color: #334155; }
    #main_sidebar .selectize-dropdown .option.selected{ background: #2f86b7; color:#fff; font-weight: 700; }
    #main_sidebar .selectize-dropdown .option.selected:not(.active)::before{ content:''; position:absolute; left:10px; top:50%; width:14px; height:14px; margin-top:-7px; background: url('data:image/svg+xml,%3Csvg xmlns=\'http://www.w3.org/2000/svg\' viewBox=\'0 0 20 20\'%3E%3Cpath fill=\'%232f86b7\' d=\'M7.7 13.3L3.9 9.6l1.4-1.4 2.4 2.4 6-6 1.4 1.4-7.4 7.3z\'/%3E%3C/svg%3E') center/14px 14px no-repeat; opacity:.95; }
    #main_sidebar .selectize-dropdown .option:not(.active):hover{ background: rgba(47,134,183,.12); }

    #download_csv.btn svg, #download_sim.btn svg{ width:18px; height:18px; margin-right:6px; color: currentColor; }

    /* === Botão 'Metadados' (download_meta) no padrão do app === */
    #download_meta.btn { color: var(--ewa-primary) !important; border-color: var(--ewa-primary) !important; background: transparent !important; box-shadow: 0 2px 8px rgba(47,134,183,.12); transition: background .15s ease, color .15s ease, border-color .15s ease, box-shadow .15s ease, transform .04s ease; }
    #download_meta.btn:hover{ background: rgba(47,134,183,.08) !important; color: var(--ewa-primary) !important; border-color: var(--ewa-primary) !important; box-shadow: 0 6px 14px rgba(47,134,183,.25); }
    #download_meta.btn:focus{ outline: none; box-shadow: 0 0 0 3px rgba(47,134,183,.25) !important; }
    #download_meta.btn:active{ transform: translateY(1px); background: rgba(47,134,183,.18) !important; border-color: var(--ewa-primary) !important; }
    #download_meta.btn svg{ width:18px; height:18px; margin-right:6px; color: currentColor; }

    /* Velocidade controlável por variáveis */
    :root{ --dbg-dur: .55s; --dbg-dur-op: .40s; --dbg-ease: cubic-bezier(.22,.61,.36,1); }
    .dbg-slide{ overflow: hidden; max-height: 0; opacity: 0; transform: translateY(-4px); transition: max-height var(--dbg-dur) var(--dbg-ease), opacity var(--dbg-dur-op) linear, transform var(--dbg-dur) var(--dbg-ease); margin-top: 0; transition-duration: .35s, .28s, .35s; }
    .dbg-slide.open{ max-height: 520px; opacity: 1; transform: translateY(0); margin-top: 8px; transition-duration: .60s, .40s, .60s; }
    @media (prefers-reduced-motion: reduce){ .dbg-slide{ transition: none; } }

    /* empilha em 1 coluna no mobile e ocupa 100% da largura */
    @media (max-width: 768px){
      .stack-mobile .bslib-grid{ grid-template-columns: 1fr !important; gap: 10px !important; }
      .stack-mobile .bslib-grid > *{ min-width: 0; }
    }

    /* Altura e largura menores no celular */
    @media (max-width: 768px){
      #ewa_bsg, #ewa_meg, #rl_bsg, #rl_meg, #bl_bsg, #bl_meg { height: 300px !important; width: 110% !important; font-size: .6rem !important; }
      #stats_hist { height: 220px !important; width: 110% !important; }
    }
    @media (max-width: 420px){
      #ewa_bsg, #ewa_meg, #rl_bsg, #rl_meg, #bl_bsg, #bl_meg { height: 290px !important; width: 110% !important; font-size: .6rem !important; }
      #stats_hist { height: 200px !important; width: 110% !important; }
    }

    .bslib-card .card-body{ padding: 12px; }
    .bslib-card .card-header{ padding: 8px 12px; font-size: .9rem; }

    @media (max-width: 768px){ .model-tabs > .nav, .model-tabs .nav.nav-pills{ gap: 10px; } }
  ")),

  tags$script(HTML(" 
    document.addEventListener('click', function (e) {
      const btn = e.target.closest('.app-header .sidebar-toggle');
      if (!btn) return;
      const real = document.querySelector(\".collapse-toggle[data-bslib-sidebar-toggle='main_sidebar']\");
      if (real) real.click();
    });
    $(document).on('shiny:connected', function(){
      var btn = document.querySelector('[data-bslib-sidebar-toggle=\"main_sidebar\"]');
      if (btn) btn.click();
    });
    (function(){
      function enhanceSlider(id){
        var $wrap = $('#'+id).find('.irs');
        if(!$wrap.length){ setTimeout(function(){ enhanceSlider(id); }, 50); return; }
        var $min = $wrap.find('.irs-min'), $max = $wrap.find('.irs-max'), $val = $wrap.find('.irs-single');
        function comma(s){ return (s||'').toString().replace('.', ','); }
        function paint(){ $min.text( comma($min.text()) ); $max.text( comma($max.text()) ); $val.text( comma($val.text()) ); }
        paint();
        if($val.length){ new MutationObserver(paint).observe($val[0], {childList:true, characterData:true, subtree:true}); }
      }
      $(document).on('shiny:connected', function(){ enhanceSlider('lambda'); });
      $(document).on('shiny:inputchanged', function(e){ if(e.name === 'lambda') enhanceSlider('lambda'); });
    })();
  ")),

  # Botão "voltar ao topo" (fica fixo via CSS)
  tags$button(
    id = "backTop", type = "button", class = "back-to-top",
    title = "Voltar ao topo", `aria-label` = "Voltar ao topo",
    bsicons::bs_icon("chevron-up")
  ),

  tags$script(HTML(" 
    // dispara uma simulação assim que o Shiny conectar
    $(document).on('shiny:connected', function(){
      setTimeout(function(){ $('#rerun').trigger('click'); }, 0);
    });
    (function(){
      var btn = document.getElementById('backTop');
      if(!btn) return;
      var OFFSET = 450;
      function toggle(){
        var doc = document.documentElement;
        var nearBottom = (doc.scrollTop + window.innerHeight) >= (doc.scrollHeight - OFFSET);
        if(nearBottom){ btn.classList.add('show'); } else{ btn.classList.remove('show'); }
      }
      btn.addEventListener('click', function(){ window.scrollTo({ top: 0, behavior: 'smooth' }); });
      window.addEventListener('scroll', toggle, { passive: true });
      window.addEventListener('resize', toggle, { passive: true });
      document.addEventListener('readystatechange', toggle);
      toggle();
    })();
  ")),

  tags$script(HTML(" 
    Shiny.addCustomMessageHandler('dbg-toggle', function(open){
      document.querySelectorAll('.dbg-slide').forEach(function(el){ el.classList.toggle('open', !!open); });
    });
    $(document).on('shown.bs.tab', function(){
      Shiny.setInputValue('dbg_reapply', Date.now(), {priority: 'event'});
    });
  ")),

  # ===== HEADER GLOBAL (fora do layout_sidebar/navset) =====
  div(class = "app-header",
      div(class = "brand",
          span(class = "brand-title", "EWA Market Dynamics Dashboard")
      ),
      div(class = "social",
          a(href = github_url,  target = "_blank", rel = "noopener", title = "GitHub",   `aria-label` = "GitHub",   bsicons::bs_icon("github")),
          a(href = linkedin_url, target = "_blank", rel = "noopener", title = "LinkedIn", `aria-label` = "LinkedIn", bsicons::bs_icon("linkedin"))
      )
  ),

  # ===== CONTEÚDO COM SIDEBAR =====
  layout_sidebar(
    sidebar = sidebar(
      id = "main_sidebar",
      open = F, # <- começa fechado
      width = 320,

      h4("Parâmetros"),
      sliderInput("lambda", HTML("λ (lambda)"), min = 0.1, max = 1.0, value = 0.5, step = 0.1, ticks = FALSE),
      numericInput("n_samples", "Amostras (n)", value = 10, min = 1, step = 1),
      numericInput("n_periods", "Períodos", value = 12, min = 1, step = 1),
      actionButton("rerun", "Simular", class = "btn btn-primary"),
      checkboxInput("fix_seed", "Fixar semente", value = FALSE),
      numericInput("seed_val", "Semente", value = 1234, min = 1, step = 1),
      checkboxInput("debug", "Mostrar debug (head)", FALSE),

      hr(),
      h5("Baixar resultados"),
      selectInput("dl_model", "Modelo", choices = c("EWA","RL","BL","Todos"), selected = "EWA"),
      selectInput("dl_game", "Jogo", choices = c("BSG","MEG","Todos"), selected = "BSG"),
      radioButtons("dl_kind", "Tipo",
                   choices = c("Agregado (proporções)" = "prop",
                               "Micro (linhas)" = "long")),
      downloadButton(
        "download_csv",
        icon = NULL,
        tagList(bsicons::bs_icon("download"), "Baixar CSV"),
        class = "btn btn-primary"
      ),
      helpText("A simulação só recalcula quando você clicar em Simular."),

      hr(),
      h5("Simulação dos modelos"),
      downloadButton(
        "download_sim",
        icon = NULL,
        tagList(bsicons::bs_icon("database"), "Baixar Simulação"),
        class = "btn btn-primary"
      ),
      helpText("Simulação completa (BSG+MEG e EWA/RL/BL) no formato 'long'. Inclui λ, amostras, períodos e semente (se fixada)."),

      # Novo: Metadados (Excel)
      hr(),
      h5("Dicionário de dados"),
      downloadButton(
        "download_meta",
        icon = NULL,
        tagList(bsicons::bs_icon("files")),
        "Metadados",
        class = "btn btn-outline-primary"
      ),
      helpText("Inclui descrições de nomes, tipos, descrições, exemplos e estatísticas por coluna.")
    ),

    # ---- abas dos modelos (sem 'header =' aqui!) ----
    div(class = "model-tabs",
      navset_pill(
        id = "modelo", # <— ID da aba ativa

        nav_panel("Experience-Weighted Attraction (EWA)", value = "EWA",
          layout_columns(class = "stack-mobile", col_widths = c(6,6),
            card(
              card_header("Buyer–Seller Game (BSG)"),
              plotlyOutput("ewa_bsg", height = "360px"),
              div(class = "dbg-slide", reactable::reactableOutput("dbg_ewa_bsg"))
            ),
            uiOutput("kpi_ewa_bsg_card")
          ),
          layout_columns(class = "stack-mobile", col_widths = c(6,6),
            card(
              card_header("Market Entry Game (MEG)"),
              plotlyOutput("ewa_meg", height = "360px"),
              div(class = "dbg-slide", reactable::reactableOutput("dbg_ewa_meg"))
            ),
            uiOutput("kpi_ewa_meg_card")
          ),
          card(
            card_header("Medidas-resumo (EWA)"),
            reactable::reactableOutput("mr_ewa_rt")
          )
        ),

        nav_panel("Reinforcement Learning (RL)", value = "RL",
          layout_columns(class = "stack-mobile", col_widths = c(6,6),
            card(
              card_header("Buyer–Seller Game (BSG)"),
              plotlyOutput("rl_bsg", height = "360px"),
              div(class = "dbg-slide", reactable::reactableOutput("dbg_rl_bsg"))
            ),
            uiOutput("kpi_rl_bsg_card")
          ),
          layout_columns(class = "stack-mobile", col_widths = c(6,6),
            card(
              card_header("Market Entry Game (MEG)"),
              plotlyOutput("rl_meg", height = "360px"),
              div(class = "dbg-slide", reactable::reactableOutput("dbg_rl_meg"))
            ),
            uiOutput("kpi_rl_meg_card")
          ),
          card(
            card_header("Medidas-resumo (RL)"),
            reactable::reactableOutput("mr_rl_rt")
          )
        ),

        nav_panel("Belief-based Learning (BL)", value = "BL",
          layout_columns(class = "stack-mobile", col_widths = c(6,6),
            card(
              card_header("Buyer–Seller Game (BSG)"),
              plotlyOutput("bl_bsg", height = "360px"),
              div(class = "dbg-slide", reactable::reactableOutput("dbg_bl_bsg"))
            ),
            uiOutput("kpi_bl_bsg_card")
          ),
          layout_columns(class = "stack-mobile", col_widths = c(6,6),
            card(
              card_header("Market Entry Game (MEG)"),
              plotlyOutput("bl_meg", height = "360px"),
              div(class = "dbg-slide", reactable::reactableOutput("dbg_bl_meg"))
            ),
            uiOutput("kpi_bl_meg_card")
          ),
          card(
            card_header("Medidas-resumo (BL)"),
            reactable::reactableOutput("mr_bl_rt")
          )
        )
      )
    ),

    # --- CARD: Estatísticas (texto à esquerda; direita vazia para futuro gráfico) ---
    card(
      card_header("Estatísticas"),
      layout_columns(class = "stack-mobile", col_widths = c(6,6),
        div(class = "stats-text", uiOutput("stats_text")),
        plotlyOutput("stats_hist", height = "280px") # <- gráfico interativo aqui
      )
    ),

    # (opcional) card de 'Sobre o projeto' depois do layout_sidebar
    card(
      collapsible = TRUE,
      card_header("Sobre o projeto"),
      card_body(HTML('
        <div style="text-align:center; margin-top:8px; margin-bottom:4px;">
          <img src="https://img.shields.io/badge/Maintained%3F-yes-green.svg" alt="maintained" style="height:20px;">
        </div>
        <p style="text-align:center; margin-top:6px;">Trabalho de Conclusão de Curso — <em>data</em></p>
      '))
    )
  )
)

# =================== SERVER ===================
server <- function(input, output, session){

  observeEvent(input$keep_alive, {
    # só consome o input, não precisa fazer nada
  })

  params <- eventReactive(input$rerun, {
    list(
      lambda    = isolate(input$lambda),
      n_samples = isolate(input$n_samples),
      n_periods = isolate(input$n_periods),
      seed      = if (isTRUE(isolate(input$fix_seed)))
                    as.integer(isolate(input$seed_val))
                  else
                    as.integer(sample.int(.Machine$integer.max, 1))
    )
  }, ignoreInit = FALSE)

  # <<< NOVO: dispara UM spinner para o lote inteiro
  sims <- eventReactive(input$rerun, {
    shinybusy::show_modal_spinner(
      spin  = "fading-circle",  # (parecido com o anel do print)
      color = color_main[1],    # azul do seu dashboard
      text  = "Executando simulações…"  # o texto pedido
    )
    on.exit(shinybusy::remove_modal_spinner(), add = TRUE)

    # Mantém as .dbg-slide abertas/fechadas conforme o checkbox "debug"
    observeEvent(input$debug, {
      session$sendCustomMessage("dbg-toggle", isTRUE(input$debug))
    }, ignoreInit = FALSE)
    observeEvent(input$dbg_reapply, {
      session$sendCustomMessage("dbg-toggle", isTRUE(input$debug))
    }, ignoreInit = TRUE)

    p <- params()
    list(
      ewa_bsg = run_sim("BSG","EWA", p$lambda, p$n_samples, p$n_periods, seed = p$seed),
      ewa_meg = run_sim("MEG","EWA", p$lambda, p$n_samples, p$n_periods, seed = p$seed),
      rl_bsg  = run_sim("BSG","RL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed),
      rl_meg  = run_sim("MEG","RL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed),
      bl_bsg  = run_sim("BSG","BL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed),
      bl_meg  = run_sim("MEG","BL",  p$lambda, p$n_samples, p$n_periods, seed = p$seed)
    )
  }, ignoreInit = FALSE)

  ewa_bsg <- reactive({ sims()$ewa_bsg })
  ewa_meg <- reactive({ sims()$ewa_meg })
  rl_bsg  <- reactive({ sims()$rl_bsg  })
  rl_meg  <- reactive({ sims()$rl_meg  })
  bl_bsg  <- reactive({ sims()$bl_bsg  })
  bl_meg  <- reactive({ sims()$bl_meg  })

  pal <- if (exists("color_main")) color_main else NULL

  # Usa sua frequency_probabilities(df) já definida
  # Retorna a média por estratégia (usa sua frequency_probabilities)
  get_media_prob <- function(long_df, model_label, jogador, estrategia) {
    df <- dplyr::filter(long_df, type == model_label)
    kp <- frequency_probabilities(df)
    v  <- kp$media_prob[kp$jogador == jogador & kp$estrategia_escolhida == estrategia]
    if (length(v)) as.numeric(v[[1]]) else NA_real_
  }

  duo_card <- function(title, left_label, left_val, right_label, right_val) {
    left_win  <- isTRUE(left_val  >= right_val)
    right_win <- isTRUE(right_val >  left_val)
    div(class = "kpi2 kpi2-group",
      tags$h5(class = "kpi2-title", title),
      bslib::layout_columns(
        col_widths = c(6,6),
        div(class = "box",
            div(class="label", left_label),
            tags$div(class = paste("value", if (left_win) "win"),
                     scales::percent(left_val, accuracy = 0.1, decimal.mark=",")) # << só o número
        ),
        div(class = "box",
            div(class="label", right_label),
            tags$div(class = paste("value", if (right_win) "win"),
                     scales::percent(right_val, accuracy = 0.1, decimal.mark=",")) # << só o número
        )
      )
    )
  }

  # Constrói as 2 'duplas' por jogo (BSG: Comprador/Vendedor; MEG: Empresa A/B)
  kpi_duo_section <- function(title, long_df, model_label, game_label) {
    if (game_label == "BSG") {
      # Comprador
      c_ace <- get_media_prob(long_df, model_label, "Comprador", "Aceitar")
      c_rej <- get_media_prob(long_df, model_label, "Comprador", "Rejeitar")
      card1 <- duo_card("Estratégias (Comprador)", "Aceitar", c_ace, "Rejeitar", c_rej)
      # Vendedor
      v_alto  <- get_media_prob(long_df, model_label, "Vendedor", "Preço Alto")
      v_baixo <- get_media_prob(long_df, model_label, "Vendedor", "Preço Baixo")
      card2 <- duo_card("Estratégias (Vendedor)", "Preço Alto", v_alto, "Preço Baixo", v_baixo)
    } else {
      # MEG
      # Empresa A
      a_ent <- get_media_prob(long_df, model_label, "Empresa A", "Entrar")
      a_nao <- get_media_prob(long_df, model_label, "Empresa A", "Não Entrar")
      card1 <- duo_card("Estratégias (Empresa A)", "Entrar", a_ent, "Não Entrar", a_nao)
      # Empresa B
      b_ent <- get_media_prob(long_df, model_label, "Empresa B", "Entrar")
      b_nao <- get_media_prob(long_df, model_label, "Empresa B", "Não Entrar")
      card2 <- duo_card("Estratégias (Empresa B)", "Entrar", b_ent, "Não Entrar", b_nao)
    }
    bslib::card(
      bslib::card_header(title),
      card1,
      card2
    )
  }

  # ----- Helpers de formatação para PT-BR -----
  fmt_pct <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 0.1, decimal.mark = ",")
  fmt_num <- function(x, digits = 1) {
    if (is.na(x)) "—" else format(round(x, digits), big.mark=".", decimal.mark=",", trim=TRUE)
  }

  # ----- Texto do card "Estatísticas" (adiciona parágrafo do histograma) -----
  output$stats_text <- renderUI({
    req(input$modelo, params(), ewa_bsg(), rl_bsg(), bl_bsg(), ewa_meg(), rl_meg(), bl_meg())
    mdl <- input$modelo # "EWA" | "RL" | "BL"

    # Escolhe as simulações do modelo ativo
    rx_bsg <- switch(mdl, EWA = ewa_bsg(), RL = rl_bsg(), BL = bl_bsg())
    rx_meg <- switch(mdl, EWA = ewa_meg(), RL = rl_meg(), BL = bl_meg())

    # Lambda atual (formato PT-BR)
    lambda_txt <- gsub("\\.", ",", format(params()$lambda, trim = TRUE))

    # ---------- BSG: estratégia mais provável por jogador ----------
    c_ace <- get_media_prob(rx_bsg$long, mdl, "Comprador", "Aceitar")
    c_rej <- get_media_prob(rx_bsg$long, mdl, "Comprador", "Rejeitar")
    v_alt <- get_media_prob(rx_bsg$long, mdl, "Vendedor", "Preço Alto")
    v_bxo <- get_media_prob(rx_bsg$long, mdl, "Vendedor", "Preço Baixo")
    comp_lab <- c("Aceitar","Rejeitar")[which.max(c(c_ace, c_rej))]
    comp_val <- max(c(c_ace, c_rej), na.rm = TRUE)
    vend_lab <- c("Preço Alto","Preço Baixo")[which.max(c(v_alt, v_bxo))]
    vend_val <- max(c(v_alt, v_bxo), na.rm = TRUE)

    # Medidas-resumo (atração) — BSG
    long_bsg <- dplyr::filter(rx_bsg$long, type == mdl)
    mr_bsg   <- summary_statistics(long_bsg, "bsg")
    bsg_m  <- mr_bsg$media_bsg[1]
    bsg_dp <- mr_bsg$dp_bsg[1]
    bsg_min <- mr_bsg$min_bsg[1]
    bsg_max <- mr_bsg$max_bsg[1]
    bsg_iqr  <- mr_bsg$iqr_bsg[1]

    # ---------- MEG: estratégia mais provável por empresa ----------
    a_ent <- get_media_prob(rx_meg$long, mdl, "Empresa A", "Entrar")
    a_nao <- get_media_prob(rx_meg$long, mdl, "Empresa A", "Não Entrar")
    b_ent <- get_media_prob(rx_meg$long, mdl, "Empresa B", "Entrar")
    b_nao <- get_media_prob(rx_meg$long, mdl, "Empresa B", "Não Entrar")
    a_lab <- c("Entrar","Não Entrar")[which.max(c(a_ent, a_nao))]
    a_val <- max(c(a_ent, a_nao), na.rm = TRUE)
    b_lab <- c("Entrar","Não Entrar")[which.max(c(b_ent, b_nao))]
    b_val <- max(c(b_ent, b_nao), na.rm = TRUE)

    # Medidas-resumo (atração) — MEG
    long_meg <- dplyr::filter(rx_meg$long, type == mdl)
    mr_meg   <- summary_statistics(long_meg, "meg")
    meg_m  <- mr_meg$media_meg[1]
    meg_dp <- mr_meg$dp_meg[1]
    meg_min <- mr_meg$min_meg[1]
    meg_max <- mr_meg$max_meg[1]
    meg_iqr  <- mr_meg$iqr_meg[1]

    # ---------- Parágrafos (BSG/MEG) ----------
    p_bsg <- htmltools::tags$p(
      htmltools::HTML(glue::glue(
        "No <strong>BSG</strong> (λ = {lambda_txt}), o <em>Vendedor</em> tende a <strong>{vend_lab}</strong> ({fmt_pct(vend_val)}), ",
        "enquanto o <em>Comprador</em> tende a <strong>{comp_lab}</strong> ({fmt_pct(comp_val)}). ",
        "Nas medidas de atração, observou-se média de <strong>{fmt_num(bsg_m)}</strong> ",
        "(dp {fmt_num(bsg_dp)}), mínimo {fmt_num(bsg_min)} e máximo {fmt_num(bsg_max)}, ",
        "com intervalo interquartil de {fmt_num(bsg_iqr, 2)}."
      ))
    )
    p_meg <- htmltools::tags$p(
      htmltools::HTML(glue::glue(
        "No <strong>MEG</strong> (λ = {lambda_txt}), a <em>Empresa A</em> tende a <strong>{a_lab}</strong> ({fmt_pct(a_val)}), ",
        "e a <em>Empresa B</em> tende a <strong>{b_lab}</strong> ({fmt_pct(b_val)}). ",
        "Para a atração, a média foi <strong>{fmt_num(meg_m)}</strong> ",
        "(dp {fmt_num(meg_dp)}), mínimo {fmt_num(meg_min)} e máximo {fmt_num(meg_max)}, ",
        "com intervalo interquartil de {fmt_num(meg_iqr, 2)}."
      ))
    )

    # ---------- Parágrafo do Histograma em densidade (BSG vs MEG) ----------
    df_hist <- dplyr::bind_rows(
      dplyr::filter(rx_bsg$long, type == mdl),
      dplyr::filter(rx_meg$long, type == mdl)
    ) |>
      dplyr::filter(estrategia_escolhida == estrategias) |>
      tidyr::drop_na(probabilidade)

    meds <- df_hist |>
      dplyr::group_by(jogo) |>
      dplyr::summarise(
        mediana = stats::median(probabilidade, na.rm = TRUE),
        sd      = stats::sd(probabilidade,      na.rm = TRUE),
        .groups = "drop"
      )

    getv <- function(df, jg, col){
      v <- df[[col]][df$jogo == jg]
      if (length(v)) as.numeric(v[[1]]) else NA_real_
    }
    med_bsg <- getv(meds, "BSG", "mediana")
    med_meg <- getv(meds, "MEG", "mediana")
    sd_bsg  <- getv(meds, "BSG", "sd")
    sd_meg  <- getv(meds, "MEG", "sd")

    conc_txt <- if (is.na(sd_bsg) || is.na(sd_meg)) "" else {
      if (sd_bsg < sd_meg) "maior concentração no BSG"
      else if (sd_meg < sd_bsg) "maior concentração no MEG"
      else "concentração semelhante entre os jogos"
    }

    p_hist <- htmltools::tags$p(
      htmltools::HTML(glue::glue(
        "No <strong>histograma em densidade</strong> (λ = {lambda_txt}), ",
        "a mediana do <strong>BSG</strong> é <span style='color:{color_main[1]};'>{fmt_pct(med_bsg)}</span> ",
        "(linha pontilhada <span style='color:{color_main[1]};'>azul</span>) ",
        "e a do <strong>MEG</strong> é <span style='color:{color_main[4]};'>{fmt_pct(med_meg)}</span> ",
        "(linha pontilhada <span style='color:{color_main[4]};'>vermelha</span>). ",
        "{if (nzchar(conc_txt)) paste0('A dispersão relativa indica ', conc_txt, ",
        " ' (dp ', fmt_num(sd_bsg, 2), ' vs ', fmt_num(sd_meg, 2), ').') else ''}"
      ))
    )

    htmltools::tagList(p_bsg, p_meg, p_hist)
  })

  output$stats_hist <- renderPlotly({
    req(input$modelo, params(), ewa_bsg(), rl_bsg(), bl_bsg(), ewa_meg(), rl_meg(), bl_meg())
    mdl <- input$modelo # "EWA" | "RL" | "BL"

    # Simulações do modelo ativo
    rx_bsg <- switch(mdl, EWA = ewa_bsg(), RL = rl_bsg(), BL = bl_bsg())
    rx_meg <- switch(mdl, EWA = ewa_meg(), RL = rl_meg(), BL = bl_meg())

    # Junta os dois jogos e mantém só linhas "acertadas" (a estratégia escolhida)
    df <- dplyr::bind_rows(
      dplyr::filter(rx_bsg$long, type == mdl),
      dplyr::filter(rx_meg$long, type == mdl)
    ) |>
      dplyr::filter(estrategia_escolhida == estrategias) |>
      tidyr::drop_na(probabilidade)
    req(nrow(df) > 0)

    # Medianas por jogo (e por lambda/sim_id, para vlines múltiplas quando houver)
    med_df <- df |>
      dplyr::group_by(jogo, lambda, sim_id) |>
      dplyr::summarise(mediana = stats::median(probabilidade), .groups = "drop")

    # Paleta para 2 jogos (pego 1ª e 4ª cores da sua paleta)
    pal2 <- setNames(color_main[c(1,4)], c("BSG","MEG"))

    # Histograma em densidade, facet por lambda (se houver >1)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = probabilidade)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = after_stat(density), fill = jogo),
        position = "identity",
        alpha = 0.5,
        bins = 30,
        color = "#485B6D",
        linewidth = 0.3
      ) +
      ggplot2::geom_vline(
        data = med_df,
        ggplot2::aes(xintercept = mediana, color = jogo),
        linetype = "dashed"
      ) +
      ggplot2::scale_fill_manual(values = pal2) +
      ggplot2::scale_color_manual(values = pal2) +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_pretty(n = 6),
        labels = scales::label_comma(big.mark = ".", decimal.mark = ","),
        expand = c(0, 0)
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(decimal.mark = ",", big.mark = ".")
      ) +
      ggplot2::labs(
        x = "Probabilidade",
        y = "Densidade",
        fill = "Jogo",
        color = "Mediana",
        title = NULL
      ) +
      theme_safe(base_size = 14) +
      ggplot2::theme(legend.position = "top")

    plotly::ggplotly(p, tooltip = c("x", "y", "fill", "color"))
  })

  # ------- KPI EWA -------
  # EWA
  output$kpi_ewa_bsg_card <- renderUI({ req(ewa_bsg()); kpi_duo_section("Médias de probabilidade — BSG", ewa_bsg()$long, "EWA", "BSG") })
  output$kpi_ewa_meg_card <- renderUI({ req(ewa_meg()); kpi_duo_section("Médias de probabilidade — MEG", ewa_meg()$long, "EWA", "MEG") })

  # RL
  output$kpi_rl_bsg_card <- renderUI({ req(rl_bsg()); kpi_duo_section("Médias de probabilidade — BSG", rl_bsg()$long, "RL", "BSG") })
  output$kpi_rl_meg_card <- renderUI({ req(rl_meg()); kpi_duo_section("Médias de probabilidade — MEG", rl_meg()$long, "RL", "MEG") })

  # BL
  output$kpi_bl_bsg_card <- renderUI({ req(bl_bsg()); kpi_duo_section("Médias de probabilidade — BSG", bl_bsg()$long, "BL", "BSG") })
  output$kpi_bl_meg_card <- renderUI({ req(bl_meg()); kpi_duo_section("Médias de probabilidade — MEG", bl_meg()$long, "BL", "MEG") })

  # Plots
  output$ewa_bsg <- renderPlotly({
    req(ewa_bsg())
    p <- plot_model(
      ewa_bsg()[["prop"]],
      "EWA",
      sprintf("BSG — EWA (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim = TRUE))),
      palette = pal
    )
    short_legend(p, legend_size = 12, itemwidth = 26) # <- aqui
  })

  output$ewa_meg <- renderPlotly({
    req(ewa_meg())
    p <- plot_model(
      ewa_meg()[["prop"]],
      "EWA",
      sprintf("MEG — EWA (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim = TRUE))),
      palette = pal
    )
    short_legend(p, legend_size = 12, itemwidth = 26) # <- aqui
  })

  output$rl_bsg <- renderPlotly({
    req(rl_bsg())
    p <- plot_model(
      rl_bsg()[["prop"]],
      "RL",
      sprintf("BSG — RL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim = TRUE))),
      palette = pal
    )
    short_legend(p, legend_size = 12, itemwidth = 26) # <- aqui
  })

  output$rl_meg <- renderPlotly({
    req(rl_meg())
    p <- plot_model(
      rl_meg()[["prop"]],
      "RL",
      sprintf("MEG — RL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim = TRUE))),
      palette = pal
    )
    short_legend(p, legend_size = 12, itemwidth = 26) # <- aqui
  })

  output$bl_bsg <- renderPlotly({
    req(bl_bsg())
    p <- plot_model(
      bl_bsg()[["prop"]],
      "BL",
      sprintf("BSG — BL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim = TRUE))),
      palette = pal
    )
    short_legend(p, legend_size = 12, itemwidth = 26) # <- aqui
  })

  output$bl_meg <- renderPlotly({
    req(bl_meg())
    p <- plot_model(
      bl_meg()[["prop"]],
      "BL",
      sprintf("MEG — BL (λ = %s)", gsub("\\.", ",", format(params()$lambda, trim = TRUE))),
      palette = pal
    )
    short_legend(p, legend_size = 12, itemwidth = 26) # <- aqui
  })

  # ------- Medidas-resumo: helper que devolve BSG/MEG nas linhas -------
  make_mr_tab <- function(rx_bsg, rx_meg, model_label) {
    req(rx_bsg, rx_meg)
    long_bsg <- dplyr::filter(rx_bsg$long, type == model_label)
    long_meg <- dplyr::filter(rx_meg$long, type == model_label)
    tb_bsg <- summary_statistics(long_bsg, "bsg")
    tb_meg <- summary_statistics(long_meg, "meg")
    norm <- function(tb, jogo) {
      names(tb) <- sub("_(bsg|meg)$", "", names(tb)) # remove sufixo dos nomes
      tb$Jogo <- jogo
      tb
    }
    dplyr::bind_rows(
      norm(tb_bsg, "BSG"),
      norm(tb_meg, "MEG")
    ) |>
      dplyr::select(Jogo, lambda, media, mediana, dp, min, max, iqr)
  }

  # ------- Reactable para cada aba -------
  output$mr_ewa_rt <- reactable::renderReactable({
    tab <- make_mr_tab(ewa_bsg(), ewa_meg(), "EWA"); req(nrow(tab) > 0)
    right_nums <- intersect(c("media","mediana","dp","min","max","iqr"), names(tab))
    cols <- c(
      list(
        Jogo   = reactable::colDef(name = "Jogo", sticky = "left"),
        lambda = reactable::colDef(name = "λ", align = "center", format = reactable::colFormat(digits = 1))
      ),
      setNames(lapply(right_nums, function(nm)
        reactable::colDef(align = "center", format = reactable::colFormat(digits = 1, separators = TRUE))
      ), right_nums)
    )
    reactable::reactable(tab, columns = cols, pagination = FALSE, highlight = TRUE, striped = TRUE)
  })

  output$mr_rl_rt <- reactable::renderReactable({
    tab <- make_mr_tab(rl_bsg(), rl_meg(), "RL"); req(nrow(tab) > 0)
    right_nums <- intersect(c("media","mediana","dp","min","max","iqr"), names(tab))
    cols <- c(
      list(
        Jogo   = reactable::colDef(name = "Jogo", sticky = "left"),
        lambda = reactable::colDef(name = "λ", align = "center", format = reactable::colFormat(digits = 1))
      ),
      setNames(lapply(right_nums, function(nm)
        reactable::colDef(align = "center", format = reactable::colFormat(digits = 1, separators = TRUE))
      ), right_nums)
    )
    reactable::reactable(tab, columns = cols, pagination = FALSE, highlight = TRUE, striped = TRUE)
  })

  output$mr_bl_rt <- reactable::renderReactable({
    tab <- make_mr_tab(bl_bsg(), bl_meg(), "BL"); req(nrow(tab) > 0)
    right_nums <- intersect(c("media","mediana","dp","min","max","iqr"), names(tab))
    cols <- c(
      list(
        Jogo   = reactable::colDef(name = "Jogo", sticky = "left"),
        lambda = reactable::colDef(name = "λ", align = "center", format = reactable::colFormat(digits = 1))
      ),
      setNames(lapply(right_nums, function(nm)
        reactable::colDef(align = "center", format = reactable::colFormat(digits = 1, separators = TRUE))
      ), right_nums)
    )
    reactable::reactable(tab, columns = cols, pagination = FALSE, highlight = TRUE, striped = TRUE)
  })

  # ---- Helpers p/ debug (head bonito: só a estratégia escolhida) ----
  # ordem preferida de jogadores (cai para a ordem de aparição se for outro jogo)
  player_levels <- function(v) {
    u <- unique(as.character(v))
    if (all(c("Comprador","Vendedor") %in% u)) c("Comprador","Vendedor")
    else if (all(c("Empresa A","Empresa B") %in% u)) c("Empresa A","Empresa B")
    else u
  }

  # monta a tabelinha de debug mostrando APENAS a estratégia vencedora por (jogador, período)
  make_debug_table <- function(rx, active_model, n = 6) {
    df <- rx[["prop"]] |>
      dplyr::filter(type == active_model)
    # força ordem J1 -> J2
    lvj <- player_levels(df$jogador)
    df$jogador <- factor(as.character(df$jogador), levels = lvj, ordered = TRUE)
    # dentro de cada grupo (modelo, sim, jogador, período) pega SÓ a maior proporção
    # em caso de empate, usa a ordem da coluna 'estrategias' para desempatar (determinístico)
    df_top <- df |>
      dplyr::arrange(estrategias) |>
      dplyr::group_by(type, sim_id, jogador, periodo) |>
      dplyr::slice_max(prop, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    # ordenação final: modelo -> jogador -> período (prop já é única por período)
    df_top <- df_top |>
      dplyr::arrange(type, jogador, periodo)
    # reparte as 'n' linhas igualmente entre os jogadores (ex.: n=6 => 3 períodos para cada)
    k <- length(levels(df_top$jogador))
    npp <- max(1, ceiling(n / max(1, k)))
    df_top <- df_top |>
      dplyr::group_by(jogador) |>
      dplyr::slice_head(n = npp) |>
      dplyr::ungroup() |>
      utils::head(n)
    # colunas enxutas
    df_top |>
      dplyr::select(type, jogador, estrategias, periodo, prop)
  }

  # colunas do reactable (com % no padrão PT-BR)
  dbg_cols <- list(
    type      = reactable::colDef(name = "Modelo", align = "center", minWidth = 90),
    jogador   = reactable::colDef(name = "Jogador"),
    estrategias = reactable::colDef(name = "Estratégia escolhida"),
    periodo   = reactable::colDef(name = "Período", align = "center", format = reactable::colFormat(digits = 0)),
    prop      = reactable::colDef(
      name = "Proporção", align = "center",
      cell = function(value) scales::percent(value, accuracy = 0.1, decimal.mark = ",")
    )
  )

  render_debug_rt <- function(rx, active_model) {
    tab <- make_debug_table(rx, active_model, n = 6)
    reactable::reactable(
      tab,
      columns = dbg_cols,
      pagination = FALSE,
      compact = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultSorted = list(jogador = "asc", periodo = "asc")
    )
  }

  # ---- Debug (head do agregado) — agora em reactable e ordenado pelo modelo ativo ----
  output$dbg_ewa_bsg <- reactable::renderReactable({ req(ewa_bsg(), input$modelo); render_debug_rt(ewa_bsg(), input$modelo) })
  output$dbg_ewa_meg <- reactable::renderReactable({ req(ewa_meg(), input$modelo); render_debug_rt(ewa_meg(), input$modelo) })
  output$dbg_rl_bsg  <- reactable::renderReactable({ req(rl_bsg(),  input$modelo); render_debug_rt(rl_bsg(),  input$modelo) })
  output$dbg_rl_meg  <- reactable::renderReactable({ req(rl_meg(),  input$modelo); render_debug_rt(rl_meg(),  input$modelo) })
  output$dbg_bl_bsg  <- reactable::renderReactable({ req(bl_bsg(),  input$modelo); render_debug_rt(bl_bsg(),  input$modelo) })
  output$dbg_bl_meg  <- reactable::renderReactable({ req(bl_meg(),  input$modelo); render_debug_rt(bl_meg(),  input$modelo) })

  # ---- Download CSV ----
  .models_all <- c("EWA","RL","BL")
  .games_all  <- c("BSG","MEG")
  .norm_sel   <- function(val, all_vals) if (identical(val, "Todos")) all_vals else val

  .get_rx <- function(mod, game){
    switch(paste(mod, game, sep = "_"),
      "EWA_BSG" = ewa_bsg(),
      "EWA_MEG" = ewa_meg(),
      "RL_BSG"  = rl_bsg(),
      "RL_MEG"  = rl_meg(),
      "BL_BSG"  = bl_bsg(),
      "BL_MEG"  = bl_meg(),
      NULL
    )
  }

  .build_download_df <- function(model_sel, game_sel, kind){
    mods  <- .norm_sel(model_sel, .models_all) # "Todos" -> c("EWA","RL","BL")
    games <- .norm_sel(game_sel,  .games_all)  # "Todos" -> c("BSG","MEG")
    dfs <- list()
    for (m in mods) for (g in games){
      rx <- .get_rx(m, g)
      # Em vez de falhar, pula combos ainda não simulados
      if (is.null(rx)) next
      df <- rx[[kind]]
      if (is.null(df) || !nrow(df)) next
      # Mantém só o modelo selecionado (por segurança)
      if ("type" %in% names(df)) df <- dplyr::filter(df, .data$type == !!m)
      # Garante lambda
      if (!"lambda" %in% names(df)) df$lambda <- params()$lambda
      df$lambda <- as.numeric(df$lambda)
      if (kind == "prop") {
        # Garante 'jogo' e padroniza classes para juntar BSG+MEG
        if (!"jogo" %in% names(df)) {
          j <- if ("jogo" %in% names(rx$long)) unique(rx$long$jogo) else g
          df$jogo <- j[1]
        }
        df$jogo     <- as.character(df$jogo)
        df$jogador  <- as.character(df$jogador)
        df$estrategias<- as.character(df$estrategias)
      }
      dfs[[paste(m, g, kind, sep = "_")]] <- df
    }
    # Se nada sobrou, aí sim avisa claramente
    shiny::validate(shiny::need(length(dfs) > 0, "Rode uma simulação para os filtros escolhidos antes de baixar."))
    out <- dplyr::bind_rows(dfs)
    if (kind == "prop") {
      out <- dplyr::arrange(out, .data$type, .data$jogo, .data$jogador, .data$estrategias, .data$periodo)
    } else {
      keep <- intersect(c("type","sim_id","jogo","amostra","jogador","periodo",
                          "estrategias","estrategia_escolhida","lambda"),
                        names(out))
      out <- out[, keep, drop = FALSE] |>
        dplyr::arrange(.data$type, .data$jogo, .data$amostra, .data$jogador, .data$periodo)
    }
    out
  }

  # --------- DICIONÁRIO DE DADOS / METADADOS ---------
  # Mapeamento de descrições "amigáveis" para colunas conhecidas
  .col_desc <- c(
    type = "Modelo de aprendizado (EWA, RL, BL).",
    sim_id = "Identificador da simulação.",
    jogo = "Rótulo do jogo (BSG ou MEG).",
    amostra = "Identificador da trajetória (amostra).",
    periodo = "Período (tempo/rodada) da simulação.",
    jogador = "Agente/jogador.",
    estrategias = "Estratégia disponível no jogo.",
    estrategia_escolhida= "Estratégia efetivamente escolhida no período.",
    prop = "Proporção de escolha (agregado) da estratégia.",
    probabilidade = "Probabilidade de escolher a estratégia (nível micro).",
    atracao = "Atração (EWA/RL/BL) associada à estratégia no período.",
    lambda = "Parâmetro λ (taxa de esquecimento).",
    total_amostra = "Total de trajetórias simuladas.",
    total_periodo = "Total de períodos simulados.",
    seed = "Semente da simulação (se fixada).",
    timestamp = "Carimbo de data/hora da geração do arquivo."
  )

  # Gera uma tabela de metadados a partir de um data.frame
  build_dict <- function(df, arquivo_label) {
    stopifnot(is.data.frame(df))
    if (!nrow(df)) return(tibble::tibble())
    tibble::tibble(
      Arquivo = arquivo_label,
      Variavel = names(df)
    ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        Tipo = paste(class(df[[Variavel]]), collapse = "/"),
        `NAs (%)` = round(mean(is.na(df[[Variavel]])) * 100, 2),
        Exemplo = {
          x <- df[[Variavel]]
          x <- x[!is.na(x)]
          if (length(x)) as.character(utils::head(x, 1)) else "—"
        },
        Min = {
          x <- df[[Variavel]]
          if (inherits(x, c("numeric","integer","Date","POSIXct","POSIXt")))
            suppressWarnings(min(x, na.rm = TRUE)) else NA
        },
        Max = {
          x <- df[[Variavel]]
          if (inherits(x, c("numeric","integer","Date","POSIXct","POSIXt")))
            suppressWarnings(max(x, na.rm = TRUE)) else NA
        },
        `Valores únicos (amostra)` = {
          x <- df[[Variavel]]
          if (is.logical(x) || is.character(x) || is.factor(x)) {
            ux <- unique(as.character(stats::na.omit(x)))
            if (length(ux) > 10) paste0(paste(utils::head(ux, 10), collapse = ", "), " +", length(ux) - 10, "…")
            else paste(ux, collapse = ", ")
          } else "—"
        },
        Descricao = if (is.null(.col_desc[[Variavel]])) "—" else .col_desc[[Variavel]]
      ) |>
      dplyr::ungroup() |>
      dplyr::select(Arquivo, Variavel, Tipo, Descricao, Exemplo, `Valores únicos (amostra)`, Min, Max, `NAs (%)`)
  }

  # Reúne FONTES GERAIS (TODOS os modelos + AMBOS os jogos) para o dicionário
  .build_meta_sources_global <- function(){
    req(ewa_bsg(), rl_bsg(), bl_bsg(), ewa_meg(), rl_meg(), bl_meg())
    # coleções
    props <- list(ewa_bsg()$prop, rl_bsg()$prop, bl_bsg()$prop, ewa_meg()$prop, rl_meg()$prop, bl_meg()$prop)
    longs <- list(ewa_bsg()$long, rl_bsg()$long, bl_bsg()$long, ewa_meg()$long, rl_meg()$long, bl_meg()$long)

    # >>> UNIFICAÇÃO (inclui BSG + MEG e os 3 modelos) <<<
    props_all <- dplyr::bind_rows(props)
    longs_all <- dplyr::bind_rows(longs)

    # garante λ presente (para refletir o que sai no CSV)
    if (!"lambda" %in% names(props_all)) props_all$lambda <- params()$lambda
    props_all$lambda <- as.numeric(props_all$lambda)
    if (!"lambda" %in% names(longs_all)) longs_all$lambda <- params()$lambda
    longs_all$lambda <- as.numeric(longs_all$lambda)

    # Simulação (long BSG+MEG) + metadados padrão
    seed_used <- if (isTRUE(isolate(input$fix_seed))) as.integer(isolate(input$seed_val)) else NA_integer_
    sim_all <- dplyr::bind_rows(longs) |>
      dplyr::mutate(
        lambda        = as.numeric(if (!"lambda" %in% names(.)) params()$lambda else lambda),
        total_amostra = params()$n_samples,
        total_periodo = params()$n_periods,
        seed          = seed_used,
        timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    list(
      # agora o dicionário de Resultados enxerga **tudo** (prop + long, BSG + MEG)
      results_list = list(prop = props_all, long = longs_all),
      sim = sim_all
    )
  }

  output$download_csv <- downloadHandler(
    filename = function() {
      sprintf("sim_%s_%s_%s_%s.csv",
              tolower(input$dl_model), tolower(input$dl_game), tolower(input$dl_kind),
              format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    content = function(file) {
      df <- .build_download_df(input$dl_model, input$dl_game, input$dl_kind)
      # metadados iguais aos que você já adicionava
      seed_used <- if (isTRUE(isolate(input$fix_seed))) as.integer(isolate(input$seed_val)) else NA_integer_
      df <- df %>%
        dplyr::mutate(
          total_amostra = params()$n_samples,
          total_periodo = params()$n_periods,
          seed          = seed_used,
          timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      readr::write_csv(df, file)
    }
  )

  # ---- Download da SIMULAÇÃO (todos os modelos + ambos os jogos, long unificado) ----
  output$download_sim <- downloadHandler(
    filename = function(){
      sprintf("simulacao_todos_modelos_bsg_meg_%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    content = function(file){
      # Garante que todas as reativas existem (o app já dispara 1 simulação ao carregar)
      req(ewa_bsg(), rl_bsg(), bl_bsg(), ewa_meg(), rl_meg(), bl_meg())
      # Junta TUDO (3 modelos × 2 jogos) no formato long
      df_long <- dplyr::bind_rows(
        ewa_bsg()$long, rl_bsg()$long, bl_bsg()$long,
        ewa_meg()$long, rl_meg()$long, bl_meg()$long
      )
      # Garante lambda e adiciona metadados padrão
      if (!"lambda" %in% names(df_long)) df_long$lambda <- params()$lambda
      df_long$lambda <- as.numeric(df_long$lambda)
      seed_used <- if (isTRUE(isolate(input$fix_seed))) as.integer(isolate(input$seed_val)) else NA_integer_
      df_long <- df_long %>%
        dplyr::mutate(
          total_amostra = params()$n_samples,
          total_periodo = params()$n_periods,
          seed          = seed_used,
          timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ) %>%
        dplyr::arrange(sim_id, type, jogo, amostra, periodo)
      # Ordem “amigável” das colunas (usa só as que existirem)
      ordem <- c(
        "type","sim_id","jogo","amostra","periodo","jogador",
        "estrategias","estrategia_escolhida","probabilidade","atracao",
        "lambda","total_amostra","total_periodo","seed","timestamp"
      )
      keep <- intersect(ordem, names(df_long))
      df_long <- df_long[, keep, drop = FALSE]
      readr::write_csv(df_long, file)
    }
  )

  # ---- Download dos METADADOS (Excel estilizado; fallback CSV em Shinylive) ----

  # --------- DICIONÁRIO DE DADOS / METADADOS (GLOBAL) ---------
  # Tradutor de classes R -> PT-BR (uma etiqueta por coluna)
  .class_pt <- function(x){
    cl <- class(x)
    if ("integer" %in% cl) return("Inteiro")
    if ("numeric" %in% cl || "double" %in% cl) return("Numérico")
    if ("character" %in% cl) return("Texto")
    if ("logical" %in% cl) return("Lógico")
    if ("Date" %in% cl) return("Data")
    if (any(c("POSIXct","POSIXt") %in% cl)) return("Data-Hora")
    if ("ordered" %in% cl || (is.factor(x) && is.ordered(x))) return("Fator ordenado")
    if ("factor" %in% cl || is.factor(x)) return("Fator")
    if ("list" %in% cl) return("Lista")
    paste(cl, collapse = "/")
  }

  # Dicionário a partir de UMA coluna concreta
  .col_dict_row <- function(df, var){
    x <- df[[var]]
    tibble::tibble(
      Variavel = var,
      Tipo     = .class_pt(x),
      Descricao = if (is.null(.col_desc[[var]])) "—" else .col_desc[[var]],
      `Valores únicos (amostra)` = {
        if (is.logical(x) || is.character(x) || is.factor(x)) {
          ux <- unique(as.character(stats::na.omit(x)))
          if (length(ux) > 10) paste0(paste(utils::head(ux, 10), collapse = ", "), " +", length(ux) - 10, "…")
          else paste(ux, collapse = ", ")
        } else "—"
      },
      Min = if (inherits(x, c("numeric","integer","Date","POSIXct","POSIXt")))
              suppressWarnings(min(x, na.rm = TRUE)) else NA,
      Max = if (inherits(x, c("numeric","integer","Date","POSIXct","POSIXt")))
              suppressWarnings(max(x, na.rm = TRUE)) else NA,
      `NAs (%)` = round(mean(is.na(x)) * 100, 2)
    )
  }

  # Constrói dicionário a partir da UNIÃO de colunas de vários data.frames
  build_dict_from_list <- function(dfs, arquivo_label){
    # nomes únicos, preservando ordem de aparição
    nm <- unique(unlist(lapply(dfs, names)))
    rows <- lapply(nm, function(v){
      # pega o primeiro df que contém a coluna v
      carrier <- NULL
      for (d in dfs) if (v %in% names(d)) { carrier <- d; break }
      .col_dict_row(carrier, v)
    })
    out <- dplyr::bind_rows(rows)
    dplyr::mutate(out, Arquivo = arquivo_label, .before = 1L) |>
      dplyr::select(Arquivo, dplyr::everything())
  }

  # Reúne FONTES GERAIS (TODOS os modelos e AMBOS os jogos) para o dicionário
  .build_meta_sources_global <- function(){
    # garante que as reativas já existem (o app roda 1a simulação na carga)
    req(ewa_bsg(), rl_bsg(), bl_bsg(), ewa_meg(), rl_meg(), bl_meg())

    # — Resultados (prop e long) — usamos exemplos de cada tipo (não depende do modelo)
    props <- list(ewa_bsg()$prop, rl_bsg()$prop, bl_bsg()$prop, ewa_meg()$prop, rl_meg()$prop, bl_meg()$prop)
    longs <- list(ewa_bsg()$long, rl_bsg()$long, bl_bsg()$long, ewa_meg()$long, rl_meg()$long, bl_meg()$long)

    # — Simulação (long BSG+MEG) — juntamos tudo e adicionamos metadados padrão
    df_sim <- dplyr::bind_rows(
      ewa_bsg()$long, rl_bsg()$long, bl_bsg()$long,
      ewa_meg()$long, rl_meg()$long, bl_meg()$long
    )
    if (!"lambda" %in% names(df_sim)) df_sim$lambda <- params()$lambda
    df_sim$lambda <- as.numeric(df_sim$lambda)
    seed_used <- if (isTRUE(isolate(input$fix_seed))) as.integer(isolate(input$seed_val)) else NA_integer_
    df_sim <- df_sim |>
      dplyr::mutate(
        total_amostra = params()$n_samples,
        total_periodo = params()$n_periods,
        seed          = seed_used,
        timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

    list(
      results_list = list( # união de prop + long
        prop = props[[1]],
        long = longs[[1]] # basta 1 de cada tipo p/ inferir colunas
      ),
      sim = df_sim
    )
  }

  # ---------- DOWNLOAD DOS METADADOS (XLSX global; fallback CSV) ----------
  output$download_meta <- downloadHandler(
    filename = function(){
      ext <- if (!is_shinylive && requireNamespace("openxlsx", quietly = TRUE)) "xlsx" else "csv"
      sprintf("metadados_geral_%s.%s", format(Sys.time(), "%Y%m%d-%H%M%S"), ext)
    },
    content = function(file){
      src <- .build_meta_sources_global()

      # Garante timestamp também nas fontes do "Dicionário – Resultados"
      ts_now  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      res_all <- src$results_list
      if (!"timestamp" %in% names(res_all$prop)) res_all$prop$timestamp <- ts_now
      if (!"timestamp" %in% names(res_all$long)) res_all$long$timestamp <- ts_now

      # DICIONÁRIOS (sem coluna "Exemplo")
      dic_resultados <- build_dict_from_list(
        dfs = unname(res_all),
        arquivo_label = "Resultados (prop + long)"
      )
      dic_sim <- build_dict_from_list(
        dfs = list(src$sim),
        arquivo_label = "Simulação (BSG + MEG, long)"
      )

      # --- FORÇA "VALORES ÚNICOS (AMOSTRA)" PARA 4 VARIÁVEIS-CHAVE (UNIÃO BSG+MEG+MODELOS) ---
      vars_union <- c("jogo", "jogador", "estrategias", "estrategia_escolhida")
      # lista de fontes a considerar (prop + long de tudo + sim unificada)
      df_pool <- c(unname(src$results_list), list(src$sim))
      get_uniques_union <- function(dfs, var) {
        xs <- lapply(dfs, function(d) if (var %in% names(d)) d[[var]] else NULL)
        xs <- Filter(Negate(is.null), xs)
        if (!length(xs)) return("—")
        xs <- unlist(lapply(xs, function(x) if (is.factor(x)) as.character(x) else x), use.names = FALSE)
        xs <- xs[!is.na(xs)]
        if (!length(xs)) return("—")
        ux <- unique(xs)
        if (length(ux) > 10) paste0(paste(utils::head(ux, 10), collapse = ", "), " +", length(ux) - 10, "…")
        else paste(ux, collapse = ", ")
      }
      for (v in vars_union) {
        i <- which(dic_resultados$Variavel == v)
        if (length(i) == 1) {
          dic_resultados$`Valores únicos (amostra)`[i] <- get_uniques_union(df_pool, v)
        }
      }

      # ------------------------------------------------------------------------------

      has_xlsx <- !is_shinylive && requireNamespace("openxlsx", quietly = TRUE)
      if (!has_xlsx){
        # Fallback CSV (concatena as duas abas)
        full_dic <- dplyr::bind_rows(dic_resultados, dic_sim)
        readr::write_csv(full_dic, file)
        return(invisible())
      }

      # ============== XLSX estilizado ==============
      wb <- openxlsx::createWorkbook()

      # ---- Estilos
      title <- openxlsx::createStyle(fontSize = 16, textDecoration = "bold", halign="left", valign="center")
      h2    <- openxlsx::createStyle(fontSize = 13, textDecoration = "bold", halign="left", valign="center")
      hdr   <- openxlsx::createStyle(fgFill="#1f3b57", fontColour="#FFFFFF", halign="center",
                                     textDecoration="bold", border="Bottom")
      body  <- openxlsx::createStyle(halign="left", valign="top", wrapText=TRUE)
      num   <- openxlsx::createStyle(numFmt = "0.00")

      # ---------------- README ----------------
      openxlsx::addWorksheet(wb, "README")
      openxlsx::writeData(wb, "README", "Dicionário de Dados — EWA Market Dynamics", startRow=1, startCol=1)
      openxlsx::addStyle(wb, "README", title, rows=1, cols=1, gridExpand=TRUE)

      # Seção: Jogos modelados
      openxlsx::writeData(wb, "README", "Jogos modelados", startRow=3, startCol=1)
      openxlsx::addStyle(wb, "README", h2, rows=3, cols=1, gridExpand=TRUE)

      # Buyer–Seller Game (BSG)
      openxlsx::writeData(wb, "README", "Buyer–Seller Game (BSG)", startRow=5, startCol=1)
      openxlsx::addStyle(wb, "README", h2, rows=5, cols=1, gridExpand=TRUE)
      # Estratégias
      bsg_estr <- tibble::tibble(
        `Vendedor (s1)` = c("Preço Alto", "Preço Baixo"),
        `Comprador (s2)`= c("Aceitar", "Rejeitar")
      )
      openxlsx::writeData(wb, "README", bsg_estr, startRow=6, startCol=1, borders="rows", headerStyle=hdr)

      # Payoffs EXATOS (BSG) — a partir do seu payoffs.R
      r <- 9
      openxlsx::writeData(wb, "README", "Payoffs exatos (BSG)", startRow=r, startCol=1)
      openxlsx::addStyle(wb, "README", h2, rows=r, cols=1, gridExpand=TRUE); r <- r + 1
      lines_bsg <- c(
        "Constante de custo: c = 25.",
        "Mapeamento de preço do vendedor: p ∈ {45 (“Preço Alto”), 35 (“Preço Baixo”)}.",
        "Indicador de aceitação: a = 1 se estratégia_comprador = “Aceitar”; a = 0 se “Rejeitar”.",
        "Payoff do Vendedor: π_Vendedor = a · (p − c).",
        "Valoração do Comprador: v_min = c + k, com k ∈ {1,…,10} escolhido aleatoriamente; v_max = 45; v ~ Uniforme[v_min, v_max].",
        "Payoff do Comprador: π_Comprador = a · (v − p). (Na função, o resultado é arredondado para 2 casas.)",
        "Se “Rejeitar” (a = 0), ambos os payoffs são 0."
      )
      openxlsx::writeData(wb, "README", lines_bsg, startRow=r, startCol=1); r <- r + length(lines_bsg) + 1

      # Market Entry Game (MEG)
      openxlsx::writeData(wb, "README", "Market Entry Game (MEG)", startRow=r, startCol=1)
      openxlsx::addStyle(wb, "README", h2, rows=r, cols=1, gridExpand=TRUE); r <- r + 1
      # Matriz de payoffs (pares: Empresa A, Empresa B)
      meg_tbl <- tibble::tibble(
        `A \\ B`      = c("Não Entrar", "Entrar"),
        `Não Entrar`  = c("(0, 0)", "(0, 5)"),
        `Entrar`      = c("(5, 0)", "(-1, -1)")
      )
      openxlsx::writeData(wb, "README", meg_tbl, startRow=r, startCol=1, borders="rows", headerStyle=hdr); r <- r + 4

      # Nota final do README
      openxlsx::writeData(
        wb, "README",
        "Este dicionário descreve as variáveis que aparecem nos arquivos de resultados (prop + long) e no arquivo de simulação (BSG+MEG, long).",
        startRow=r, startCol=1
      )

      openxlsx::setColWidths(wb, "README", cols=1:4, widths=c(54, 24, 24, 24))
      openxlsx::setRowHeights(wb, "README", rows = 1, heights = 24)

      # ---------------- Dicionário – Resultados ----------------
      openxlsx::addWorksheet(wb, "Dicionário – Resultados")
      openxlsx::writeData(wb, "Dicionário – Resultados", dic_resultados, headerStyle = hdr, borders = "rows")
      openxlsx::freezePane(wb, "Dicionário – Resultados", firstActiveRow = 2, firstActiveCol = 1)
      openxlsx::addStyle(wb, "Dicionário – Resultados", style = body, rows = 2:(nrow(dic_resultados)+1), cols = 1:ncol(dic_resultados), gridExpand = TRUE)
      for (nm in c("Min","Max","NAs (%)")) {
        j <- which(names(dic_resultados) == nm)
        if (length(j)) openxlsx::addStyle(wb, "Dicionário – Resultados", num, rows = 2:(nrow(dic_resultados)+1), cols = j, gridExpand = TRUE)
      }
      openxlsx::setColWidths(wb, "Dicionário – Resultados", cols = 1:ncol(dic_resultados),
                             widths = c(30, 28, 20, 64, 44, 16, 16, 12))

      # ---------------- Dicionário – Simulação ----------------
      openxlsx::addWorksheet(wb, "Dicionário – Simulação")
      openxlsx::writeData(wb, "Dicionário – Simulação", dic_sim, headerStyle = hdr, borders = "rows")
      openxlsx::freezePane(wb, "Dicionário – Simulação", firstActiveRow = 2, firstActiveCol = 1)
      openxlsx::addStyle(wb, "Dicionário – Simulação", style = body, rows = 2:(nrow(dic_sim)+1), cols = 1:ncol(dic_sim), gridExpand = TRUE)
      for (nm in c("Min","Max","NAs (%)")) {
        j <- which(names(dic_sim) == nm)
        if (length(j)) openxlsx::addStyle(wb, "Dicionário – Simulação", num, rows = 2:(nrow(dic_sim)+1), cols = j, gridExpand = TRUE)
      }
      openxlsx::setColWidths(wb, "Dicionário – Simulação", cols = 1:ncol(dic_sim),
                             widths = c(30, 28, 20, 64, 44, 16, 16, 12))

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# ---- Inicialização ----
shinyApp(ui, server)
