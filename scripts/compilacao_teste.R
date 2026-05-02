### ========== TESTE DE RESULTADOS (COMPILAÇÃO DO EQUILÍBRIO DE NASH) ===========

# Carregando os payoffs dos jogadores
# Ative a linha abaixo apenas se o script principal ("main") não tiver sido executado
# source("scripts/payoffs.R")

# Inicialize uma lista para armazenar os data frames
lista_dfs <- base::list()

# Verificando o resultado (100 vezes)
viewer_antigo <- getOption("viewer")
options(viewer = function(url, height = NULL) invisible(NULL))

lista_dfs <- base::list()

n_iter <- 100
pb <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)

for (i in seq_len(n_iter)) {
  
  matriz_1_ex <- rgamer::normal_form(
    players = c("Vendedor", "Comprador"),
    pars = c("preco_vendedor", "estrategia_comprador"),
    s1 = c("Preço Alto", "Preço Baixo"),
    s2 = c("Aceitar", "Rejeitar"),
    payoffs1 = func_payoff1,
    payoffs2 = func_payoff2,
    discretize = TRUE
  )
  
  tmp <- tempfile()
  con <- file(tmp, open = "wt")
  
  resultado <- tryCatch({
    sink(con, type = "output")
    sink(con, type = "message")
    
    rgamer::solve_nfg(matriz_1_ex, mark_br = TRUE)
    
  }, finally = {
    if (sink.number(type = "message") > 0) sink(type = "message")
    if (sink.number(type = "output") > 0) sink(type = "output")
    close(con)
    unlink(tmp)
  })
  
  if (base::is.character(resultado)) {
    df_resultado <- tibble::tibble(NE = resultado)
  } else {
    df_resultado <- tibble::tibble(NE = resultado[1])
  }
  
  lista_dfs[[i]] <- df_resultado
  
  utils::setTxtProgressBar(pb, i)
}

close(pb)
options(viewer = viewer_antigo)

ne_m1 <- base::do.call(rbind, lista_dfs)

# Concatenar todos os elementos dentro de cada lista como uma string única
ne_m1$NE <- base::sapply(ne_m1$NE, function(x) base::paste(x, collapse = ", "))

# Agrupando por categoria de NE e calculando a contagem acumulada
ne_m1 <- ne_m1 |>
  dplyr::group_by(NE) |>
  dplyr::mutate(count_n = base::cumsum(base::rep(1, dplyr::n()))) |>
  dplyr::ungroup()

# Criando o gráfico com ggplot2
p19 <- ggplot2::ggplot(ne_m1, ggplot2::aes(x = count_n, color = NE, group = NE)) +
  ggplot2::geom_freqpoly(binwidth = 3.7, linewidth = 1.6, position = 'jitter') +
  ggplot2::labs(
    # title = "Contagem Acumulada por Categoria de NE"  # Se precisar do título, remova o comentário
    x = "Número de Ocorrências",
    y = "Contagem Acumulada",
    color = NULL  # Remove o título da legenda
  ) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  ggplot2::scale_color_manual(values = color_main) +  # Define as cores manualmente
  beautyxtrar::theme_academic(base_family = fonte_base) +  # Aplica o tema personalizado
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 20),
    axis.text = ggplot2::element_text(size = 20),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(l = 5, unit = "pt")),
    axis.title.x = ggplot2::element_text(size = 22),
    axis.title.y = ggplot2::element_text(size = 22, angle = 90)
  )

