### ============== FUNÇÕES PARA TABELAS (E MATRIZES 2X2) ======================

# Função para exportar um jogo normal form (2x2) para PDF usando ggplot2
export_game_table <- function(game_matrix, title, footnote = NULL, file,
                              height = 6, width = 8, overwrite = FALSE) {

  # Se overwrite for TRUE, remover o arquivo existente
  if (overwrite && base::file.exists(file)) {
    base::file.remove(file)
  }

  # Extrair informações do jogo
  player1 <- game_matrix$player[1]  # Primeiro jogador (p1)
  player2 <- game_matrix$player[2]  # Segundo jogador (p2)

  strategies_p1 <- game_matrix$strategy$s1  # Estratégias do jogador 1
  strategies_p2 <- game_matrix$strategy$s2  # Estratégias do jogador 2

  payoffs1 <- game_matrix$payoff$payoffs1  # Payoff do jogador 1
  payoffs2 <- game_matrix$payoff$payoffs2  # Payoff do jogador 2

  # Criar a tabela do jogo corretamente
  game_table <- tibble::tibble(
    ` ` = c(player1, base::rep("", base::length(strategies_p1) - 1)),  # Criar a coluna extra para o jogador 1
    `  ` = strategies_p1,  # Estratégias do jogador 1
    !!strategies_p2[1] := c(base::paste(payoffs1[1], ",", payoffs2[1]),
                            base::paste(payoffs1[2], ",", payoffs2[2])),
    !!strategies_p2[2] := c(base::paste(payoffs1[3], ",", payoffs2[3]),
                            base::paste(payoffs1[4], ",", payoffs2[4]))
  )

  ft <- flextable::flextable(game_table) |>
    flextable::add_header_row(values = c("", player2), colwidths = c(2, 2)) |>  # Ajustar cabeçalho
    flextable::merge_at(i = 1:2, j = 1) |>  # Mesclar célula do jogador 1 para ocupar 2 linhas
    flextable::theme_vanilla() |>
    flextable::autofit()  # Ajuste automático do tamanho das colunas

  # Adicionar rodapé se houver
  if (!base::is.null(footnote)) {
    ft <- flextable::add_footer_lines(ft, footnote)
  }

  # Converter flextable para rasterGrob
  ft_raster <- flextable::as_raster(ft)

  # Criar ggplot vazio com a tabela adicionada
  plot <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::annotation_custom(grid::rasterGrob(ft_raster), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 25, face = "bold", hjust = 0.5,
        margin = ggplot2::margin(t = 100, b = -100))
    )

  # Salvar como PDF
  grDevices::pdf(file, height = height, width = width)
  base::print(plot)
  grDevices::dev.off()

  base::message("✅ Tabela exportada para PDF: ", file)
}

# Criar função para gerar e salvar gráficos de tabelas usando flextable
model_stats_table <- function(data, file,
                              modelo   = "EWA",   # 🔹 define manualmente
                              title    = NULL,
                              footnote = "Fonte: Elaborado pelo autor (2024).",
                              font_size = 11) {

  # Seleciona colunas e renomeia
  df <- data |>
    dplyr::select(
      lambda,
      media_m1,  mediana_m1,  dp_m1,  min_m1,  max_m1,  cv_m1,
      media_m2,  mediana_m2,  dp_m2,  min_m2,  max_m2,  cv_m2
    ) |>
    dplyr::rename(
      `λ`      = lambda,
      `Média_BSG` = media_m1,  `Md_BSG` = mediana_m1, `DP_BSG` = dp_m1,
      `Min_BSG`   = min_m1,    `Máx_BSG`= max_m1,     `CV_BSG` = cv_m1,
      `Média_MEG` = media_m2,  `Md_MEG` = mediana_m2, `DP_MEG` = dp_m2,
      `Min_MEG`   = min_m2,    `Máx_MEG`= max_m2,     `CV_MEG` = cv_m2
    )

  keys <- colnames(df)

  # Cabeçalho dinâmico usando argumento 'modelo'
  header_map <- data.frame(
    keys   = keys,
    top    = c(modelo, rep("BSG", 6), rep("MEG", 6)),  
    bottom = c("λ", "Média","Md","DP","Min","Máx","CV",
               "Média","Md","DP","Min","Máx","CV"),
    check.names = FALSE
  )

  ft <- flextable::flextable(df) |>
    flextable::set_header_df(mapping = header_map, key = "keys") |>
    flextable::merge_h(part = "header") |>
    flextable::theme_booktabs() |>
    flextable::colformat_num(j = 2:ncol(df), digits = 2,
                             decimal.mark = ",", big.mark = ".") |>
    flextable::align(align = "center", part = "all") |>
    flextable::add_footer_lines(footnote) |>
    flextable::align(align = "center", part = "footer") |>
    flextable::autofit() |>
    flextable::bg(bg = "white", part = "all")

  if (!is.null(title)) {
    ft <- flextable::add_header_lines(ft, values = title)
  }

  ft <- flextable::fontsize(ft, size = font_size, part = "all")

  flextable::save_as_image(ft, path = file)

  message("✅ Tabela exportada: ", modelo, " → ", file)
}

