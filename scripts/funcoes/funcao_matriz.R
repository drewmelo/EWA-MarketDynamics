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

  # Criar flextable formatado
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
  grDevices::pdf(file, height = height, width = width, family = "Times")
  base::print(plot)
  grDevices::dev.off()

  base::message("✅ Tabela exportada para PDF: ", file)
}

# Criar função para gerar e salvar gráficos de tabelas usando flextable
plot_table <- function(data, title, font_size, footnote = NULL, file,
                       height = NULL, width = NULL, overwrite = FALSE) {

  # Se overwrite for TRUE, remover o arquivo existente
  if (overwrite && base::file.exists(file)) {
    base::file.remove(file)
  }

  # Criar tabela formatada com flextable e adicionar título como linha de cabeçalho
  ft <- flextable::flextable(data) |>
    flextable::add_footer_lines(footnote) |>
    flextable::autofit()  # Ajuste automático do tamanho das colunas

  # Converter flextable para imagem raster
  ft_raster <- flextable::as_raster(ft)

  # Criar um ggplot vazio para adicionar a tabela
  plot <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::annotation_custom(grid::rasterGrob(ft_raster),
                               xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = font_size, face = "bold", hjust = 0.5, vjust = 20,
        margin = ggplot2::margin(t = 110, b = -60))
    )

  # Salvar como PDF
  grDevices::pdf(file, height = height, width = width, family = "Times")
  base::print(plot)
  grDevices::dev.off()

  base::message("✅ Tabela exportada para PDF: ", file)
}
