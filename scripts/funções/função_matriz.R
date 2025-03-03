# Função para exportar um jogo normal form (2x2) para PDF usando ggplot
export_game_table <- function(game_matrix, title, footnote = NULL, file, height = 6, width = 8, overwrite = FALSE) {

  # Se overwrite for TRUE, remover o arquivo existente
  if (overwrite && file.exists(file)) {
    file.remove(file)
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
    ` ` = c(player1, rep("", length(strategies_p1) - 1)),  # Criar a coluna extra para Vendedor
    `  ` = strategies_p1,  # Estratégias do jogador 1 (Preço Alto/Preço Baixo)
    !!strategies_p2[1] := c(paste(payoffs1[1], ",", payoffs2[1]), paste(payoffs1[2], ",", payoffs2[2])),
    !!strategies_p2[2] := c(paste(payoffs1[3], ",", payoffs2[3]), paste(payoffs1[4], ",", payoffs2[4]))
  )

  # Criar flextable formatado
  ft <- flextable(game_table) |>
    add_header_row(values = c("", player2), colwidths = c(2, 2)) |>  # Ajustar cabeçalho
    merge_at(i = 1:2, j = 1) |>  # Mesclar célula do jogador 1 para ocupar 2 linhas
    theme_vanilla() |>
    autofit()  # Ajuste automático do tamanho das colunas

  # Adicionar rodapé se houver
  if (!is.null(footnote)) {
    ft <- add_footer_lines(ft, footnote)
  }

  # Converter flextable para rasterGrob
  ft_raster <- as_raster(ft)

  # Criar ggplot vazio com a tabela adicionada
  plot <- ggplot() +
    theme_void() +
    labs(title = title) +
    annotation_custom(rasterGrob(ft_raster), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(t = 100, b = -100))
    )

  # Salvar como PDF
  pdf(file, height = height, width = width, family = "Times")
  print(plot)
  dev.off()

  message("✅ Tabela exportada para PDF: ", file)
}

# Criar função para gerar e salvar gráficos de tabelas usando flextable
plot_table <- function(data, title, font_size, footnote = NULL, file, height = NULL, width = NULL, overwrite = FALSE) {

  # Se overwrite for TRUE, remover o arquivo existente
  if (overwrite && file.exists(file)) {
    file.remove(file)
  }

  # Criar tabela formatada com flextable e adicionar título como linha de cabeçalho
  ft <- flextable(data) |>
    add_footer_lines(footnote) |>
    autofit()                              # Ajuste automático do tamanho das colunas

  # Converter flextable para imagem raster
  ft_raster <- as_raster(ft)

  # Criar um ggplot vazio para adicionar a tabela
  plot <- ggplot() +
    theme_void() +
    labs(title = title) +
    annotation_custom(rasterGrob(ft_raster), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(
      plot.title = element_text(size = font_size, face = "bold", hjust = 0.5, vjust = 20, margin = margin(t = 110, b = -60))
    )

  # Salvar como PDF
  pdf(file, height = height, width = width, family = "Times")
  print(plot)
  dev.off()

  message("✅ Tabela exportada para PDF: ", file)
}

