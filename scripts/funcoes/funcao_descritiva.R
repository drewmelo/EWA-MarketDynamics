### ================== FUNÇÃO PARA ANÁLISE DESCRITIVA =========================

## MEDIDAS-RESUMO  ------------------------------------------------------------
# Função para calcular medidas-resumo
summary_statistics <- function(df, sufixo) {
  df |>
    dplyr::filter(estrategia_escolhida == estrategias) |>
    dplyr::group_by(type, lambda) |>
    dplyr::reframe(
      media = base::round(base::mean(atracao, na.rm = T), 1),
      mediana = base::round(stats::median(atracao, na.rm = T), 1),
      dp = base::round(stats::sd(atracao, na.rm = T), 1),
      min = base::round(base::min(atracao, 1)),
      max = base::round(base::max(atracao, 1)),
      cv = base::round(dp / media, 1)
    ) |>
    dplyr::rename_with(~ base::paste0(.x, "_", sufixo), .cols = 3:8) # Adiciona sufixo personalizado
}

# Função para criar tabelas combinando BSG e MEG por tipo
summary_tables <- function(modelo, lista_resumo) {
  lista_resumo$mr_bsg |>
    dplyr::filter(type == modelo) |>
    dplyr::bind_cols(lista_resumo$mr_meg |> dplyr::filter(type == modelo)) |>
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
      cont_n = base::sum(dplyr::n())
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

# Criar função para formatar os dados corretamente
format_summary_table <- function(df, matrix_game) {
  strategy_order <- base::unique(c(
    matrix_game$strategy$s1[[1]], matrix_game$strategy$s1[[2]], # Estratégias do primeiro jogador (P1)
    matrix_game$strategy$s2[[1]], matrix_game$strategy$s2[[2]]  # Estratégias do segundo jogador (P2)
  ))

  df |>
    tidyr::pivot_wider(names_from = type, values_from = media_prob) |>  # Expandir os tipos (EWA, RL, BL)
    dplyr::arrange(lambda, base::factor(estrategia_escolhida, levels = strategy_order, ordered = T)) |>
    dplyr::mutate(
      estrategia_escolhida = dplyr::case_when(
        estrategia_escolhida == matrix_game$strategy$s1[[1]] & jogador == matrix_game$player[[1]] ~
          base::paste0(matrix_game$strategy$s1[[1]], "/", matrix_game$strategy$s1[[2]], " (P1)"),
        estrategia_escolhida == matrix_game$strategy$s2[[1]] & jogador == matrix_game$player[[2]] ~
          base::paste0(matrix_game$strategy$s2[[1]], "/", matrix_game$strategy$s2[[2]], " (P2)"),
        T ~ estrategia_escolhida
      )
    ) |>
    dplyr::group_by(lambda, estrategia_escolhida) |>
    dplyr::summarise(
      EWA = base::paste(stats::na.omit(EWA), collapse = "/"),
      RL  = base::paste(stats::na.omit(RL), collapse = "/"),
      BL  = base::paste(stats::na.omit(BL), collapse = "/"),
      .groups = "drop"
    ) |>
    dplyr::mutate(ordem = dplyr::if_else(base::grepl("\\(P1\\)", estrategia_escolhida), 1, 2)) |>
    dplyr::arrange(lambda, base::factor(estrategia_escolhida, levels = strategy_order, ordered = T), ordem) |>
    dplyr::select(-ordem)
}

# Função para calcular tabelas de frequência e gerar gráficos
contingency_plots <- function(data_m1, data_m2, matrix_m1, matrix_m2, tag_label_m1, tag_label_m2) {

  # Definir os níveis das estratégias de cada jogo corretamente acessando dentro de `strategy`
  strategy_levels_m1 <- c(
    base::paste0(matrix_m1$strategy$s1, " (P1)"),
    base::paste0(matrix_m1$strategy$s2, " (P2)")
  )  # Estratégias do jogo 1 (ex: BSG)

  strategy_levels_m2 <- c(
    base::paste0(matrix_m2$strategy$s1, " (P1)"),
    base::paste0(matrix_m2$strategy$s2, " (P2)")
  )  # Estratégias do jogo 2 (ex: MEG)

  # Criar tabela de frequências para o primeiro jogo (ex: BSG)
  tab_m1 <- data_m1 |>
    dplyr::mutate(estrategia_escolhida = dplyr::case_when(
      estrategia_escolhida %in% matrix_m1$strategy$s1 & jogador == matrix_m1$player[1] ~ base::paste0(estrategia_escolhida, " (P1)"),
      estrategia_escolhida %in% matrix_m1$strategy$s2 & jogador == matrix_m1$player[2] ~ base::paste0(estrategia_escolhida, " (P2)"),
      T ~ estrategia_escolhida
    )) |>
    base::with(base::table(type, estrategia_escolhida)) |>
    (\(x) base::round(base::prop.table(x, margin = 2) * 100, 1))() |> # Total das linhas = 100%
    tibble::as_tibble() |>
    dplyr::mutate(
      estrategia_escolhida = base::factor(estrategia_escolhida, levels = strategy_levels_m1),
      type = base::factor(type, levels = c("EWA", "RL", "BL"))
    )

  # Criar tabela de frequências para o segundo jogo (ex: MEG)
  tab_m2 <- data_m2 |>
    dplyr::mutate(estrategia_escolhida = dplyr::case_when(
      estrategia_escolhida %in% matrix_m2$strategy$s1 & jogador == matrix_m2$player[1] ~ base::paste0(estrategia_escolhida, " (P1)"),
      estrategia_escolhida %in% matrix_m2$strategy$s2 & jogador == matrix_m2$player[2] ~ base::paste0(estrategia_escolhida, " (P2)"),
      T ~ estrategia_escolhida
    )) |>
    base::with(base::table(type, estrategia_escolhida)) |>
    (\(x) base::round(base::prop.table(x, margin = 2) * 100, 1))() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      estrategia_escolhida = base::factor(estrategia_escolhida, levels = strategy_levels_m2),
      type = base::factor(type, levels = c("EWA", "RL", "BL"))
    )

  # Criar gráfico para o primeiro jogo
  p_a <- tab_m1 |>
    dplyr::group_by(type) |>
    ggplot2::ggplot(ggplot2::aes(x = estrategia_escolhida, y = n, fill = type)) +
    ggplot2::geom_bar(stat = "identity", col = '#485B6D',
                      linewidth = .8, width = .5) +  # Barras empilhadas normalizadas
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1),
                                breaks = scales::breaks_pretty(n = 8)) +  # Exibir eixo Y em porcentagem
    paletteer::scale_fill_paletteer_d("MetBrewer::Egypt") +
    ggplot2::labs(x = "Estratégias",
                  y = "Frequência Relativa",
                  fill = "Modelo",
                  tag = tag_label_m1) +
    beautyxtrar::theme_xtra(base_family = "Times New Roman", base_size = 14) +
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 18, margin = ggplot2::margin(t = 10)),
                   plot.tag.position = "bottom")

  # Criar gráfico para o segundo jogo
  p_b <- tab_m2 |>
    dplyr::group_by(type) |>
    ggplot2::ggplot(ggplot2::aes(x = estrategia_escolhida, y = n, fill = type)) +
    ggplot2::geom_bar(stat = "identity", col = '#485B6D',
                      linewidth = .8, width = .5) +  # Barras empilhadas normalizadas
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1),
                                breaks = scales::breaks_pretty(n = 8)) +  # Exibir eixo Y em porcentagem
    paletteer::scale_fill_paletteer_d("MetBrewer::Egypt") +
    ggplot2::labs(x = "Estratégias",
                  y = NULL,
                  fill = "Modelo",
                  tag = tag_label_m2) +
    beautyxtrar::theme_xtra(base_family = "Times New Roman", base_size = 14) +
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 18, margin = ggplot2::margin(t = 10)),
                   plot.tag.position = "bottom")

  # Combinar gráficos
  combined_plot <- patchwork::guide_area() + (p_a + p_b) +
    patchwork::plot_layout(guides = "collect",
                           axis_titles = "collect",
                           nrow = 2,
                           heights = c(1, 10)) +
    ggplot2::theme(legend.position = "top")

  # Retornar uma lista contendo as tabelas e os gráficos
  return(base::list(
    tables = base::list(tab_m1 = tab_m1, tab_m2 = tab_m2),
    plots = base::list(p_a = p_a, p_b = p_b, combined_plot = combined_plot)
  ))
}

