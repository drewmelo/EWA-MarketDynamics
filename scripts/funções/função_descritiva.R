### ================== FUNÇÃO PARA ANÁLISE DESCRITIVA =========================

## MEDIDAS-RESUMO  ------------------------------------------------------------
# Função para calcular medidas-resumo
summary_statistics <- function(df, sufixo) {
  df |>
    filter(estrategia_escolhida == estrategias) |>
    group_by(type, lambda) |>
    reframe(
      media = round(mean(atracao, na.rm = T), 1),
      mediana = round(median(atracao, na.rm = T), 1),
      dp = round(sd(atracao, na.rm = T), 1),
      min = round(min(atracao, 1)),
      max = round(max(atracao, 1)),
      cv = round(dp / media, 1)
    ) |>
    rename_with(~ paste0(.x, "_", sufixo), .cols = 3:8) # Adiciona sufixo personalizado
}

# Função para criar tabelas combinando BSG e MEG por tipo
summary_tables <- function(modelo, lista_resumo) {
  lista_resumo$mr_bsg |>
    filter(type == modelo) |>
    bind_cols(lista_resumo$mr_meg |> filter(type == modelo)) |>
    janitor::clean_names() |>
    select(-type_9, -lambda_10) |>
    rename(type = "type_1", lambda = "lambda_2")
}

# Função para calcular frequência e média de probabilidades
frequency_probabilities <- function(df) {
  df |>
    filter(estrategia_escolhida == estrategias) |>
    group_by(type, lambda, jogador, estrategia_escolhida) |>
    reframe(
      media_prob = round(mean(probabilidade), 3),
      cont_n = sum(n())
    ) |>
    group_by(type, lambda, jogador) |>
    mutate(
      prop_n = round((cont_n / sum(cont_n) * 100), 2) # Proporção em %
    ) |>
    ungroup() |>
    arrange(lambda) |>
    mutate(
      type = factor(type, levels = c("EWA", "RL", "BL")) # Ordenação correta de type
    )
}

# Criar função para formatar os dados corretamente
format_summary_table <- function(df, matrix_game) {

  # Criar um vetor de ordenação baseado na posição original das estratégias, removendo duplicatas
  strategy_order <- unique(c(
    matrix_game$strategy$s1[[1]], matrix_game$strategy$s1[[2]], # Estratégias do primeiro jogador (P1)
    matrix_game$strategy$s2[[1]], matrix_game$strategy$s2[[2]]  # Estratégias do segundo jogador (P2)
  ))

  df |>
    pivot_wider(names_from = type, values_from = media_prob) |>  # Expandir os tipos (EWA, RL, BL)
    arrange(lambda, factor(estrategia_escolhida, levels = strategy_order, ordered = TRUE)) |>  # Ordenar pela posição original das estratégias
    mutate(
      estrategia_escolhida = case_when(
        estrategia_escolhida == matrix_game$strategy$s1[[1]] & jogador == matrix_game$player[[1]] ~
          paste0(matrix_game$strategy$s1[[1]], "/", matrix_game$strategy$s1[[2]], " (P1)"),

        estrategia_escolhida == matrix_game$strategy$s1[[2]] & jogador == matrix_game$player[[1]] ~
          paste0(matrix_game$strategy$s1[[1]], "/", matrix_game$strategy$s1[[2]], " (P1)"),

        estrategia_escolhida == matrix_game$strategy$s2[[1]] & jogador == matrix_game$player[[2]] ~
          paste0(matrix_game$strategy$s2[[1]], "/", matrix_game$strategy$s2[[2]], " (P2)"),

        estrategia_escolhida == matrix_game$strategy$s2[[2]] & jogador == matrix_game$player[[2]] ~
          paste0(matrix_game$strategy$s2[[1]], "/", matrix_game$strategy$s2[[2]], " (P2)"),

        TRUE ~ estrategia_escolhida  # Mantém o valor original para outros casos
      )
    ) |>
    group_by(lambda, estrategia_escolhida) |>
    summarise(
      EWA = paste(na.omit(EWA), collapse = "/"),  # Remove os NA antes de concatenar
      RL  = paste(na.omit(RL), collapse = "/"),
      BL  = paste(na.omit(BL), collapse = "/"),
      .groups = "drop"
    ) |>
    mutate(ordem = ifelse(grepl("\\(P1\\)", estrategia_escolhida), 1, 2)) |>  # Criar coluna para ordenar P1 antes de P2
    arrange(lambda, factor(estrategia_escolhida, levels = strategy_order, ordered = TRUE), ordem) |>  # Ordenar com base na ordem original das estratégias
    select(-ordem)  # Remover a coluna auxiliar de ordenação
}

# Função para calcular tabelas de frequência e gerar gráficos
contingency_plots <- function(data_m1, data_m2, matrix_m1, matrix_m2, tag_label_m1, tag_label_m2) {

  # Definir os níveis das estratégias de cada jogo corretamente acessando dentro de `strategy`
  strategy_levels_m1 <- c(
    paste0(matrix_m1$strategy$s1, " (P1)"),
    paste0(matrix_m1$strategy$s2, " (P2)")
  )  # Estratégias do jogo 1 (ex: BSG)

  strategy_levels_m2 <- c(
    paste0(matrix_m2$strategy$s1, " (P1)"),
    paste0(matrix_m2$strategy$s2, " (P2)")
  )  # Estratégias do jogo 2 (ex: MEG)

  # Criar tabela de frequências para o primeiro jogo (ex: BSG)
  tab_m1 <- data_m1 |>
    mutate(estrategia_escolhida = case_when(
      estrategia_escolhida %in% matrix_m1$strategy$s1 & jogador == matrix_m1$player[1] ~ paste0(estrategia_escolhida, " (P1)"),
      estrategia_escolhida %in% matrix_m1$strategy$s2 & jogador == matrix_m1$player[2] ~ paste0(estrategia_escolhida, " (P2)"),
      TRUE ~ estrategia_escolhida
    )) |>
    with(table(type, estrategia_escolhida)) |>
    (\(x) round(prop.table(x, margin = 2) * 100, 1))() |> # Total das linhas = 100%
    as_tibble() |>
    mutate(
      estrategia_escolhida = factor(estrategia_escolhida, levels = strategy_levels_m1),
      type = factor(type, levels = c("EWA", "RL", "BL"))
    )

  # Criar tabela de frequências para o segundo jogo (ex: MEG)
  tab_m2 <- data_m2 |>
    mutate(estrategia_escolhida = case_when(
      estrategia_escolhida %in% matrix_m2$strategy$s1 & jogador == matrix_m2$player[1] ~ paste0(estrategia_escolhida, " (P1)"),
      estrategia_escolhida %in% matrix_m2$strategy$s2 & jogador == matrix_m2$player[2] ~ paste0(estrategia_escolhida, " (P2)"),
      TRUE ~ estrategia_escolhida
    )) |>
    with(table(type, estrategia_escolhida)) |>
    (\(x) round(prop.table(x, margin = 2) * 100, 1))() |>
    as_tibble() |>
    mutate(
      estrategia_escolhida = factor(estrategia_escolhida, levels = strategy_levels_m2),
      type = factor(type, levels = c("EWA", "RL", "BL"))
    )

  # Criar gráfico para o primeiro jogo
  p_a <- tab_m1 |>
    group_by(type) |>
    ggplot(aes(x = estrategia_escolhida, y = n, fill = type)) +
    geom_bar(stat = "identity", col = '#485B6D',
             linewidth = .8, width = .5) +  # Barras empilhadas normalizadas
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       breaks = scales::breaks_pretty(n = 8)) +  # Exibir eixo Y em porcentagem
    paletteer::scale_fill_paletteer_d("MetBrewer::Egypt") +
    labs(x = "Estratégias",
         y = "Frequência Relativa",
         fill = "Modelo",
         tag = tag_label_m1) +
    theme_xtra(base_family = "Times New Roman", base_size = 14) +
    theme(plot.tag = element_text(size = 18, margin = margin(t = 10)),
          plot.tag.position = "bottom")

  # Criar gráfico para o segundo jogo
  p_b <- tab_m2 |>
    group_by(type) |>
    ggplot(aes(x = estrategia_escolhida, y = n, fill = type)) +
    geom_bar(stat = "identity", col = '#485B6D',
             linewidth = .8, width = .5) +  # Barras empilhadas normalizadas
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       breaks = scales::breaks_pretty(n = 8)) +  # Exibir eixo Y em porcentagem
    paletteer::scale_fill_paletteer_d("MetBrewer::Egypt") +
    labs(x = "Estratégias",
         y = NULL,
         fill = "Modelo",
         tag = tag_label_m2) +
    theme_xtra(base_family = "Times New Roman", base_size = 14) +
    theme(plot.tag = element_text(size = 18, margin = margin(t = 10)),
          plot.tag.position = "bottom")

  # Combinar gráficos
  combined_plot <- guide_area() + (p_a + p_b) +
    plot_layout(guides = "collect",
                axis_titles = "collect",
                nrow = 2,
                heights = c(1, 10)) +
    theme(legend.position = "top")

  # Retornar uma lista contendo as tabelas e os gráficos
  return(list(
    tables = list(tab_m1 = tab_m1, tab_m2 = tab_m2),
    plots = list(p_a = p_a, p_b = p_b, combined_plot = combined_plot)
  ))
}
