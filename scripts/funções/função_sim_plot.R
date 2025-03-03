# Criar função para gerar e combinar gráficos
sim_plot <- function(df_m1, df_m2, matrix_m1, matrix_m2, sim_id) {

  lambda_values <- seq(0.1, 1, by = 0.1)

  # Criar vetor de labels para λ (lambda) correspondente ao sim_id
  lambda_labels <- factor(sim_id, levels = 1:10, labels = paste0("λ = ", str_replace(lambda_values, "\\.", ",")))

  # Função para calcular proporções e padronizar a ordenação das estratégias
  calcular_proporcao <- function(df, matrix_game) {

    # Criar um vetor de ordenação baseado na posição original das estratégias, removendo duplicatas
    strategy_order <- unique(c(
      matrix_game$strategy$s1[[1]], matrix_game$strategy$s1[[2]], # Estratégias do primeiro jogador (P1)
      matrix_game$strategy$s2[[1]], matrix_game$strategy$s2[[2]]  # Estratégias do segundo jogador (P2)
    ))

    df |>
      dplyr::mutate(escolhida = ifelse(estrategia_escolhida == estrategias, 1, 0)) |>
      dplyr::group_by(type, sim_id, jogador, periodo, estrategias) |>
      dplyr::reframe(
        count_escolhida = sum(escolhida),  # Conta quantas vezes a estratégia foi escolhida
        total_amostra = n()  # Conta o total de observações no grupo
      ) |>
      dplyr::mutate(
        prop = (count_escolhida / total_amostra),
        estrategias = factor(estrategias, levels = strategy_order, ordered = TRUE)  # Ordena com base na posição original
      )
  }

  # Calcular proporções e aplicar ordenação das estratégias para os dois jogos
  prop_m1 <- calcular_proporcao(df_m1, matrix_m1)
  prop_m2 <- calcular_proporcao(df_m2, matrix_m2)

  # Função auxiliar para gerar cada gráfico individualmente
  ploting <- function(data, type, tag_label, y_label = NULL) {
    data |>
      dplyr::filter(type == !!type, sim_id == !!sim_id) |>  # Filtra pelo modelo e sim_id
      ggplot(aes(x = periodo, y = prop, col = estrategias)) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1),
                         labels = label_comma(decimal.mark = ",", big.mark = ".")) +
      scale_x_continuous(breaks = breaks_pretty(n = 3)) +
      facet_wrap(~jogador) +
      labs(col = "Estratégias",
           x = "Período",
           y = y_label,  # Define se o primeiro gráfico da linha terá "Proporção"
           tag = tag_label) +
      theme_xtra(base_family = "Times New Roman", base_size = 15) +
      theme(plot.tag = element_text(size = 18, margin = margin(t = 10)),
            plot.tag.position = "bottom") +
      guides(col = guide_legend(override.aes = list(linewidth = 1.5)))
  }

  # Criar gráficos para BSG e MEG usando a função auxiliar
  plots_m1 <- list(
    ploting(prop_m1, "EWA", paste0("(a) EWA – BSG (", lambda_labels, ")"), "Proporção"),
    ploting(prop_m1, "RL", paste0("(b) RL – BSG (", lambda_labels, ")")),
    ploting(prop_m1, "BL", paste0("(c) BL – BSG (", lambda_labels, ")"))
  )

  plots_m2 <- list(
    ploting(prop_m2, "EWA", paste0("(d) EWA – MEG (", lambda_labels, ")"), "Proporção"),
    ploting(prop_m2, "RL", paste0("(e) RL – MEG (", lambda_labels, ")")),
    ploting(prop_m2, "BL", paste0("(f) BL – MEG (", lambda_labels, ")"))
  )

  # Combinação dos gráficos usando patchwork
  s_m1 <- wrap_plots(plots_m1, nrow = 1, guides = "collect") &
    theme(legend.position = "top") &
    paletteer::scale_colour_paletteer_d("MetBrewer::Egypt")

  s_m2 <- wrap_plots(plots_m2, nrow = 1, guides = "collect") &
    theme(legend.position = "top") &
    paletteer::scale_colour_paletteer_d("MetBrewer::Egypt")

  # Layout final (BSG em cima, MEG embaixo)
  p_s <- s_m1 / s_m2

  return(p_s)  # Retorna o gráfico combinado
}
