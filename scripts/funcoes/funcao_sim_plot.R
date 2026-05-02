### ================ FUNÇÃO PARA PLOTAGEM DE SIMULAÇÕES =======================

sim_plot <- function(df_m1, df_m2, matrix_m1, matrix_m2, sim_id) {

  lambda_values <- base::seq(0.1, 1, by = 0.1)

  # Criar vetor de labels para λ (lambda) correspondente ao sim_id
  lambda_labels <- base::factor(
    sim_id,
    levels = 1:10,
    labels = base::paste0(
      "λ = ", stringr::str_replace(lambda_values, "\\.", ",")
    )
  )

  # Função para calcular proporções e padronizar a ordenação das estratégias
  calcular_proporcao <- function(df, matrix_game) {

    # Criar um vetor de ordenação baseado na posição original das estratégias
    strategy_order <- base::unique(c(
      matrix_game$strategy$s1[[1]], matrix_game$strategy$s1[[2]],
      matrix_game$strategy$s2[[1]], matrix_game$strategy$s2[[2]]
    ))

    df |>
      dplyr::mutate(
        escolhida = dplyr::if_else(estrategia_escolhida == estrategias, 1, 0)
      ) |>
      dplyr::group_by(type, sim_id, jogador, periodo, estrategias) |>
      dplyr::reframe(
        count_escolhida = base::sum(escolhida),
        total_amostra = dplyr::n()
      ) |>
      dplyr::mutate(
        prop = count_escolhida / total_amostra,
        estrategias = base::factor(
          estrategias,
          levels = strategy_order,
          ordered = TRUE
        )
      )
  }

  # Calcular proporções
  prop_m1 <- calcular_proporcao(df_m1, matrix_m1)
  prop_m2 <- calcular_proporcao(df_m2, matrix_m2)

  # Função auxiliar para gerar cada gráfico individualmente
  ploting <- function(data, type, tag_label, y_label = NULL) {
    data |>
      dplyr::filter(type == !!type, sim_id == !!sim_id) |>
      ggplot2::ggplot(ggplot2::aes(x = periodo, y = prop, col = estrategias)) +
      ggplot2::geom_line() +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        labels = scales::label_comma(decimal.mark = ",", big.mark = ".")
      ) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
      ggplot2::facet_wrap(~jogador) +
      ggplot2::labs(
        col = "Estratégias",
        x = "Período",
        y = y_label,
        tag = tag_label
      ) +
      beautyxtrar::theme_xtra(base_family = fonte_base, base_size = 15) +
      ggplot2::theme(
        plot.tag = ggplot2::element_text(
          size = 18,
          margin = ggplot2::margin(t = 10)
        ),
        plot.tag.position = "bottom"
      ) +
      ggplot2::guides(
        col = ggplot2::guide_legend(
          override.aes = list(linewidth = 1.5)
        )
      )
  }

  # Criar gráficos para BSG
  plots_m1 <- base::list(
    ploting(prop_m1, "EWA", base::paste0("(a) EWA – BSG (", lambda_labels, ")"), "Proporção"),
    ploting(prop_m1, "RL",  base::paste0("(b) RL – BSG (", lambda_labels, ")")),
    ploting(prop_m1, "BL",  base::paste0("(c) BL – BSG (", lambda_labels, ")"))
  )

  # Criar gráficos para MEG
  plots_m2 <- base::list(
    ploting(prop_m2, "EWA", base::paste0("(d) EWA – MEG (", lambda_labels, ")"), "Proporção"),
    ploting(prop_m2, "RL",  base::paste0("(e) RL – MEG (", lambda_labels, ")")),
    ploting(prop_m2, "BL",  base::paste0("(f) BL – MEG (", lambda_labels, ")"))
  )

  # Combinação dos gráficos usando patchwork
  s_m1 <- patchwork::wrap_plots(plots_m1, nrow = 1, guides = "collect") &
    ggplot2::theme(legend.position = "top") &
    ggplot2::scale_color_manual(values = color_main)

  s_m2 <- patchwork::wrap_plots(plots_m2, nrow = 1, guides = "collect") &
    ggplot2::theme(legend.position = "top") &
    ggplot2::scale_color_manual(values = color_main)

  # Layout final
  p_s <- s_m1 / s_m2

  # Retornar tudo em lista
  return(list(
  plot = p_s,
  dados_m1 = dplyr::filter(prop_m1, sim_id == !!sim_id),
  dados_m2 = dplyr::filter(prop_m2, sim_id == !!sim_id)
  ))
}
