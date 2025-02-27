### ==================== SIMULAÇÃO 10 (LAMBDA = 1) ===========================

# Gráficos para BSG
bsg_p1 <- prop_bsg |>
  dplyr::filter(type == "EWA" & sim_id == 10) |>
  ggplot2::ggplot(ggplot2::aes(periodo, prop, col = estrategias)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(limits = c(0, 1),
                              labels = scales::label_comma(decimal.mark = ",",
                                                           big.mark = ".")) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::facet_wrap(~jogador) +
  ggplot2::labs(col = "Estratégias",
                x = "Período",
                y = "Proporção",
                tag = "(a) EWA – BSG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 18,
                                                  margin = ggplot2::margin(t = 10)),
                 plot.tag.position = "bottom") +
  ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

bsg_p2 <- prop_bsg |>
  dplyr::filter(type == "RL" & sim_id == 10) |>
  ggplot2::ggplot(ggplot2::aes(periodo, prop, col = estrategias)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(limits = c(0, 1),
                              labels = scales::label_comma(decimal.mark = ",",
                                                           big.mark = ".")) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::facet_wrap(~jogador) +
  ggplot2::labs(col = "Estratégias",
                x = "Período",
                y = NULL,
                tag = "(b) RL – BSG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 18,
                                                  margin = ggplot2::margin(t = 10)),
                 plot.tag.position = "bottom") +
  ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

bsg_p3 <- prop_bsg |>
  dplyr::filter(type == "BL" & sim_id == 10) |>
  ggplot2::ggplot(ggplot2::aes(periodo, prop, col = estrategias)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(limits = c(0, 1),
                              labels = scales::label_comma(decimal.mark = ",",
                                                           big.mark = ".")) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::facet_wrap(~jogador) +
  ggplot2::labs(col = "Estratégias",
                x = "Período",
                y = NULL,
                tag = "(c) BL – BSG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 18,
                                                  margin = ggplot2::margin(t = 10)),
                 plot.tag.position = "bottom") +
  ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

# Gráficos para MEG
meg_p1 <- prop_meg |>
  dplyr::filter(type == "EWA" & sim_id == 10) |>
  ggplot2::ggplot(ggplot2::aes(periodo, prop, col = estrategias)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(limits = c(0, 1),
                              labels = scales::label_comma(decimal.mark = ",",
                                                           big.mark = ".")) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::facet_wrap(~jogador) +
  ggplot2::labs(col = "Estratégias",
                x = "Período",
                y = "Proporção",
                tag = "(d) EWA – MEG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 18,
                                                  margin = ggplot2::margin(t = 10)),
                 plot.tag.position = "bottom") +
  ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

meg_p2 <- prop_meg |>
  dplyr::filter(type == "RL" & sim_id == 10) |>
  ggplot2::ggplot(ggplot2::aes(periodo, prop, col = estrategias)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(limits = c(0, 1),
                              labels = scales::label_comma(decimal.mark = ",",
                                                           big.mark = ".")) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::facet_wrap(~jogador) +
  ggplot2::labs(col = "Estratégias",
                x = "Período",
                y = NULL,
                tag = "(e) RL – MEG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 18,
                                                  margin = ggplot2::margin(t = 10)),
                 plot.tag.position = "bottom") +
  ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

meg_p3 <- prop_meg |>
  dplyr::filter(type == "BL" & sim_id == 10) |>
  ggplot2::ggplot(ggplot2::aes(periodo, prop, col = estrategias)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(limits = c(0, 1),
                              labels = scales::label_comma(decimal.mark = ",",
                                                           big.mark = ".")) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggplot2::facet_wrap(~jogador) +
  ggplot2::labs(col = "Estratégias",
                x = "Período",
                y = NULL,
                tag = "(f) BL – MEG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 18,
                                                  margin = ggplot2::margin(t = 10)),
                 plot.tag.position = "bottom") +
  ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

# Combinação dos gráficos
s10_bsg <- patchwork::guide_area() + (bsg_p1 + bsg_p2 + bsg_p3) +
  patchwork::plot_layout(guides = "collect",
                         axis_titles = "collect",
                         nrow = 2,
                         heights = c(1, 10)) +
  ggplot2::theme(legend.position = "top") &
  paletteer::scale_colour_paletteer_d("MetBrewer::Egypt")

s10_meg <- patchwork::guide_area() + (meg_p1 + meg_p2 + meg_p3) +
  patchwork::plot_layout(guides = "collect",
                         axis_titles = "collect",
                         nrow = 2,
                         heights = c(1, 10)) +
  ggplot2::theme(legend.position = "top") &
  paletteer::scale_colour_paletteer_d("MetBrewer::Egypt")

# Layout final e salvamento
p_s10 <- s10_bsg / s10_meg

# Removendo apenas objetos listados
obj_remov <- ls(pattern = "^(meg_p|bsg_p)")

rm(list = obj_remov)
