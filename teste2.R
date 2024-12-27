prop_meg <- meg_df |>
  mutate(escolhida = ifelse(estrategia_escolhida == estrategias, 1, 0)) |>
  group_by(type, sim_id, jogador, periodo, estrategias) |>
  reframe(
    count_escolhida = sum(escolhida),
    total_amostra = n()
  ) |>
  mutate(prop = (count_escolhida / total_amostra))

# Configuração dos gráficos individuais
meg_p1 <- prop_meg |>
  filter(type == "ewa" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = "Proporção",
       tag = "(a) EWA") +
  theme_xtra(base_family = "Times New Roman") +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom",
    panel.grid.major = ggplot2::element_line(color = "gray90",
                                             linewidth = .5),
    panel.grid.minor = ggplot2::element_line(color = "gray90",
                                             linewidth = 0.25,
                                             linetype = "dashed")
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

meg_p2 <- prop_meg |>
  filter(type == "rl" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = "Proporção",
       tag = "(b) RL") +
  theme_xtra(base_family = "Times New Roman") +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom",
    panel.grid.major = ggplot2::element_line(color = "gray90",
                                             linewidth = .5),
    panel.grid.minor = ggplot2::element_line(color = "gray90",
                                             linewidth = 0.25,
                                             linetype = "dashed")
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

meg_p3 <- prop_meg |>
  filter(type == "bl" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = "Proporção",
       tag = "(c) BL") +
  theme_xtra(base_family = "Times New Roman") +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom",
    panel.grid.major = ggplot2::element_line(color = "gray90",
                                             linewidth = .5),
    panel.grid.minor = ggplot2::element_line(color = "gray90",
                                             linewidth = 0.25,
                                             linetype = "dashed")
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

# Combinação dos gráficos com legenda no topo
guide_area() + (meg_p1 + meg_p2 + meg_p3) +
  plot_layout(guides = "collect",
              axis_titles = "collect",
              nrow = 2, heights = c(0.4,10)) +
  #plot_annotation(title = "main title") &
  theme(legend.position = "top")




