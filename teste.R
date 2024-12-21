prop_meg <- meg_df |>
  mutate(escolhida = ifelse(estrategia_escolhida == estrategias, 1, 0)) |>
  group_by(type, sim_id, jogador, periodo, estrategias) |>
  reframe(
    count_escolhida = sum(escolhida),
    total_amostra = n()
  ) |>
  mutate(prop = (count_escolhida / total_amostra))

prop_bsg <- bsg_df |>
  mutate(escolhida = ifelse(estrategia_escolhida == estrategias, 1, 0)) |>
  group_by(type, sim_id, jogador, periodo, estrategias) |>
  reframe(
    count_escolhida = sum(escolhida),
    total_amostra = n()
  ) |>
  mutate(prop = (count_escolhida / total_amostra),
         estrategias = factor(estrategias, levels = c("Aceitar",
                                                      "Rejeitar",
                                                      "Preço Alto",
                                                      "Preço Baixo")))


# SIMULAÇÃO 1 -------------------------------------------------------------
# Configuração dos gráficos individuais
bsg_p1 <- prop_bsg |>
  filter(type == "ewa" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  #paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax") +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = "Proporção",
       tag = "(a) EWA – BSG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  theme(
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom"
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

bsg_p2 <- prop_bsg |>
  filter(type == "rl" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  #paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax") +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = NULL,
       tag = "(b) RL – BSG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  theme(
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom"
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

bsg_p3 <- prop_bsg |>
  filter(type == "bl" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  #paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax") +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = NULL,
       tag = "(c) BL – BSG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  theme(
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom"
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

meg_p1 <- prop_meg |>
  filter(type == "ewa" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  #paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax") +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = "Proporção",
       tag = "(d) EWA – MEG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  theme(
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom"
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

meg_p2 <- prop_meg |>
  filter(type == "rl" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  #paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax") +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = NULL,
       tag = "(e) RL – MEG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  theme(
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom"
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

meg_p3 <- prop_meg |>
  filter(type == "bl" & sim_id == 1) |>
  ggplot(aes(periodo, prop, col = estrategias)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  #paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax") +
  facet_wrap(~jogador) +
  labs(col = "Estratégias",
       y = NULL,
       tag = "(f) BL – MEG") +
  theme_xtra(base_family = "Times New Roman", base_size = 15) +
  theme(
    plot.tag = element_text(size = 18, margin = margin(t = 5)),
    plot.tag.position = "bottom"
  ) +
  guides(col = guide_legend(override.aes = list(linewidth = 1.5)))

# Combinação dos gráficos com legenda no topo
s1_bsg <- guide_area() + (bsg_p1 + bsg_p2 + bsg_p3) +
  plot_layout(guides = "collect",
              axis_titles = "collect",
              nrow = 2, heights = c(1,10)) +
  #plot_annotation(title = "main title") &
  theme(legend.position = "top") &
  paletteer::scale_color_paletteer_d("ggthemes::excel_Parallax")

s1_meg <- guide_area() + (meg_p1 + meg_p2 + meg_p3) +
  plot_layout(guides = "collect",
              axis_titles = "collect",
              nrow = 2, heights = c(1,10)) +
  #plot_annotation(title = "main title") &
  theme(legend.position = "top") &
  paletteer::scale_color_paletteer_d("ggthemes::excel_Marquee")

p_s1 <- s1_bsg / s1_meg

ggsave(plot = p_s1, filename = "figuras/teste.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)
