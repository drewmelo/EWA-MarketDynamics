### ================== PLOT (NE) - ESTRATÉGIAS MISTAS  ========================

## GRÁFICOS --------------------------------------------------------------------

# BSG
p3 <- ggplot2::ggplot(s_matriz_bsg$br_plot$data, ggplot2::aes(col = player)) +
  ggplot2::geom_path(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Vendedor"),
    ggplot2::aes(x = xs, y = ys), linewidth = 3, lineend = "round", linejoin = "mitre"
  ) +
  ggplot2::geom_path(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Vendedor"),
    ggplot2::aes(x = xs, y = ye), linewidth = 3, lineend = "round", linejoin = "mitre"
  ) +
  ggplot2::geom_line(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Comprador"),
    ggplot2::aes(x = xe, y = ye), linewidth = 1.2
  ) +
  ggplot2::geom_line(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Comprador"),
    ggplot2::aes(x = xs, y = ys), linewidth = 1.2
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_comma(big.mark = ".",
                                                           decimal.mark = ",")) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark = ".",
                                                           decimal.mark = ",")) +
  paletteer::scale_colour_paletteer_d("MetBrewer::Egypt") +
  ggplot2::labs(
    x = "Probabilidade do Vendedor Escolher Preço Alto (q)",
    y = "Probabilidade do Comprador Escolher Aceitar (p)",
    col = NULL
  ) +
  theme_xtra(base_family = "Times New Roman", base_size = 18)

# MEG
p4 <- ggplot2::ggplot(s_matriz_meg$br_plot$data, ggplot2::aes(col = player)) +
  ggplot2::geom_path(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa A"),
    ggplot2::aes(x = xe, y = ye), linewidth = 3, lineend = "round", linejoin = "mitre"
  ) +
  ggplot2::geom_path(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa A"),
    ggplot2::aes(x = xs, y = ys), linewidth = 3, lineend = "round", linejoin = "mitre"
  ) +
  ggplot2::geom_line(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa B"),
    ggplot2::aes(x = xe, y = ye), linewidth = 1.2
  ) +
  ggplot2::geom_line(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa B"),
    ggplot2::aes(x = xs, y = ys), linewidth = 1.2
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_comma(big.mark = ".",
                                                           decimal.mark = ",")) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark = ".",
                                                           decimal.mark = ",")) +
  paletteer::scale_colour_paletteer_d("MetBrewer::Egypt") +
  ggplot2::labs(
    x = "Probabilidade da Empresa A Não Entrar (q)",
    y = "Probabilidade da Empresa B Não Entrar (p)",
    col = NULL
  ) +
  theme_xtra(base_family = "Times New Roman", base_size = 18)


## EXPORTAÇÃO DAS FIGURAS ------------------------------------------------------

# Figura 3 (BSG)
ggplot2::ggsave(
  plot = p3, filename = "figuras/figura_3.pdf",
  width = 10.81, height = 7.75, units = "in", device = cairo_pdf
)

# Figura 4 (MEG)
ggplot2::ggsave(
  plot = p4, filename = "figuras/figura_4.pdf",
  width = 10.81, height = 7.75, units = "in", device = cairo_pdf
)
