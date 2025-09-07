### ================== PLOT (NE) - ESTRATÉGIAS MISTAS  ========================

## GRÁFICOS --------------------------------------------------------------------

# BSG
p3 <- ggplot2::ggplot(
  s_matriz_bsg$br_plot$data,
  ggplot2::aes(col = factor(player, levels = c("Vendedor", "Comprador")))
) +
  # Linhas do vendedor
  ggplot2::geom_path(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Vendedor"),
    ggplot2::aes(x = xs, y = ys),
    linewidth = 3, lineend = "round", linejoin = "mitre",
    show.legend = F
  ) +
  ggplot2::geom_path(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Vendedor"),
    ggplot2::aes(x = xs, y = ye),
    linewidth = 3, lineend = "round", linejoin = "mitre",
    show.legend = F
  ) +

  # Linhas do comprador
  ggplot2::geom_line(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Comprador"),
    ggplot2::aes(x = xe, y = ye),
    linewidth = 1.2, linetype = "dotted",
    show.legend = F
  ) +
  ggplot2::geom_line(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Comprador"),
    ggplot2::aes(x = xs, y = ys),
    linewidth = 1.2, linetype = "dotted",
    show.legend = F
  ) +

  # Nome dos jogadores
  geom_text(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Comprador", xs == 0),
    aes(x = xs, y = ys, label = player, color = player),
    vjust = 2, hjust = -0.1, size = 8,
    family = fonte_base,
    show.legend = F, inherit.aes = F
  ) +
  geom_text(
    data = s_matriz_bsg$br_plot$data |> dplyr::filter(player == "Vendedor", xs == 0),
    aes(x = xs, y = ys, label = player, color = player),
    vjust = -1, hjust = -0.1, size = 8,
    family = fonte_base,
    show.legend = F, inherit.aes = F
  ) +

  # Texto explicativo do ponto de equilíbrio
  ggtext::geom_richtext(
    data = data.frame(
      xs = 0.639,
      ys = 0.03,
      label = "<b style='color: #566876;'>p = 0,649 (Aceitar)</b><br><b style='color: #0B86CA;'>q = 1 (Preço Alto)</b>"
    ),
    aes(x = xs, y = ys, label = label),
    fill = "white", label.color = NA,
    label.padding = unit(c(0.6, 0.6, 0.6, 0.6), "lines"),
    hjust = -0.1, vjust = 0,
    size = 7,
    inherit.aes = F,
    show.legend = F,
    family = fonte_base
  ) +

  # Escalas e tema
  ggplot2::scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  ggplot2::scale_color_manual(values = color_main) +
  ggplot2::labs(
    x = "Probabilidade do Vendedor Escolher Preço Alto (q)",
    y = "Probabilidade do Comprador Escolher Aceitar (p)",
    col = NULL
  ) +
  beautyxtrar::theme_xtra(base_family = fonte_base, base_size = 18)


# MEG
p4 <- ggplot2::ggplot(
  s_matriz_meg$br_plot$data,
  ggplot2::aes(col = player)
) +
  # Caminhos da Empresa A (linha principal)
  ggplot2::geom_path(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa A"),
    ggplot2::aes(x = xe, y = ye),
    linewidth = 3, lineend = "round", linejoin = "mitre",
    show.legend = F
  ) +
  ggplot2::geom_path(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa A"),
    ggplot2::aes(x = xs, y = ys),
    linewidth = 3, lineend = "round", linejoin = "mitre",
    show.legend = F
  ) +

  # Caminhos da Empresa B (linha pontilhada)
  ggplot2::geom_line(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa B"),
    ggplot2::aes(x = xe, y = ye),
    linewidth = 1.2, linetype = "dotted",
    show.legend = F
  ) +
  ggplot2::geom_line(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa B"),
    ggplot2::aes(x = xs, y = ys),
    linewidth = 1.2, linetype = "dotted",
    show.legend = F
  ) +

  # Nome da Empresa A (com negrito)
  geom_text(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa A"),
    aes(x = 0.77, y = 0.28, label = player, color = player),
    vjust = 2, hjust = -0.1, size = 8,
    show.legend = F, inherit.aes = F,
    family = fonte_base,
  ) +

  # Nome da Empresa B
  geom_text(
    data = s_matriz_meg$br_plot$data |> dplyr::filter(player == "Empresa B"),
    aes(x = 0.78, y = -0.01, label = player, color = player),
    vjust = -1, hjust = -0.1, size = 8,
    show.legend = F, inherit.aes = F,
    family = fonte_base
  ) +

  # Destaque do ponto de equilíbrio
  ggtext::geom_richtext(
    data = data.frame(
      xs = 0.16,
      ys = 0.18,
      label = "<b style='color: #566876;'>p = 0,1667 (Não Entrar)</b><br><b style='color: #0B86CA;'>q = 0,1667 (Não Entrar)</b>"
    ),
    aes(x = xs, y = ys, label = label),
    fill = "white", label.color = NA,
    label.padding = unit(c(0.6, 0.6, 0.6, 0.6), "lines"),
    hjust = -0.05, vjust = 0,
    size = 7,
    inherit.aes = F,
    show.legend = F,
    family = fonte_base
  ) +

  # Eixos e escalas
  ggplot2::scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  ggplot2::scale_color_manual(values = color_main) +

  # Rótulos dos eixos e tema
  ggplot2::labs(
    x = "Probabilidade da Empresa A Não Entrar (q)",
    y = "Probabilidade da Empresa B Não Entrar (p)",
    col = NULL
  ) +
  beautyxtrar::theme_xtra(base_family = fonte_base, base_size = 18)
