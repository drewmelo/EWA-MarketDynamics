# Função para criar histograma de probabilidades com medianas
plot_histogram <- function(data, file_name,
                           x_label = "Probabilidade",
                           y_label = "Densidade",
                           title = NULL,
                           bins = 30,
                           width = 12.13,
                           height = 9.02) {

  # Filtrar dados com a estratégia escolhida
  data_filtered <- data |>
    filter(estrategia_escolhida == estrategias)

  # Criar o histograma
  p <- ggplot(data_filtered, aes(x = probabilidade, color = type)) +
    geom_histogram(aes(fill = type), alpha = 0.5, position = "identity",
                   col = '#485B6D', linewidth = 0.3, bins = bins) +
    geom_vline(data = data_filtered |>
                 group_by(type, sim_id) |>
                 mutate(mediana = median(probabilidade)),
               aes(xintercept = mediana, color = type),
               linetype = "dashed") +
    facet_wrap(~ lambda, nrow = 2) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                       labels = scales::label_comma(big.mark = ".",
                                                    decimal.mark = ",")) +
    scale_x_continuous(labels = scales::label_comma(decimal.mark = ",", big.mark = ".")) +
    paletteer::scale_fill_paletteer_d("MetBrewer::Egypt") +
    paletteer::scale_colour_paletteer_d("MetBrewer::Egypt") +
    labs(x = x_label, y = y_label, fill = "Mediana", col = "Mediana", title = title) +
    beautyxtrar::theme_xtra(base_family = "Times New Roman", base_size = 14)

  # Salvar o gráfico como PDF
  ggsave(plot = p, filename = file_name,
         width = width, height = height, units = "in",
         device = cairo_pdf)

  return(p)  # Retorna o gráfico para visualização se necessário
}

# Função para gerar gráficos de correlação entre atração e probabilidade
plot_correlation <- function(data_m1, data_m2,
                             file_name_combined = "figuras/correlacao_combinada.pdf",
                             file_name_m1 = "figuras/correlacao_m1.pdf",
                             file_name_m2 = "figuras/correlacao_m2.pdf",
                             x_label = "Valores de λ",
                             y_label_m1 = "Correlação [Atração, Probabilidade]",
                             y_label_m2 = NULL,
                             tag_label_m1 = "(a) BSG",
                             tag_label_m2 = "(b) MEG",
                             width = 10.81,
                             height = 7.75) {

  # Transformar `lambda` em fator e calcular correlação para o primeiro conjunto de dados (ex: BSG)
  df_m1 <- data_m1 |>
    filter(estrategia_escolhida == estrategias) |>
    mutate(
      lambda = factor(sim_id, levels = 1:10,
                      labels = paste(str_replace(lambda_values, "\\.", ",")))
    ) |>
    group_by(type, lambda) |>
    reframe(correlacao = round(cor(atracao, probabilidade, use = "complete.obs"), 3))

  # Criar o gráfico do primeiro conjunto de dados
  p_m1 <- df_m1 |>
    ggplot(aes(lambda, correlacao, col = type, group = type)) +
    geom_line(linewidth = 0.9) +
    ggrepel::geom_text_repel(data = df_m1 |> filter(lambda == "0,1" | lambda == "1"),
                             aes(label = paste(str_replace(correlacao, "\\.", ","))),
                             family = "Times New Roman", size = 5.5, show.legend = F) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::label_comma(decimal.mark = ",", big.mark = ".")) +
    labs(x = x_label, y = y_label_m1, col = NULL, tag = tag_label_m1) +
    theme_academic(base_family = "Times New Roman", base_size = 18) +
    theme(
      plot.tag = element_text(size = 19, margin = margin(t = 10)),
      plot.tag.position = "bottom"
    )

  # Salvar gráfico individual de data_m1
  ggsave(plot = p_m1, filename = file_name_m1,
         width = width, height = height, units = "in",
         device = cairo_pdf)

  # Transformar `lambda` em fator e calcular correlação para o segundo conjunto de dados (ex: MEG)
  df_m2 <- data_m2 |>
    filter(estrategia_escolhida == estrategias) |>
    mutate(
      lambda = factor(sim_id, levels = 1:10,
                      labels = paste(str_replace(lambda_values, "\\.", ",")))
    ) |>
    group_by(type, lambda) |>
    reframe(correlacao = round(cor(atracao, probabilidade, use = "complete.obs"), 3))

  # Criar o gráfico do segundo conjunto de dados
  p_m2 <- df_m2 |>
    ggplot(aes(lambda, correlacao, col = type, group = type)) +
    geom_line(linewidth = 0.9) +
    ggrepel::geom_text_repel(data = df_m2 |> filter(lambda == "0,1" | lambda == "1"),
                             aes(label = paste(str_replace(correlacao, "\\.", ","))),
                             family = "Times New Roman", size = 5.5, show.legend = F) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::label_comma(decimal.mark = ",", big.mark = ".")) +
    labs(x = x_label, y = y_label_m2, col = NULL, tag = tag_label_m2) +
    theme_academic(base_family = "Times New Roman", base_size = 18) +
    theme(
      plot.tag = element_text(size = 19, margin = margin(t = 10)),
      plot.tag.position = "bottom"
    )

  # Salvar gráfico individual de data_m2
  ggsave(plot = p_m2, filename = file_name_m2,
         width = width, height = height, units = "in",
         device = cairo_pdf)

  # Criar o gráfico combinado automaticamente
  p_combined <- guide_area() + (p_m1 + p_m2) +
    plot_layout(guides = "collect",
                axis_titles = "collect",
                nrow = 2, heights = c(1, 10)) +
    theme(legend.position = "top") &
    scale_colour_paletteer_d("MetBrewer::Egypt")

  # Salvar o gráfico combinado como PDF
  ggsave(plot = p_combined, filename = file_name_combined,
         width = width, height = height, units = "in",
         device = cairo_pdf)

  return(list(
    individual_plots = list(p_m1 = p_m1, p_m2 = p_m2),
    combined_plot = p_combined
  ))  # Retorna os gráficos individuais e o combinado
}

