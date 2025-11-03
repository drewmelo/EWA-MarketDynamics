### ================== FUNÇÃO PARA ANÁLISE DESCRITIVA =========================

## MEDIDAS-RESUMO  ------------------------------------------------------------
# Função para calcular medidas-resumo
summary_statistics <- function(df, sufixo) {
  df |>
    dplyr::filter(estrategia_escolhida == estrategias) |>
    dplyr::group_by(type, lambda) |>
    dplyr::reframe(
      media = base::round(base::mean(atracao, na.rm = T)),
      mediana = base::round(stats::median(atracao, na.rm = T)),
      dp = base::round(stats::sd(atracao, na.rm = T)),
      min = base::round(base::min(atracao)),
      max = base::round(base::max(atracao)),
      iqr = base::round(IQR(atracao))
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
avg_prob_table <- function(data, file,
                               p1_player, p1_pair,
                               p2_player, p2_pair,
                               p1_label = "P1", p2_label = "P2",
                               title = NULL,
                               footnote = "Fonte: Elaborado pelo autor (2024).",
                               font_size = 10) {

  stopifnot(requireNamespace("dplyr"),
            requireNamespace("tidyr"),
            requireNamespace("flextable"))

  fmt <- function(x) formatC(as.numeric(x), format = "f", digits = 3,
                             big.mark = ".", decimal.mark = ",")

  # P1 e P2 têm as mesmas estratégias? (caso MEG)
  s1 <- data |> dplyr::filter(jogador == p1_player) |> dplyr::distinct(estrategia_escolhida) |> dplyr::pull()
  s2 <- data |> dplyr::filter(jogador == p2_player) |> dplyr::distinct(estrategia_escolhida) |> dplyr::pull()
  same_strats <- setequal(trimws(s1), trimws(s2))

  # monta bloco (P1 ou P2)
  prep_block <- function(dat, player, pair_vec, which_tag) {
    block_clean <- paste0(pair_vec[1], "/", pair_vec[2])
    # nome interno único só quando as estratégias são iguais
    block_raw   <- if (same_strats) paste0(block_clean, "__", which_tag) else block_clean

    out <- dat |>
      dplyr::filter(jogador == player,
                    estrategia_escolhida %in% pair_vec) |>
      dplyr::select(lambda, type, estrategia_escolhida, media_prob) |>
      dplyr::group_by(lambda, type, estrategia_escolhida) |>
      dplyr::summarise(media_prob = mean(media_prob, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        id_cols    = c(lambda, type),
        names_from = estrategia_escolhida,
        values_from= media_prob
      ) |>
      dplyr::mutate(
        value = paste0(fmt(.data[[pair_vec[1]]]), "/", fmt(.data[[pair_vec[2]]])),
        block_raw   = block_raw,
        block_clean = block_clean,
        which       = which_tag
      ) |>
      dplyr::select(lambda, type, block_raw, block_clean, which, value)

    out
  }

  p1 <- prep_block(data, p1_player, p1_pair, "A")
  p2 <- prep_block(data, p2_player, p2_pair, "B")

  long <- dplyr::bind_rows(p1, p2)

  # 1 linha por λ com os dois blocos lado a lado
  wide <- long |>
    tidyr::pivot_wider(
      id_cols    = lambda,
      names_from = c(block_raw, type),
      values_from= value,
      names_sep  = "___",
      values_fn  = list(value = dplyr::first),
      values_fill= list(value = "")
    ) |>
    dplyr::arrange(lambda)

  # ordenar colunas
  blocks_raw <- unique(long$block_raw)
  ord <- c("lambda",
           as.vector(t(outer(blocks_raw, c("EWA","RL","BL"),
                             function(b, t) paste0(b, "___", t)))))
  ord <- ord[ord %in% names(wide)]
  wide <- wide[, ord]
  names(wide)[1] <- "Incremento"

  # meta p/ cabeçalho (usa 'which' para decidir P1/P2)
  block_meta <- long |>
    dplyr::distinct(block_raw, block_clean, which)

  top_labels <- c("Incremento")
  for (br in blocks_raw) {
    row <- block_meta[block_meta$block_raw == br, ]
    lab <- paste0(row$block_clean, " (", ifelse(row$which == "A", p1_label, p2_label), ")")
    top_labels <- c(top_labels, rep(lab, 3))
  }
  bot_labels <- c("λ", rep(c("EWA","RL","BL"), length(blocks_raw)))

  header_map <- data.frame(
    keys = colnames(wide),
    topo = top_labels,
    base = bot_labels,
    check.names = FALSE
  )

  ft <- flextable::flextable(wide) |>
    flextable::set_header_df(mapping = header_map, key = "keys") |>
    flextable::merge_h(part = "header") |>
    flextable::theme_booktabs() |>
    flextable::align(align = "center", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::add_footer_lines(footnote) |>
    flextable::align(align = "center", part = "footer") |>
    flextable::autofit() |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::fontsize(size = font_size, part = "all")

  if (!is.null(title) && nzchar(title)) {
    ft <- flextable::add_header_lines(ft, values = title)
  }

  flextable::save_as_image(ft, path = file)
  message("✅ Tabela exportada: ", file)
}

# ---------------------------------------------------------------
# Função: tabelas absolutas, relativas (linha=100%), "Total" (% global)
#         versões long (para plot) e gráficos empilhados por type.
# ---------------------------------------------------------------
contingency_plots <- function(data_m1, data_m2, matrix_m1, matrix_m2, tag_label_m1, tag_label_m2) {

  # 1) Levels das estratégias (P1/P2) para ordenar
  strategy_levels_m1 <- c(
    paste0(matrix_m1$strategy$s1, " (P1)"),
    paste0(matrix_m1$strategy$s2, " (P2)")
  )
  strategy_levels_m2 <- c(
    paste0(matrix_m2$strategy$s1, " (P1)"),
    paste0(matrix_m2$strategy$s2, " (P2)")
  )

  # ---------------------- JOGO 1 (m1) ----------------------
  # 2) Tabela absoluta (observados)
  tab_abs_m1 <- data_m1 |>
    dplyr::mutate(
      estrategia_escolhida = dplyr::case_when(
        estrategia_escolhida %in% matrix_m1$strategy$s1 & jogador == matrix_m1$player[1] ~ paste0(estrategia_escolhida, " (P1)"),
        estrategia_escolhida %in% matrix_m1$strategy$s2 & jogador == matrix_m1$player[2] ~ paste0(estrategia_escolhida, " (P2)"),
        TRUE ~ estrategia_escolhida
      )
    ) |>
    (\(d) base::table(d$type, d$estrategia_escolhida))()

  # 3) Tabela relativa por linha (linha = 100%)
  tab_rel_m1 <- round(prop.table(tab_abs_m1, margin = 1) * 100, 1)

  # 4) Linha "Total" = distribuição global por coluna (em %)
  total_m1 <- round((colSums(tab_abs_m1) / sum(tab_abs_m1)) * 100, 1)

  # 5) Anexa "Total" à tabela relativa (ordem EWA, RL, BL, Total)
  tab_rel_total_m1 <- rbind(tab_rel_m1, Total = total_m1)

  # 6) Versão tidy/long para plot
  tab_m1_long <- tab_rel_total_m1 |>
    tibble::as_tibble(rownames = "type") |>
    tidyr::pivot_longer(
      cols = -type,
      names_to = "estrategia_escolhida",
      values_to = "freq"
    ) |>
    dplyr::mutate(
      estrategia_escolhida = factor(estrategia_escolhida, levels = strategy_levels_m1),
      type = factor(type, levels = c("EWA", "RL", "BL", "Total"))
    )

  # 7) Gráfico empilhado por type (m1)
  p_a <- tab_m1_long |>
    dplyr::group_by(type) |>
    ggplot2::ggplot(ggplot2::aes(x = type, y = freq, fill = estrategia_escolhida)) +
    ggplot2::geom_col(position = "stack", col = "#485B6D", linewidth = .8, width = .5) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      breaks = scales::breaks_pretty(n = 8),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(values = color_main) +
    ggplot2::labs(
      x = "Estratégias",
      y = "Frequência Relativa",
      fill = NULL,
      tag = tag_label_m1
    ) +
    beautyxtrar::theme_xtra(base_family = fonte_base, base_size = 18) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      legend.text = ggplot2::element_text(size = 14),
      legend.margin = ggplot2::margin(b = 5),
      plot.tag.position = "bottom"
    )

  # ---------------------- JOGO 2 (m2) — mesma lógica ----------------------
  tab_abs_m2 <- data_m2 |>
    dplyr::mutate(
      estrategia_escolhida = dplyr::case_when(
        estrategia_escolhida %in% matrix_m2$strategy$s1 & jogador == matrix_m2$player[1] ~ paste0(estrategia_escolhida, " (P1)"),
        estrategia_escolhida %in% matrix_m2$strategy$s2 & jogador == matrix_m2$player[2] ~ paste0(estrategia_escolhida, " (P2)"),
        TRUE ~ estrategia_escolhida
      )
    ) |>
    (\(d) base::table(d$type, d$estrategia_escolhida))()

  tab_rel_m2 <- round(prop.table(tab_abs_m2, margin = 1) * 100, 1)
  total_m2   <- round((colSums(tab_abs_m2) / sum(tab_abs_m2)) * 100, 1)
  tab_rel_total_m2 <- rbind(tab_rel_m2, Total = total_m2)

  tab_m2_long <- tab_rel_total_m2 |>
    tibble::as_tibble(rownames = "type") |>
    tidyr::pivot_longer(
      cols = -type,
      names_to = "estrategia_escolhida",
      values_to = "freq"
    ) |>
    dplyr::mutate(
      estrategia_escolhida = factor(estrategia_escolhida, levels = strategy_levels_m2),
      type = factor(type, levels = c("EWA", "RL", "BL", "Total"))
    )

  p_b <- tab_m2_long |>
    dplyr::group_by(type) |>
    ggplot2::ggplot(ggplot2::aes(x = type, y = freq, fill = estrategia_escolhida)) +
    ggplot2::geom_col(position = "stack", col = "#485B6D", linewidth = .8, width = .5) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      breaks = scales::breaks_pretty(n = 8),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(values = color_main) +
    ggplot2::labs(
      x = "Estratégias",
      y = NULL,
      fill = NULL,
      tag = tag_label_m2
    ) +
    beautyxtrar::theme_xtra(base_family = fonte_base, base_size = 18) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      legend.text = ggplot2::element_text(size = 14),
      legend.margin = ggplot2::margin(b = 5),
      plot.tag.position = "bottom"
    )

  # 8) Combinar plots
  combined_plot <- patchwork::guide_area() + (p_a + p_b) +
    patchwork::plot_layout(
      axis_titles = "collect",
      nrow = 2,
      heights = c(1, 10)
    ) +
    ggplot2::theme(legend.position = "top")

  # 9) Retorno: tudo organizado
  base::list(
    tables = base::list(
      # absolutos
      tab_abs_m1 = tab_abs_m1,
      tab_abs_m2 = tab_abs_m2,
      # relativos por linha
      tab_rel_m1 = tab_rel_m1,
      tab_rel_m2 = tab_rel_m2,
      # relativos + linha "Total"
      tab_rel_total_m1 = tab_rel_total_m1,
      tab_rel_total_m2 = tab_rel_total_m2,
      # versões long para plot
      tab_m1_long = tab_m1_long,
      tab_m2_long = tab_m2_long
    ),
    plots = base::list(
      p_a = p_a,
      p_b = p_b,
      combined_plot = combined_plot
    )
  )
}

# deps: install.packages(c("flextable","dplyr"))
table_wide <- function(prob_bsg, prob_meg,
                                    file      = "tabelas/tabela_8_bsg_meg.png",
                                    title     = NULL,
                                    footnote  = "Fonte: Elaborado pelo autor (2024).",
                                    digits    = 1,
                                    font_size = 10) {
  stopifnot(requireNamespace("flextable"),
            requireNamespace("dplyr"))

  # --- helpers ---------------------------------------------------------------
  to_df <- function(x) {
    df <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(rownames(df))) {
      df <- dplyr::tibble(Modelo = rownames(df), df, .name_repair = "minimal")
      rownames(df) <- NULL
    } else if (!"Modelo" %in% names(df)) {
      stop("A tabela precisa ter rownames (EWA, RL, BL, Total) ou coluna 'Modelo'.")
    }
    # força numérico nas métricas
    num_cols <- setdiff(names(df), "Modelo")
    df[num_cols] <- lapply(df[num_cols], function(v) suppressWarnings(as.numeric(v)))
    df
  }

  df_bsg <- to_df(prob_bsg)
  df_meg <- to_df(prob_meg)

  # ordena linhas (se existirem estes nomes)
  ord_rows <- c("EWA","RL","BL","Total")
  df_bsg <- dplyr::arrange(df_bsg, factor(Modelo, levels = ord_rows))
  df_meg <- dplyr::arrange(df_meg, factor(Modelo, levels = ord_rows))

  bsg_cols <- setdiff(names(df_bsg), "Modelo")
  meg_cols <- setdiff(names(df_meg), "Modelo")

  # junta lado a lado (mesmas linhas/ordem por 'Modelo')
  df <- df_bsg |>
    dplyr::full_join(df_meg, by = "Modelo")

  # --- flextable com cabeçalho em 2 níveis -----------------------------------
  keys   <- names(df)
  top    <- c(" ", rep("BSG", length(bsg_cols)), rep("MEG", length(meg_cols)))
  bottom <- c("Modelo", bsg_cols, meg_cols)

  header_map <- data.frame(
    keys = keys, top = top, bottom = bottom,
    check.names = FALSE
  )

  ft <- flextable::flextable(df) |>
    flextable::set_header_df(mapping = header_map, key = "keys") |>
    flextable::merge_h(part = "header") |>
    flextable::theme_booktabs() |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::colformat_num(
      j = which(sapply(df, is.numeric)),
      digits = digits, decimal.mark = ",", big.mark = "."
    ) |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::autofit()

  if (!is.null(title) && nzchar(title)) {
    ft <- flextable::add_header_lines(ft, values = title)
  }

  if (!is.null(footnote) && nzchar(footnote)) {
    ft <- flextable::add_footer_lines(ft, values = footnote) |>
      flextable::align(align = "center", part = "footer")
  }

  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  flextable::save_as_image(ft, path = file)
  message("✅ Tabela exportada para PNG: ", normalizePath(file, mustWork = FALSE))

  invisible(ft)
}
