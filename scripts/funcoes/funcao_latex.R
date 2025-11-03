model_stats_tabletex <- function(data,
                                 modelo    = "EWA",
                                 caption   = NULL,
                                 label     = "tabela_stats",
                                 footnote  = "Fonte: Elaborado pelo autor (2024).",
                                 font_size = 10,
                                 file_tex  = NULL) {
  # ---- 1) preparar dados ----
  df <- data |>
    dplyr::select(
      lambda,
      media_m1,  mediana_m1,  dp_m1,  min_m1,  max_m1,  iqr_m1,
      media_m2,  mediana_m2,  dp_m2,  min_m2,  max_m2,  iqr_m2
    ) |>
    dplyr::rename(
      media_bsg = media_m1,  md_bsg = mediana_m1, dp_bsg = dp_m1,
      min_bsg   = min_m1,    max_bsg= max_m1,     iqr_bsg = iqr_m1,
      media_meg = media_m2,  md_meg = mediana_m2, dp_meg = dp_m2,
      min_meg   = min_m2,    max_meg= max_m2,     iqr_meg = iqr_m2
    )

  # formatador numérico PT-BR sem casas decimais
  fmt <- function(x) {
    if (is.numeric(x)) {
      formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
    } else {
      as.character(x)
    }
  }
  df[] <- lapply(df, fmt)

  # transforma "λ =  0,1" -> "$\\lambda = 0,1$"
  to_lambda_tex <- function(s) {
    s <- as.character(s)
    val <- sub(".*=\\s*", "", s)
    paste0("$\\lambda = ", val, "$")
  }
  lambdas <- to_lambda_tex(df$lambda)

  # monta as células por linha
  row_cells <- function(i) {
    c(
      lambdas[i],
      df$media_bsg[i], df$md_bsg[i], df$dp_bsg[i], df$min_bsg[i], df$max_bsg[i], df$iqr_bsg[i],
      df$media_meg[i], df$md_meg[i], df$dp_meg[i], df$min_meg[i], df$max_meg[i], df$iqr_meg[i]
    )
  }

  # caption default
  if (is.null(caption) || !nzchar(caption)) {
    caption <- sprintf("Estatísticas do Modelo %s para BSG e MEG", modelo)
  }

  # ---- 2) construir LaTeX ----
  header_top <- paste0(
    "\t\t\\Xhline{2\\arrayrulewidth}\n",
    "\t\t& \\multicolumn{6}{c|}{BSG} & \\multicolumn{6}{c}{MEG} \\\\ \\hline\n",
    "\t\t\\multicolumn{1}{c|}{", modelo, "} & Média & Md & DP & Min & Máx & IQR & ",
    "Média & Md & DP & Min & Máx & IQR \\\\ \\hline\n"
  )

  # linhas da tabela
  lines <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    cells <- row_cells(i)
    lines[i] <- paste0("\t\t", paste(cells, collapse = " & "), " \\\\")
  }
  body <- paste(lines, collapse = "\n")

  # estrutura completa
  out <- paste0(
"\\begin{tabela}[H]
\t\\small
\t\\centering
\t\\caption{", caption, "}
\t\\begin{tabular}{l|c c c c c c|c c c c c c}
", header_top, body, "\n",
"\t\t\\Xhline{2\\arrayrulewidth}\n",
"\t\\end{tabular}
\t\\label{", label, "}
\t\\caption*{\\fontsize{", font_size, "}{", font_size + 2, "}\\selectfont ", footnote, "}
\\end{tabela}"
  )

  if (!is.null(file_tex)) {
    dir.create(dirname(file_tex), showWarnings = FALSE, recursive = TRUE)
    writeLines(out, file_tex)
    message("✅ Snippet LaTeX salvo em: ", normalizePath(file_tex, mustWork = FALSE))
  }

  return(out)
}

# Snippet LaTeX (sem preâmbulo) — com "(P1)" e "(P2)" nos grupos
avg_prob_tabletex <- function(data,
                                         p1_player, p1_pair,
                                         p2_player, p2_pair,
                                         p1_label = "P1", p2_label = "P2",
                                         caption   = "Probabilidade média atingida durante o comportamento estável",
                                         label     = "tabela_pares",
                                         footnote  = "Fonte: Elaborado pelo autor (2024).",
                                         font_size = 10,
                                         file_tex  = NULL) {
  stopifnot(requireNamespace("dplyr"), requireNamespace("tidyr"))

  # helpers
  fmt_dec <- function(x) formatC(as.numeric(x), format = "f", digits = 3,
                                 big.mark = ".", decimal.mark = ",")
  to_lambda_tex <- function(s) {
    s <- as.character(s)
    val <- sub(".*=\\s*", "", s)
    paste0("$\\lambda = ", val, "$")
  }

  # monta um bloco (P1/P2) -> λ + EWA/RL/BL (a/b)
  prep_block <- function(dat, player, pair_vec) {
    dat |>
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
      dplyr::mutate(value = paste0(fmt_dec(.data[[pair_vec[1]]]), "/", fmt_dec(.data[[pair_vec[2]]]))) |>
      dplyr::select(lambda, type, value) |>
      tidyr::pivot_wider(
        id_cols    = lambda,
        names_from = type,
        values_from= value,
        values_fill= list(value = "")
      ) |>
      dplyr::select(lambda, dplyr::any_of(c("EWA","RL","BL")))
  }

  p1 <- prep_block(data, p1_player, p1_pair)
  p2 <- prep_block(data, p2_player, p2_pair)

  out <- dplyr::full_join(p1, p2, by = "lambda", suffix = c("_P1", "_P2")) |>
    dplyr::mutate(lambda_tex = to_lambda_tex(lambda)) |>
    dplyr::arrange(lambda) |>
    dplyr::select(lambda_tex,
                  dplyr::any_of(c("EWA_P1","RL_P1","BL_P1")),
                  dplyr::any_of(c("EWA_P2","RL_P2","BL_P2")))

  # títulos dos grupos com (P1)/(P2)
  p1_title <- paste0(p1_pair[1], "/", p1_pair[2], " (", p1_label, ")")
  p2_title <- paste0(p2_pair[1], "/", p2_pair[2], " (", p2_label, ")")

  header_top <- paste0(
    "\t\t\\Xhline{2\\arrayrulewidth}\n",
    "\t\t& \\multicolumn{3}{c|}{", p1_title, "} & \\multicolumn{3}{c}{", p2_title, "} \\\\ \n",
    "\t\t\\hline\n",
    "\t\tIncremento & EWA & RL & BL & EWA & RL & BL \\\\ \\hline\n"
  )

  make_line <- function(r) {
    cells <- c(r[["lambda_tex"]],
               r[["EWA_P1"]], r[["RL_P1"]], r[["BL_P1"]],
               r[["EWA_P2"]], r[["RL_P2"]], r[["BL_P2"]])
    cells[is.na(cells)] <- ""
    paste0("\t\t", paste(cells, collapse = " & "), " \\\\")
  }
  body <- paste(apply(out, 1, make_line), collapse = "\n")

  latex <- paste0(
"\\begin{tabela}[H]
\t\\small
\t\\centering
\t\\caption{", caption, "}
\t\\begin{tabular}{l|c c c|c c c}
", header_top, body, "\n",
"\t\t\\Xhline{2\\arrayrulewidth}\n",
"\t\\end{tabular}
\t\\label{", label, "}
\t\\caption*{\\fontsize{", font_size, "}{", font_size + 2, "}\\selectfont ", footnote, "}
\\end{tabela}"
  )

  if (!is.null(file_tex)) {
    dir.create(dirname(file_tex), showWarnings = FALSE, recursive = TRUE)
    writeLines(latex, file_tex)
    message("✅ Snippet LaTeX salvo em: ", normalizePath(file_tex, mustWork = FALSE))
  }
  latex
}

# Gera o snippet LaTeX (sem preâmbulo)
prob_snippettex <- function(prob_bsg, prob_meg,
                                       caption  = "Distribuição (%) por estratégia e modelo",
                                       label    = "tabela_8",
                                       footnote = "Fonte: Elaborado pelo autor (2024).",
                                       digits   = 1,
                                       decimal  = ",") {
  # --- helpers
  to_df <- function(x) {
    df <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(rownames(df))) {
      df <- data.frame(Modelo = rownames(df), df, check.names = FALSE)
      rownames(df) <- NULL
    }
    # força numérico nas métricas
    num_cols <- setdiff(names(df), "Modelo")
    df[num_cols] <- lapply(df[num_cols], function(v) suppressWarnings(as.numeric(v)))
    df
  }
  fmt <- function(x) {
    x <- formatC(as.numeric(x), format = "f", digits = digits, decimal.mark = decimal, big.mark = ".")
    x
  }
  fmt_row <- function(v) paste(v, collapse = " & ")

  # --- dados
  bsg <- to_df(prob_bsg)
  meg <- to_df(prob_meg)

  # ordem opcional nas linhas
  ord <- c("EWA","RL","BL","Total")
  if ("Modelo" %in% names(bsg)) bsg <- bsg[order(factor(bsg$Modelo, levels = ord)), , drop = FALSE]
  if ("Modelo" %in% names(meg)) meg <- meg[order(factor(meg$Modelo, levels = ord)), , drop = FALSE]

  # cabeçalhos por bloco (pegam os nomes das colunas vindas das suas tabelas)
  bsg_cols <- names(bsg)[-1]
  meg_cols <- names(meg)[-1]

  # formata os números
  bsg_fmt <- bsg
  bsg_fmt[-1] <- lapply(bsg_fmt[-1], fmt)
  meg_fmt <- meg
  meg_fmt[-1] <- lapply(meg_fmt[-1], fmt)

  # monta linhas LaTeX
  header_line_bsg <- fmt_row(c("Modelo", bsg_cols))
  header_line_meg <- fmt_row(c("Modelo", meg_cols))
  body_bsg <- apply(bsg_fmt, 1, fmt_row)
  body_meg <- apply(meg_fmt, 1, fmt_row)

  # snippet completo no seu estilo (ambiente 'tabela')
  snippet <- paste0(
"\\begin{tabela}[H]
\\small
\\centering
\\caption{", caption, "}
\\begin{tabular}{l|c c c c}
\\Xhline{2\\arrayrulewidth}
\\multicolumn{5}{c}{\\textbf{BSG}} \\\\ \\hline
", header_line_bsg, " \\\\ \\hline
", paste0(body_bsg, " \\\\", collapse = "\n"), " \
\\hline
\\multicolumn{5}{c}{\\textbf{MEG}} \\\\ \\hline
", header_line_meg, " \\\\ \\hline
", paste0(body_meg, " \\\\", collapse = "\n"), " \
\\Xhline{2\\arrayrulewidth}
\\end{tabular}
\\label{", label, "}
\\caption*{\\fontsize{10}{12}\\selectfont ", footnote, "}
\\end{tabela}"
  )

  return(snippet)
}

# Salva direto em um arquivo .tex (só o snippet da tabela)
write_probtex <- function(prob_bsg, prob_meg, file,
                                   caption  = "Distribuição (%) por estratégia e modelo",
                                   label    = "tabela_8",
                                   footnote = "Fonte: Elaborado pelo autor (2024).",
                                   digits   = 1,
                                   decimal  = ",") {
  txt <- prob_snippettex(prob_bsg, prob_meg, caption, label, footnote, digits, decimal)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(txt, file, useBytes = TRUE)
  message("✅ Snippet LaTeX salvo em: ", normalizePath(file, mustWork = FALSE))
  invisible(file)
}

# ===== bloco LaTeX com parênteses =====
latex_block <- function(main_df, paren_df_raw,
                        block_title,
                        digits_main  = 1,
                        digits_paren = 1,
                        decimal      = ",",
                        show_paren_in_total = FALSE) {
  # normaliza main
  main <- as.data.frame(main_df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(rownames(main))) {
    main <- data.frame(Modelo = rownames(main), main, check.names = FALSE)
    rownames(main) <- NULL
  }
  names(main) <- .normalize_names(names(main))
  main$Modelo <- .normalize_names(main$Modelo)

  # força numérico nas métricas
  num_cols <- setdiff(names(main), "Modelo")
  main[num_cols] <- lapply(main[num_cols], function(v) suppressWarnings(as.numeric(v)))

  # reordena colunas do main
  main <- .reorder_by_player(main)

  # ajusta paren p/ ser igual ao main
  parn <- .coerce_like_main(main, paren_df_raw)
  parn <- .reorder_by_player(parn)

  # formatadores
  fmt_main  <- function(x) formatC(x, format = "f", digits = digits_main,  decimal.mark = decimal, big.mark = ".")
  fmt_paren <- function(x) formatC(x, format = "f", digits = digits_paren, decimal.mark = decimal, big.mark = ".")

  out <- main
  value_cols <- setdiff(names(main), "Modelo")
  for (cl in value_cols) {
    use_paren <- rep(TRUE, nrow(main))
    if (!show_paren_in_total) {
      use_paren[grepl("(?i)^(total|sum)$", main$Modelo)] <- FALSE
    }
    par_col <- parn[[cl]]
    out[[cl]] <- ifelse(
      use_paren & !is.na(par_col),
      paste0(fmt_main(main[[cl]]), " (", fmt_paren(par_col), ")"),
      fmt_main(main[[cl]])
    )
  }

  header_line <- paste(c("Modelo", value_cols), collapse = " & ")
  body_lines  <- apply(out, 1, function(r) paste(r, collapse = " & "))

  paste0(
    "\\multicolumn{5}{c}{\\textbf{", block_title, "}} \\\\ \\hline\n",
    header_line, " \\\\ \\hline\n",
    paste0(body_lines, " \\\\", collapse = "\n"), " \n"
  )
}

# ===== wrapper BSG+MEG =====
tex_matrix <- function(main_bsg, paren_bsg,
                          main_meg, paren_meg,
                          caption  = "Probabilidade média (valor principal) com estatística entre parênteses",
                          label    = "tabela_bsg_meg",
                          footnote = "Fonte: Elaborado pelo autor (2024).",
                          digits_main  = 1,
                          digits_paren = 1,
                          decimal      = ",",
                          show_paren_in_total = FALSE) {
  block_bsg <- latex_block(
    main_bsg, paren_bsg, block_title = "BSG",
    digits_main = digits_main, digits_paren = digits_paren,
    decimal = decimal, show_paren_in_total = show_paren_in_total
  )
  block_meg <- latex_block(
    main_meg, paren_meg, block_title = "MEG",
    digits_main = digits_main, digits_paren = digits_paren,
    decimal = decimal, show_paren_in_total = show_paren_in_total
  )

  paste0(
"\\begin{tabela}[H]
\\small
\\centering
\\caption{", caption, "}
\\begin{tabular}{l|c c c c}
\\Xhline{2\\arrayrulewidth}
", block_bsg,
"\\hline
", block_meg,
"\\Xhline{2\\arrayrulewidth}
\\end{tabular}
\\label{", label, "}
\\caption*{\\fontsize{10}{12}\\selectfont ", footnote, "}
\\end{tabela}"
  )
}

