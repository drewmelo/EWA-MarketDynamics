## ======================= FUNÇÕES PARA EXTRAÇÃO DE DADOS DA SIMULAÇÃO ==============================

# Função para mapear argumentos de probabilidade e atração
map_argument <- function(arg) {
  arg <- tolower(arg)
  if (arg == "p1") {
    return("P1")
  } else if (arg == "p2") {
    return("P2")
  } else if (arg == "a1") {
    return("A1")
  } else if (arg == "a2") {
    return("A2")
  } else {
    stop("Valor inválido. Use 'p1' ou 'p2' para probability e 'a1' ou 'a2' para attraction.")
  }
}

# Função para extrair dados de probabilidade e/ou atração
data_extraction <- function(sim, probability = NULL, attraction = NULL, n_strategy = NULL) {

  # Verifica se 'choice_prob' e 'attraction' estão presentes em sim
  if (!("choice_prob" %in% names(sim)) || !("attraction" %in% names(sim))) {
    stop(paste("'sim' deve conter as listas 'choice_prob' e 'attraction' provenientes da",
               "função sim_learning() do pacote Rgamer.", sep = "\n"))
  }

  result <- NULL

  # Extrai e transforma os dados de probabilidade, se fornecido
  if (!is.null(probability)) {
    probability <- map_argument(probability)

    prob_player <- sim$choice_prob[[probability]] %>%
      do.call(rbind, .) %>%
      tidyr::pivot_longer(cols = 1:n_strategy, names_to = 'estrategias', values_to = 'probabilidade')

    result <- prob_player
  }

  # Extrai e transforma os dados de atração, se fornecido
  if (!is.null(attraction)) {
    attraction <- map_argument(attraction)

    attr_player <- sim$attraction[[attraction]] %>%
      do.call(rbind, .) %>%
      tidyr::pivot_longer(cols = 1:n_strategy, names_to = 'estrategias_atr', values_to = 'atracao') %>%
      dplyr::mutate(
        estrategias_atr = stringr::str_replace_all(estrategias_atr, "\\.1", "")
      )

    # Combina dados de probabilidade e atração, se ambos forem fornecidos
    if (is.null(result)) {
      result <- attr_player
    } else {
      result <- dplyr::bind_cols(prob_player, attr_player) %>%
        dplyr::select(-estrategias_atr)
    }
  }

  if (is.null(n_strategy)) {
    stop("É necessário especificar o número de estratégias escolhidas para os jogadores 1 ou 2 através do parâmetro 'n_strategy'.\n")
  }

  return(result)
}

# Função para processar dados de jogadores e combinar com dados de probabilidade ou atração
player_render <- function(sim, n_player = NULL, data_join, n_replicate = NULL) {

  if (!is.null(n_player) && length(n_player) != 1) {
    stop("É necessário especificar os valores 1 ou 2 para o parâmetro 'n_player'")
  }

  if (!(n_player %in% c(1, 2))) {
    stop("'n_player' deve ser 1 ou 2, correspondendo aos jogadores 1 ou 2.")
  }

  if (is.null(n_replicate)) {
    stop("É necessário especificar o número de replicações através do parâmetro 'n_replicate'.")
  }

  suppressMessages({
    base <- sim$data
    base$n_player <- rep(1:2, length.out = nrow(base))

    if(n_player == 1) {
      p1 <- base[base$n_player == 1, ] %>%
        replicate(n_replicate, ., simplify = F) %>%
        do.call(rbind, .) %>%
        dplyr::arrange(sample, period) %>%
        dplyr::bind_cols(., data_join) %>%
        dplyr::select(-n_player)

      return(p1)
    } else if(n_player == 2) {
      p2 <- base[base$n_player == 2, ] %>%
        replicate(n_replicate, ., simplify = F) %>%
        do.call(rbind, .) %>%
        dplyr::arrange(sample, period) %>%
        dplyr::bind_cols(., data_join) %>%
        dplyr::select(-n_player)

      return(p2)
    }
  })
}

# Função para remover colunas específicas e renomear a primeira coluna 'period'
remove_cols <- function(data) {
  period_cols <- grep(paste0("^", 'period'), names(data))

  first_col <- period_cols[1]
  other_cols <- period_cols[-1]

  data <- data[, -other_cols]

  colnames(data)[first_col] <- "period"

  return(data)
}

# ===== utils =====
.normalize_names <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "UTF-8")
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Converte qualquer coisa (vector/matrix/df) num DF com 'Modelo' + mesmas colunas do main,
# criando Total se faltar; reordena linhas em EWA, RL, BL, Total
.coerce_like_main <- function(main_df, paren_df_raw) {
  main <- as.data.frame(main_df, check.names = FALSE, stringsAsFactors = FALSE)
  # modelo/colunas "alvo"
  if (!is.null(rownames(main))) {
    modelos_target <- rownames(main)
  } else if ("Modelo" %in% names(main)) {
    modelos_target <- main$Modelo
  } else {
    stop("main_df precisa de rownames (EWA, RL, BL, Total) ou coluna 'Modelo'.")
  }
  modelos_target <- .normalize_names(modelos_target)
  cols_target <- .normalize_names(colnames(main))
  if ("Modelo" %in% cols_target) {
    metric_cols <- setdiff(cols_target, "Modelo")
  } else {
    # se main não tinha coluna Modelo, vamos assumir que rownames são os modelos
    metric_cols <- cols_target
  }

  # pega valores numéricos de paren_df_raw como vetor
  get_numeric_vector <- function(x) {
    if (is.null(x)) return(numeric(0))
    if (is.vector(x)) return(suppressWarnings(as.numeric(x)))
    if (is.matrix(x)) return(suppressWarnings(as.numeric(t(x))))  # por linha
    if (is.data.frame(x)) return(suppressWarnings(as.numeric(t(as.matrix(x)))))
    suppressWarnings(as.numeric(x))
  }
  v <- get_numeric_vector(paren_df_raw)
  k  <- length(metric_cols)
  r  <- length(modelos_target)

  # tenta dar forma à matriz de parênteses
  M <- NULL
  if (length(v) == k * r) {
    M <- matrix(v, nrow = r, ncol = k, byrow = TRUE)
    rown <- modelos_target
  } else if (length(v) == k * (r - 1)) {
    # faltou Total: usa EWA/RL/BL e computa Total = soma por coluna
    M3 <- matrix(v, nrow = r - 1, ncol = k, byrow = TRUE)
    total <- colSums(M3, na.rm = TRUE)
    M <- rbind(M3, total)
    rown <- c(modelos_target[1:(r-1)], modelos_target[r])
  } else {
    # plano C: tentar “moldar” via ncol do input
    asmat <- tryCatch(as.matrix(paren_df_raw), error = function(e) NULL)
    if (!is.null(asmat)) {
      nr <- nrow(asmat); nc <- ncol(asmat)
      # se dimensões batem, ok
      if (nr == r && nc == k) {
        M <- asmat
        rown <- modelos_target
      } else if (nr == r - 1 && nc == k) {
        total <- colSums(asmat, na.rm = TRUE)
        M <- rbind(asmat, total)
        rown <- c(modelos_target[1:(r-1)], modelos_target[r])
      }
    }
  }

  if (is.null(M)) {
    # não deu para alinhar → cria NAs mas preserva estrutura
    M <- matrix(NA_real_, nrow = r, ncol = k)
    rown <- modelos_target
    warning("Não consegui alinhar 'paren_df' automaticamente; valores entre parênteses ficarão vazios.")
  }

  colnames(M) <- metric_cols
  rownames(M) <- rown

  # monta DF final como o main: 'Modelo' + metric_cols, linhas na ordem alvo
  paren_df <- data.frame(Modelo = rown, M, check.names = FALSE)
  paren_df$Modelo <- .normalize_names(paren_df$Modelo)

  # reordena linhas p/ EWA, RL, BL, Total (ou o que houver)
  ord <- intersect(c("EWA","RL","BL","Total","Sum"), paren_df$Modelo)
  rest <- setdiff(paren_df$Modelo, ord)
  paren_df <- paren_df[match(c(ord, rest), paren_df$Modelo), , drop = FALSE]

  # garante colunas iguais às do main
  paren_df <- paren_df[, c("Modelo", metric_cols), drop = FALSE]
  paren_df
}

# Reordena colunas (P1 primeiro, P2 depois), preservando 'Modelo'
.reorder_by_player <- function(df) {
  cols <- names(df)
  base <- "Modelo"
  metrics <- setdiff(cols, base)
  cols_p1 <- grep("\\(P1\\)", metrics, value = TRUE)
  cols_p2 <- grep("\\(P2\\)", metrics, value = TRUE)
  others  <- setdiff(metrics, c(cols_p1, cols_p2))
  df[, c(base, sort(cols_p1), sort(cols_p2), sort(others)), drop = FALSE]
}