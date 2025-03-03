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

  # Mensagens de console para o usuário
  if (!is.null(probability) && !is.null(attraction)) {
    cat("Tanto probability quanto attraction foram especificados. Processando os dados dos valores de ambos...\n")
  } else if (!is.null(probability)) {
    cat("Apenas probability foi especificado. Processando somente os dados dos valores de probabilidade...\n")
  } else if (!is.null(attraction)) {
    cat("Apenas attraction foi especificado. Processando somente os dados dos valores de atração...\n")
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
