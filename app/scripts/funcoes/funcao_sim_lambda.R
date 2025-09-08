# Função para simular barra de progresso customizada com info de lambda
progress_bar <- function(current, total, width = 50, lambda_val = NULL) {
  pct <- current / total
  n_filled <- round(pct * width)
  bar <- paste0(
    "|",
    strrep("=", n_filled),
    strrep(" ", width - n_filled),
    "| λ = ", lambda_val,
    " | Simulação: ", current, "/", total,
    strrep(" ", 10)  # limpa sobra visual do console
  )
  cat("\r", bar)
  flush.console()
}

sim_lambda <- function(matriz, learning_types, lambda_values, 
                        game_label = NULL, n_samples = 50, n_periods = 50) {
  
  # Captura o nome do objeto se o game_label não for fornecido
  if (is.null(game_label)) {
    game_label <- as.character(substitute(matriz))
  }
  
  results <- base::list()
  
  # Iterando sobre os tipos de aprendizado
  for (learning_type in base::names(learning_types)) {
    # Configurando os parâmetros do tipo de aprendizado
    config <- learning_types[[learning_type]]
    results[[learning_type]] <- base::list()
    
    # Mensagem informando o tipo de aprendizado
    message(glue("🧠 Simulando {game_label} - Tipo de aprendizado: {learning_type}"))
    
    # Número total de simulações com diferentes valores de lambda
    total_steps <- length(lambda_values)
    
    # Variando os valores de lambda
    for (i in seq_along(lambda_values)) {
      lambda_value <- lambda_values[i]
      
      # Simulação para a matriz fornecida
      result <- sim_learning(
        matriz,
        n_samples = n_samples,
        n_periods = n_periods,
        type = config$type,
        lambda = lambda_value,
        delta = config$delta,
        rho = config$rho,
        phi = config$phi
      )
      
      # Armazenando resultados da simulação
      results[[learning_type]][[i]] <- result
      
      # Atualiza a barra de progresso personalizada
      progress_bar(i, total_steps, lambda_val = lambda_value)
    }
    
    cat("\n\n")  # quebra de linha após cada bloco de aprendizado
  }
  
  return(results)
}