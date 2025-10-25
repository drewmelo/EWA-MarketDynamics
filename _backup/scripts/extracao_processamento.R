### ========== EXTRAÇÃO E PROCESSAMENTO DOS DADOS SIMULADOS ==================

## BSG -----------------------------------------------------------------------
# Inicializando a lista para armazenar os dataframes processados da matriz BSG
combined_results_bsg <- base::list()

# Processando os resultados da matriz BSG para cada tipo de aprendizado
for (learning_type in base::names(results_bsg)) {
  # Lista para armazenar os dataframes de cada simulação do tipo atual
  type_combined <- base::list()

  # Mensagem clara no console
  message(glue::glue("Processando simulações do jogo BSG ({learning_type})..."))

  # Criar barra de progresso
  total_sims <- length(results_bsg[[learning_type]])
  pb <- txtProgressBar(min = 0, max = total_sims, style = 3)

  # Processando cada simulação para o tipo de aprendizado atual
  for (sim_id in base::seq_along(results_bsg[[learning_type]])) {
    setTxtProgressBar(pb, sim_id)  # Atualiza a barra de progresso
    
    sim_data <- dplyr::bind_rows(
      # Extraindo e processando dados do jogador 1
      results_bsg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p1",
                        attraction = "a1",
                        n_strategy = base::length(matriz_bsg$strategy$s1)) |>
        player_render(sim = results_bsg[[learning_type]][[sim_id]],
                      n_player = 1,
                      n_replicate = base::length(matriz_bsg$strategy$s1)) |>
        remove_cols(),
      # Extraindo e processando dados do jogador 2
      results_bsg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p2",
                        attraction = "a2",
                        n_strategy = base::length(matriz_bsg$strategy$s2)) |>
        player_render(sim = results_bsg[[learning_type]][[sim_id]],
                      n_player = 2,
                      n_replicate = base::length(matriz_bsg$strategy$s2)) |>
        remove_cols()
    ) |>
      # Adicionando informações auxiliares
      dplyr::mutate(sim_id = sim_id,  # Identificação da simulação
                    type = learning_type) # Tipo de aprendizado

    # Armazenando os dados processados na lista
    type_combined[[sim_id]] <- sim_data
  }

  close(pb)  # Fecha a barra ao terminar a simulação

  # Combinando os dataframes de todas as simulações do tipo atual
  combined_results_bsg[[learning_type]] <- dplyr::bind_rows(type_combined)
}

# Consolidando os dados de todos os tipos de aprendizado em um único dataframe
bsg_df <- dplyr::bind_rows(combined_results_bsg) |>
  dplyr::rename(
    amostra = 1, periodo = 2, jogador = 3, estrategia_escolhida = 4
  ) |>
  dplyr::mutate(type = base::factor(type, levels = c("EWA", "RL", "BL"))) |>
  dplyr::relocate(type, sim_id, .before = amostra) |>
  dplyr::arrange(sim_id, type, amostra, periodo)

## MEG -----------------------------------------------------------------------
# Inicializando a lista para armazenar os dataframes processados da matriz MEG
combined_results_meg <- base::list()

# Processando os resultados da matriz MEG para cada tipo de aprendizado
for (learning_type in base::names(results_meg)) {
  # Lista para armazenar os dataframes de cada simulação do tipo atual
  type_combined <- base::list()

  message(glue::glue("Processando simulações do jogo MEG ({learning_type})..."))

  # Criar barra de progresso
  total_sims <- length(results_meg[[learning_type]])
  pb <- txtProgressBar(min = 0, max = total_sims, style = 3)

  # Processando cada simulação para o tipo de aprendizado atual
  for (sim_id in base::seq_along(results_meg[[learning_type]])) {

    setTxtProgressBar(pb, sim_id)  # Atualiza a barra de progresso
    
    sim_data <- dplyr::bind_rows(
      # Extraindo e processando dados do jogador 1
      results_meg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p1",
                        attraction = "a1",
                        n_strategy = base::length(matriz_meg$strategy$s1)) |>
        player_render(sim = results_meg[[learning_type]][[sim_id]],
                      n_player = 1,
                      n_replicate = base::length(matriz_meg$strategy$s1)) |>
        remove_cols(),
      # Extraindo e processando dados do jogador 2
      results_meg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p2",
                        attraction = "a2",
                        n_strategy = base::length(matriz_meg$strategy$s2)) |>
        player_render(sim = results_meg[[learning_type]][[sim_id]],
                      n_player = 2,
                      n_replicate = base::length(matriz_meg$strategy$s2)) |>
        remove_cols()
    ) |>
      # Adicionando informações auxiliares
      dplyr::mutate(sim_id = sim_id,  # Identificação da simulação
                    type = learning_type) # Tipo de aprendizado

    # Armazenando os dados processados na lista
    type_combined[[sim_id]] <- sim_data
  }

  close(pb)

  # Combinando os dataframes de todas as simulações do tipo atual
  combined_results_meg[[learning_type]] <- dplyr::bind_rows(type_combined)
}

# Consolidando os dados de todos os tipos de aprendizado em um único dataframe
meg_df <- dplyr::bind_rows(combined_results_meg) |>
  dplyr::rename(
    amostra = 1, periodo = 2, jogador = 3, estrategia_escolhida = 4
  ) |>
  dplyr::mutate(type = base::factor(type, levels = c("EWA", "RL", "BL"))) |>
  dplyr::relocate(type, sim_id, .before = amostra) |>
  dplyr::arrange(sim_id, type, amostra, periodo)
