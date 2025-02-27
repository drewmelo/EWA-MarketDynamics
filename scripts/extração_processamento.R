## BSG -----------------------------------------------------------------------
# Inicializando a lista para armazenar os dataframes processados da matriz BSG
combined_results_bsg <- list()

# Processando os resultados da matriz BSG para cada tipo de aprendizado
for (learning_type in names(results_bsg)) {
  # Lista para armazenar os dataframes de cada simulação do tipo atual
  type_combined <- list()

  # Processando cada simulação para o tipo de aprendizado atual
  for (sim_id in seq_along(results_bsg[[learning_type]])) {
    sim_data <- bind_rows(
      # Extraindo e processando dados do jogador 1
      results_bsg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p1",
                        attraction = "a1",
                        n_strategy = length(matriz_bsg$strategy$s1)) |>
        player_render(sim = results_bsg[[learning_type]][[sim_id]],
                      n_player = 1,
                      n_replicate = length(matriz_bsg$strategy$s1)) |>
        remove_cols(),
      # Extraindo e processando dados do jogador 2
      results_bsg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p2",
                        attraction = "a2",
                        n_strategy = length(matriz_bsg$strategy$s2)) |>
        player_render(sim = results_bsg[[learning_type]][[sim_id]],
                      n_player = 2,
                      n_replicate = length(matriz_bsg$strategy$s2)) |>
        remove_cols()
    ) |>
      # Adicionando informações auxiliares
      mutate(sim_id = sim_id,  # Identificação da simulação
             type = learning_type) # Tipo de aprendizado

    # Armazenando os dados processados na lista
    type_combined[[sim_id]] <- sim_data
  }

  # Combinando os dataframes de todas as simulações do tipo atual
  combined_results_bsg[[learning_type]] <- bind_rows(type_combined)
}

# Consolidando os dados de todos os tipos de aprendizado em um único dataframe
bsg_df <- bind_rows(combined_results_bsg) |>
  rename(
    amostra = 1, periodo = 2, jogador = 3, estrategia_escolhida = 4
  ) |>
  mutate(type = factor(type, levels = c("EWA", "RL", "BL"))) |>
  relocate(type, sim_id, .before = amostra) |>
  arrange(sim_id, type, amostra, periodo)

## MEG -----------------------------------------------------------------------
# Inicializando a lista para armazenar os dataframes processados da matriz MEG
combined_results_meg <- list()

# Processando os resultados da matriz MEG para cada tipo de aprendizado
for (learning_type in names(results_meg)) {
  # Lista para armazenar os dataframes de cada simulação do tipo atual
  type_combined <- list()

  # Processando cada simulação para o tipo de aprendizado atual
  for (sim_id in seq_along(results_meg[[learning_type]])) {
    sim_data <- bind_rows(
      # Extraindo e processando dados do jogador 1
      results_meg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p1",
                        attraction = "a1",
                        n_strategy = length(matriz_meg$strategy$s1)) |>
        player_render(sim = results_meg[[learning_type]][[sim_id]],
                      n_player = 1,
                      n_replicate = length(matriz_meg$strategy$s1)) |>
        remove_cols(),
      # Extraindo e processando dados do jogador 2
      results_meg[[learning_type]][[sim_id]] |>
        data_extraction(probability = "p2",
                        attraction = "a2",
                        n_strategy = length(matriz_meg$strategy$s2)) |>
        player_render(sim = results_meg[[learning_type]][[sim_id]],
                      n_player = 2,
                      n_replicate = length(matriz_meg$strategy$s2)) |>
        remove_cols()
    ) |>
      # Adicionando informações auxiliares
      mutate(sim_id = sim_id,  # Identificação da simulação
             type = learning_type) # Tipo de aprendizado

    # Armazenando os dados processados na lista
    type_combined[[sim_id]] <- sim_data
  }

  # Combinando os dataframes de todas as simulações do tipo atual
  combined_results_meg[[learning_type]] <- bind_rows(type_combined)
}

# Consolidando os dados de todos os tipos de aprendizado em um único dataframe
meg_df <- bind_rows(combined_results_meg) |>
  rename(
    amostra = 1, periodo = 2, jogador = 3, estrategia_escolhida = 4
  ) |>
  mutate(type = factor(type, levels = c("EWA", "RL", "BL"))) |>
  relocate(type, sim_id, .before = amostra) |>
  arrange(sim_id, type, amostra, periodo)
