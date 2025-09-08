build_simulation_df <- function(results, matriz, jogo = NULL) {
  if (is.null(jogo)) {
    jogo <- as.character(substitute(matriz)) # usa o nome da matriz se não for especificado
  }

  combined_results <- base::list()

  # Iterando sobre os tipos de aprendizado
  for (learning_type in base::names(results)) {
    # Lista para armazenar os dataframes de cada simulação do tipo atual
    type_combined <- base::list()

    # Mensagem clara no console
    message(glue::glue("🔄 Processando simulações do jogo {jogo} ({learning_type})..."))

    # Criar barra de progresso
    total_sims <- length(results[[learning_type]])
    pb <- txtProgressBar(min = 0, max = total_sims, style = 3)

    # Processando cada simulação
    for (sim_id in base::seq_along(results[[learning_type]])) {
      setTxtProgressBar(pb, sim_id)  # Atualiza a barra de progresso

      sim_data <- dplyr::bind_rows(
        # Jogador 1
        results[[learning_type]][[sim_id]] |>
          data_extraction(probability = "p1", attraction = "a1",
                          n_strategy = length(matriz$strategy$s1)) |>
          player_render(sim = results[[learning_type]][[sim_id]],
                        n_player = 1,
                        n_replicate = length(matriz$strategy$s1)) |>
          remove_cols(),
        # Jogador 2
        results[[learning_type]][[sim_id]] |>
          data_extraction(probability = "p2", attraction = "a2",
                          n_strategy = length(matriz$strategy$s2)) |>
          player_render(sim = results[[learning_type]][[sim_id]],
                        n_player = 2,
                        n_replicate = length(matriz$strategy$s2)) |>
          remove_cols()
      ) |>
        dplyr::mutate(sim_id = sim_id, type = learning_type)

      type_combined[[sim_id]] <- sim_data
    }

    close(pb)  # Fecha a barra

    # Agrupa todas as simulações do tipo atual
    combined_results[[learning_type]] <- dplyr::bind_rows(type_combined)
  }

  # Retorna o dataframe consolidado e formatado
  final_df <- dplyr::bind_rows(combined_results) |>
    dplyr::rename(amostra = 1, periodo = 2, jogador = 3, estrategia_escolhida = 4) |>
    dplyr::mutate(type = factor(type, levels = c("EWA", "RL", "BL"))) |>
    dplyr::relocate(type, sim_id, .before = amostra) |>
    dplyr::arrange(sim_id, type, amostra, periodo)

  return(final_df)
}
