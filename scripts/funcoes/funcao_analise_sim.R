# ------------------------------------------------------------
# Função: sim_convergence
# Objetivo:
# Calcular medidas de convergência das simulações em relação aos
# equilíbrios teóricos definidos para o BSG e o MEG.
# ------------------------------------------------------------
sim_convergence <- function(sim_data, lambda_val) {
  
  # Define os alvos teóricos de cada estratégia por jogo e jogador.
  targets <- tibble::tribble(
    ~jogo, ~jogador,    ~estrategias,   ~eq_teorico,
    "BSG", "Comprador", "Aceitar",      0,
    "BSG", "Comprador", "Rejeitar",     1,
    "BSG", "Vendedor",  "Preço Alto",   1,
    "BSG", "Vendedor",  "Preço Baixo",  0,
    "MEG", "Empresa A", "Entrar",       5/6,
    "MEG", "Empresa A", "Não Entrar",   1/6,
    "MEG", "Empresa B", "Entrar",       5/6,
    "MEG", "Empresa B", "Não Entrar",   1/6
  )
  
  # Junta as bases dos dois jogos em uma única base.
  # dados_m1 recebe identificação de BSG e dados_m2 recebe identificação de MEG.
  all_data <- dplyr::bind_rows(
    sim_data$dados_m1 |>
      dplyr::mutate(
        jogo = "BSG",
        type = as.character(type),
        jogador = as.character(jogador),
        estrategias = as.character(estrategias)
      ),
    sim_data$dados_m2 |>
      dplyr::mutate(
        jogo = "MEG",
        type = as.character(type),
        jogador = as.character(jogador),
        estrategias = as.character(estrategias)
      )
  ) |>
    
    # Associa cada linha ao seu respectivo alvo teórico.
    dplyr::left_join(targets, by = c("jogo", "jogador", "estrategias")) |>
    
    # Calcula a distância absoluta entre a proporção observada
    # e o equilíbrio teórico.
    dplyr::mutate(
      dist_eq = abs(prop - eq_teorico),
      lambda = lambda_val
    )
  
  # Função auxiliar para manter apenas as estratégias usadas
  # como referência principal de convergência.
  keep_eq <- \(df) {
    df |>
      dplyr::filter(
        (jogo == "BSG" & jogador == "Comprador" & estrategias == "Rejeitar") |
        (jogo == "BSG" & jogador == "Vendedor" & estrategias == "Preço Alto") |
        (jogo == "MEG" & estrategias == "Entrar")
      )
  }
  
  # Calcula medidas-resumo da distância ao equilíbrio teórico.
  summary <- all_data |>
    keep_eq() |>
    dplyr::arrange(type, jogador, estrategias, periodo) |>
    dplyr::group_by(lambda, jogo, type, jogador, estrategias) |>
    dplyr::summarise(
      mean_error = mean(dist_eq, na.rm = TRUE),
      final_error = dplyr::last(dist_eq),
      last10_error = mean(tail(dist_eq, 10), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calcula a trajetória média da distância ao equilíbrio ao longo dos períodos.
  path <- all_data |>
    keep_eq() |>
    dplyr::group_by(lambda, jogo, type, periodo) |>
    dplyr::summarise(
      mean_dist = mean(dist_eq, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Retorna a tabela-resumo e a trajetória temporal da distância ao equilíbrio.
  list(
    summary = summary,
    path = path
  )
}

# ------------------------------------------------------------
# Função: plot_base
# Objetivo:
# Construir um gráfico-base da distância média ao alvo teórico
# em função dos valores de lambda.
# ------------------------------------------------------------
plot_base <- function(dados, tag_lab) {
  
  # Seleciona o último ponto de cada modelo para posicionar os rótulos
  # diretamente no fim das linhas.
  rotulos_fim <- dados |>
    group_by(type) |>
    slice_max(order_by = lambda, n = 1) |>
    ungroup()
  
  # Constrói o gráfico de linha comparando os modelos.
  ggplot(dados, aes(x = lambda, y = erro_medio, color = type, group = type)) +
    geom_line(linewidth = 1.6) +
    
    # Adiciona rótulos dos modelos no final de cada trajetória.
    geom_text_repel(
      data = rotulos_fim,
      aes(label = type),
      nudge_x = 0.05,
      direction = "y",
      segment.color = NA,
      show.legend = FALSE,
      family = fonte_base,
      size = 5.5
    ) +
    
    # Permite que os rótulos ultrapassem levemente a área do gráfico.
    coord_cartesian(clip = "off") +
    
    # Ajusta os intervalos do eixo x e usa vírgula como separador decimal.
    scale_x_continuous(
      breaks = seq(0.1, 1, by = 0.2),
      labels = label_number(decimal.mark = ",", accuracy = 0.1)
    ) +
    
    # Define manualmente as cores dos modelos.
    scale_color_manual(
      values = c(
        "EWA" = color_main[1],
        "RL"  = color_main[2],
        "BL"  = color_main[3]
      )
    ) +
    
    # Define títulos dos eixos e tag do painel.
    labs(
      x = expression(lambda),
      y = "Distância média ao alvo teórico",
      color = NULL,
      tag = tag_lab
    ) +
    
    # Aplica tema acadêmico usado no projeto.
    beautyxtrar::theme_academic(base_size = 18, base_family = fonte_base) +
    theme(
      legend.position = "none",
      plot.tag = element_text(
        size = 18,
        family = fonte_base,
        color = "black"
      ),
      plot.tag.position = "bottom",
      plot.margin = margin(5.5, 40, 5.5, 5.5)
    )
}

# ------------------------------------------------------------
# Função: get_props
# Objetivo:
# Calcular a proporção de escolha de cada estratégia por modelo,
# simulação, jogador e período.
# ------------------------------------------------------------
get_props <- function(dados, sims = 1:10, percentual = F) {
  
  # Cria uma base auxiliar indicando, para cada linha, se a estratégia
  # observada foi a mesma estratégia listada na coluna "estrategias".
  prop_base <- dados |>
    dplyr::mutate(
      escolhida = dplyr::if_else(estrategia_escolhida == estrategias, 1, 0)
    ) |>
    
    # Agrupa por modelo, simulação, jogador, período e estratégia.
    dplyr::group_by(type, sim_id, jogador, periodo, estrategias) |>
    
    # Conta quantas vezes a estratégia foi escolhida e o total de observações.
    dplyr::reframe(
      count_escolhida = sum(escolhida),
      total_amostra   = dplyr::n()
    ) |>
    
    # Calcula a proporção de escolha da estratégia.
    dplyr::mutate(
      prop = count_escolhida / total_amostra
    )
  
  # Cria uma lista com uma base separada para cada simulação selecionada.
  lista_sims <- purrr::map(
    sims,
    function(sim) {
      
      # Filtra apenas a simulação atual.
      dados_saida <- prop_base |>
        dplyr::filter(sim_id == sim)
      
      # Caso percentual = TRUE, transforma a proporção em porcentagem.
      if (percentual) {
        dados_saida <- dados_saida |>
          dplyr::mutate(prop = prop * 100)
      }
      
      # Mantém apenas as colunas principais e a proporção calculada.
      dados_saida |>
        dplyr::select(type:estrategias, prop)
    }
  )
  
  # Nomeia os elementos da lista como sim1, sim2, ..., sim10.
  names(lista_sims) <- paste0("sim", sims)
  
  # Retorna a lista com as proporções por simulação.
  lista_sims
}

# ------------------------------------------------------------
# Função: get_stability
# Objetivo:
# Avaliar a estabilidade das estratégias dominantes ao longo dos
# períodos para os jogos BSG e MEG, considerando diferentes modelos.
# ------------------------------------------------------------
get_stability <- function(bsg, meg, modelos = c("EWA", "RL", "BL"),
                                 limiar = 0.05, janela = 10, min_estavel = 0.8) {
  
  # Função interna usada para calcular a estabilidade em um jogo
  # específico e para um modelo específico.
  calcular <- function(dados, nome_jogo, modelo) {
    
    dados |> 
      # Filtra apenas o modelo analisado.
      dplyr::filter(type == modelo) |> 
      
      # Agrupa por período e jogador.
      dplyr::group_by(periodo, jogador) |> 
      
      # Identifica, em cada período, a maior proporção observada
      # e a estratégia associada a essa maior proporção.
      dplyr::summarise(
        max_prop = max(prop, na.rm = TRUE),
        estrategia_dominante = estrategias[which.max(prop)],
        .groups = "drop"
      ) |> 
      
      # Agrupa por jogador para calcular a evolução temporal
      # da estratégia dominante separadamente.
      dplyr::group_by(jogador) |> 
      dplyr::arrange(periodo, .by_group = TRUE) |> 
      
      # Calcula a variação entre períodos e identifica estabilidade.
      dplyr::mutate(
        variacao = abs(max_prop - dplyr::lag(max_prop)),
        
        # Considera estável quando a variação entre períodos
        # é menor que o limiar definido.
        estabilidade = variacao < limiar,
        
        # O primeiro período não possui defasagem; por isso,
        # valores NA são tratados como FALSE.
        estabilidade = ifelse(is.na(estabilidade), FALSE, estabilidade),
        
        # Aplica uma janela móvel para verificar se a estabilidade
        # se mantém em uma sequência de períodos.
        estabilidade_janela = zoo::rollapply(
          estabilidade,
          width = janela,
          FUN = function(x) mean(x) >= min_estavel,
          fill = NA,
          align = "right"
        ),
        
        # Identifica o jogo e o modelo no resultado final.
        jogo = nome_jogo,
        modelo = modelo
      ) |> 
      dplyr::ungroup()
  }
  
  # Aplica a função interna para todos os modelos no BSG e no MEG,
  # juntando os resultados em uma única base.
  resultado_completo <- dplyr::bind_rows(
    purrr::map_dfr(modelos, ~ calcular(bsg, "BSG", .x)),
    purrr::map_dfr(modelos, ~ calcular(meg, "MEG", .x))
  )
  
  # Resume apenas os períodos em que a estabilidade da janela foi atingida.
  resumo <- resultado_completo |> 
  dplyr::filter(estabilidade_janela == TRUE) |> 
  dplyr::group_by(jogo, modelo, jogador) |> 
  dplyr::summarise(
    
    # Estratégia dominante mais frequente durante o período estável.
    estrategia_escolhida = names(
      sort(table(estrategia_dominante), decreasing = TRUE)
    )[1],
    
    # Primeiro período em que a condição de estabilidade foi observada.
    periodo_estabilidade = min(periodo),
    
    # Proporção média de períodos classificados como estáveis.
    taxa_estabilidade = mean(estabilidade, na.rm = TRUE),
    
    # Variação média da proporção dominante no trecho estável.
    variacao_media_estavel = mean(variacao, na.rm = TRUE),
    
    # Proporção média da estratégia dominante no trecho estável.
    prop_media_estavel = mean(max_prop, na.rm = TRUE),
    
    # Menor proporção observada no trecho estável.
    prop_min_estavel = min(max_prop, na.rm = TRUE),
    
    # Maior proporção observada no trecho estável.
    prop_max_estavel = max(max_prop, na.rm = TRUE),
    
    # Diferença entre o maior e o menor valor da proporção dominante.
    amplitude_estavel = prop_max_estavel - prop_min_estavel,
    
    .groups = "drop"
  )
  
  # Retorna tanto a base completa quanto o resumo final.
  list(
    completo = resultado_completo,
    resumo = resumo
  )
}