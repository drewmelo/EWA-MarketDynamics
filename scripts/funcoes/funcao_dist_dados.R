### ================ FUNÇÕES PARA DISTRIBUIÇÃO DE DADOS =======================

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
    dplyr::filter(estrategia_escolhida == estrategias)

  # Criar o histograma
  p <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = probabilidade, color = type)) +
    ggplot2::geom_histogram(ggplot2::aes(fill = type), alpha = 0.5, position = "identity",
                            col = '#485B6D', linewidth = 0.3, bins = bins) +
    ggplot2::geom_vline(data = data_filtered |>
                          dplyr::group_by(type, sim_id) |>
                          dplyr::mutate(mediana = stats::median(probabilidade)),
                        ggplot2::aes(xintercept = mediana, color = type),
                        linetype = "dashed") +
    ggplot2::facet_wrap(~ lambda, nrow = 2) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                                labels = scales::label_comma(big.mark = ".",
                                                             decimal.mark = ","),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(labels = scales::label_comma(decimal.mark = ",", big.mark = ".")) +
    ggplot2::scale_fill_manual(values = color_main) +
    ggplot2::scale_color_manual(values = color_main) +
    ggplot2::labs(x = x_label, y = y_label, fill = "Mediana", col = "Mediana", title = title) +
    beautyxtrar::theme_xtra(base_family = fonte_base, base_size = 14)

  # Salvar o gráfico como PDF
  ggplot2::ggsave(plot = p, filename = file_name,
                  width = width, height = height, units = "in",
                  device = cairo_pdf)

  return(p)  # Retorna o gráfico para visualização se necessário
}

#--------------------------------------------------
# Extrair probabilidades
#--------------------------------------------------
# Filtra a base para um modelo, jogador e estratégia específicos.
# Essa função é usada como etapa inicial para montar as comparações
# entre papéis estratégicos do BSG e do MEG.
#
# Entradas principais:
#   - df: base original do jogo
#   - modelo_sel: modelo de aprendizado selecionado
#   - jogador_sel: jogador analisado
#   - estrategia_sel: estratégia analisada
#   - jogo_label: rótulo do jogo ("BSG" ou "MEG")
#
# Saída:
#   - base com probabilidades já padronizadas para comparação.
get_probs <- function(df, modelo_sel, jogador_sel, estrategia_sel, jogo_label) {
  df %>%
    
    # Mantém apenas as linhas do modelo, jogador e estratégia selecionados.
    filter(
      type == modelo_sel,
      jogador == jogador_sel,
      estrategia_escolhida == estrategia_sel
    ) %>%
    
    # Reorganiza as colunas e padroniza os nomes para uso posterior.
    transmute(
      modelo = type,
      sim_id,
      lambda,
      jogador = jogador_sel,
      estrategia = estrategia_sel,
      jogo = jogo_label,
      prob = probabilidade
    )
}

#--------------------------------------------------
# Rodar KS entre dois vetores
#--------------------------------------------------
# Executa o teste de Kolmogorov-Smirnov entre duas distribuições.
# O teste compara se dois vetores de probabilidades parecem vir
# da mesma distribuição.
#
# Caso uma das amostras esteja vazia, a função retorna NA.
# Isso evita que o script pare por erro quando algum filtro não
# encontra observações.
ks_two <- function(x, y) {
  
  # Verifica se algum dos vetores está vazio.
  if (length(x) == 0 || length(y) == 0) {
    return(
      tibble(
        estatistica_D = NA_real_,
        p_value = NA_real_,
        metodo = "KS (amostra ausente)"
      )
    )
  }

  # Aplica o teste KS aos dois vetores.
  ks <- ks.test(x, y)

  # Retorna apenas as informações centrais do teste.
  tibble(
    estatistica_D = unname(ks$statistic),
    p_value = unname(ks$p.value),
    metodo = ks$method
  )
}

#--------------------------------------------------
# Montar base conjunta BSG x MEG
#--------------------------------------------------
# Constrói uma base única com probabilidades dos dois jogos.
# A função recebe um jogador/estratégia do BSG e um jogador/estratégia
# do MEG, permitindo comparar papéis considerados análogos.
#
# Exemplo:
#   - Comprador/Rejeitar no BSG
#   - Empresa A/Entrar no MEG
#
# A saída contém as probabilidades dos dois jogos empilhadas.
build_probs_compare <- function(
    bsg_df,
    meg_df,
    jogador_bsg,
    estrategia_bsg,
    jogador_meg,
    estrategia_meg,
    modelos = c("EWA", "RL", "BL"),
    par_label = NULL
) {

  # Extrai probabilidades do BSG para todos os modelos.
  probs_bsg <- purrr::map_dfr(
    modelos,
    ~ get_probs(
      df = bsg_df,
      modelo_sel = .x,
      jogador_sel = jogador_bsg,
      estrategia_sel = estrategia_bsg,
      jogo_label = "BSG"
    )
  )

  # Extrai probabilidades do MEG para todos os modelos.
  probs_meg <- purrr::map_dfr(
    modelos,
    ~ get_probs(
      df = meg_df,
      modelo_sel = .x,
      jogador_sel = jogador_meg,
      estrategia_sel = estrategia_meg,
      jogo_label = "MEG"
    )
  )

  # Junta as probabilidades dos dois jogos na mesma base.
  out <- bind_rows(probs_bsg, probs_meg)

  # Se informado, adiciona o rótulo do par estratégico.
  # Isso permite diferenciar p1 e p2 nas análises posteriores.
  if (!is.null(par_label)) {
    out <- out %>%
      mutate(par = par_label)
  }

  out
}

#--------------------------------------------------
# KS entre BSG e MEG dentro de cada modelo
# Agregado
#--------------------------------------------------
# Compara BSG e MEG dentro de cada modelo de aprendizado.
# A comparação é feita separadamente para EWA, RL e BL.
#
# Essa versão é agregada: não separa por lambda.
run_ks_by_model <- function(df_probs, comparacao_label = NULL) {

  # Agrupa por modelo e aplica o teste KS entre os jogos.
  out <- df_probs %>%
    group_by(modelo) %>%
    group_modify(~ {
      
      # Vetor de probabilidades do BSG.
      x <- .x$prob[.x$jogo == "BSG"]
      
      # Vetor de probabilidades do MEG.
      y <- .x$prob[.x$jogo == "MEG"]
      
      # Teste KS entre as duas distribuições.
      ks_two(x, y)
    }) %>%
    ungroup()

  # Preserva o identificador do par estratégico, quando existir.
  if ("par" %in% names(df_probs)) {
    out <- out %>%
      mutate(par = unique(df_probs$par))
  }

  # Adiciona descrição textual da comparação, quando informada.
  if (!is.null(comparacao_label)) {
    out <- out %>%
      mutate(comparacao = comparacao_label, .before = 1)
  }

  out
}

#--------------------------------------------------
# KS entre dois modelos dentro de um jogo
# Agregado
#--------------------------------------------------
# Compara duas distribuições de probabilidades dentro do mesmo jogo.
#
# Exemplo:
#   - EWA vs RL no BSG
#   - EWA vs BL no MEG
#
# Essa função é útil para avaliar o quanto os modelos diferem
# entre si em um mesmo ambiente estratégico.
run_ks_between_models <- function(df_probs, jogo_sel, modelo_1, modelo_2, comparacao_label = NULL) {

  # Probabilidades do primeiro modelo.
  x <- df_probs %>%
    filter(jogo == jogo_sel, modelo == modelo_1) %>%
    pull(prob)

  # Probabilidades do segundo modelo.
  y <- df_probs %>%
    filter(jogo == jogo_sel, modelo == modelo_2) %>%
    pull(prob)

  # Aplica o teste KS e adiciona identificação do jogo e dos modelos.
  out <- ks_two(x, y) %>%
    mutate(
      jogo = jogo_sel,
      modelo_1 = modelo_1,
      modelo_2 = modelo_2,
      .before = 1
    )

  # Preserva o identificador do par estratégico, quando existir.
  if ("par" %in% names(df_probs)) {
    out <- out %>%
      mutate(par = unique(df_probs$par))
  }

  # Adiciona descrição textual da comparação, quando informada.
  if (!is.null(comparacao_label)) {
    out <- out %>%
      mutate(comparacao = comparacao_label, .before = 1)
  }

  out
}

#--------------------------------------------------
# KS entre dois modelos dentro de um jogo
# Por lambda
#--------------------------------------------------
# Aplica o teste KS entre dois modelos dentro de um mesmo jogo,
# separando a análise por valor de lambda.
#
# Essa etapa permite observar se a distância entre modelos muda
# conforme aumenta a sensibilidade do aprendizado.
run_ks_between_models_lambda <- function(df_probs, jogo_sel, modelo_1, modelo_2, comparacao_label = NULL) {

  # Filtra o jogo analisado e aplica o teste dentro de cada lambda.
  out <- df_probs %>%
    filter(jogo == jogo_sel) %>%
    group_by(lambda) %>%
    group_modify(~ {

      # Probabilidades do primeiro modelo no lambda atual.
      x <- .x %>%
        filter(modelo == modelo_1) %>%
        pull(prob)

      # Probabilidades do segundo modelo no lambda atual.
      y <- .x %>%
        filter(modelo == modelo_2) %>%
        pull(prob)

      # Teste KS para o lambda atual.
      ks_two(x, y)
    }) %>%
    ungroup() %>%
    
    # Identifica jogo e modelos comparados.
    mutate(
      jogo = jogo_sel,
      modelo_1 = modelo_1,
      modelo_2 = modelo_2,
      .before = 1
    )

  # Recupera o identificador do par estratégico por lambda, se existir.
  if ("par" %in% names(df_probs)) {
    out <- out %>%
      left_join(
        df_probs %>% distinct(lambda, par),
        by = "lambda"
      )
  }

  # Adiciona descrição textual da comparação.
  if (!is.null(comparacao_label)) {
    out <- out %>%
      mutate(comparacao = comparacao_label, .before = 1)
  }

  out
}

#--------------------------------------------------
# KS para todos os pares de modelos
# Por lambda
#--------------------------------------------------
# Gera automaticamente todas as combinações possíveis entre modelos
# e executa o teste KS por lambda.
#
# Com três modelos, as comparações geradas são:
#   - EWA vs RL
#   - EWA vs BL
#   - RL vs BL
run_ks_all_pairs_lambda <- function(df_probs, jogo_sel) {

  # Identifica os modelos presentes na base.
  modelos <- unique(df_probs$modelo)

  # Cria todas as combinações dois a dois entre modelos.
  pares <- combn(modelos, 2, simplify = FALSE)

  # Aplica o teste KS para cada par de modelos.
  purrr::map_dfr(pares, function(par) {

    m1 <- par[1]
    m2 <- par[2]

    run_ks_between_models_lambda(
      df_probs = df_probs,
      jogo_sel = jogo_sel,
      modelo_1 = m1,
      modelo_2 = m2,
      comparacao_label = paste(m1, "vs", m2, "no", jogo_sel)
    )
  })
}

#--------------------------------------------------
# Preparar resultados para análise e gráficos
#--------------------------------------------------
# Converte e reorganiza os resultados dos testes KS.
# Essa função prepara a base para resumo estatístico e visualização.
prep_ks_lambda_plot <- function(ks_lambda_all) {
  ks_lambda_all |>
    mutate(
      
      # Converte lambda de rótulo textual para número.
      lambda_num = lambda |>
        stringr::str_remove("λ =") |>
        stringr::str_trim() |>
        stringr::str_replace(",", ".") |>
        as.numeric(),

      # Remove a informação do jogo do texto da comparação.
      comparacao = stringr::str_remove(comparacao, " no BSG| no MEG"),

      # Define o papel estratégico associado a cada jogo e par.
      papel = case_when(
        jogo == "BSG" & par == "p1" ~ "Comprador",
        jogo == "BSG" & par == "p2" ~ "Vendedor",
        jogo == "MEG" & par == "p1" ~ "Empresa A",
        jogo == "MEG" & par == "p2" ~ "Empresa B",
        TRUE ~ NA_character_
      ),

      # Renomeia os jogos para aparecerem como painéis no gráfico.
      jogo = recode(
        jogo,
        "BSG" = "(a) BSG",
        "MEG" = "(b) MEG"
      ),

      # Define a ordem dos papéis nos painéis.
      papel = factor(
        papel,
        levels = c("Comprador", "Vendedor", "Empresa A", "Empresa B")
      ),

      # Cria uma variável auxiliar para destacar linhas no gráfico.
      destaque_linha = case_when(
        comparacao == "EWA vs BL" ~ "principal",

        comparacao == "EWA vs RL" &
          jogo == "(a) BSG" &
          papel == "Comprador" ~ "secundario",

        TRUE ~ "normal"
      )
    )
}

#--------------------------------------------------
# Resumo das distâncias KS por jogo, par e comparação
#--------------------------------------------------
# Resume a estatística D do KS por jogo, papel e comparação.
# O resultado mostra a distância média, mínima e máxima ao longo
# dos valores de lambda.
summarise_ks_lambda <- function(ks_lambda_all) {
  ks_lambda_all |>
    prep_ks_lambda_plot() |>
    group_by(jogo, par, papel, comparacao) |>
    summarise(
      D_medio = mean(estatistica_D, na.rm = TRUE),
      D_min = min(estatistica_D, na.rm = TRUE),
      D_max = max(estatistica_D, na.rm = TRUE),
      .groups = "drop"
    )
}

#--------------------------------------------------
# Gráfico das distâncias KS por lambda
#--------------------------------------------------
# Constrói o gráfico da estatística D do teste KS ao longo de lambda.
# Cada painel representa um jogo e um papel estratégico.
plot_ks_lambda <- function(
    ks_lambda_all,
    fonte_base = "Times New Roman",
    cores = c(
      "EWA vs BL" = color_main[4],
      "RL vs BL"  = color_main[1],
      "EWA vs RL" = color_main[2]
    )
) {

  # Prepara os dados para o gráfico.
  ks_plot <- prep_ks_lambda_plot(ks_lambda_all)

  # Seleciona os pontos finais das linhas usadas como rótulos.
  labels_linhas <- ks_plot |>
    filter(papel %in% c("Vendedor", "Empresa B")) |>
    group_by(jogo, papel, comparacao) |>
    slice_max(lambda_num, n = 1) |>
    ungroup()

  ggplot(
    ks_plot,
    aes(
      x = lambda_num,
      y = estatistica_D,
      color = comparacao,
      group = comparacao
    )
  ) +
    
    # Linha da distância KS ao longo de lambda.
    geom_line(aes(linewidth = destaque_linha), lineend = "round") +

    # Rótulos das linhas no final das trajetórias.
    ggrepel::geom_text_repel(
      data = labels_linhas,
      aes(label = comparacao),
      direction = "y",
      hjust = 0,
      nudge_x = 0.05,
      segment.color = NA,
      size = 4.5,
      fontface = "bold",
      family = fonte_base
    ) +

    # Organiza os painéis por jogo e papel estratégico.
    facet_wrap(~ jogo + papel, ncol = 2) +

    # Define cores manuais das comparações.
    scale_color_manual(values = cores) +

    # Define espessura das linhas segundo o destaque analítico.
    scale_linewidth_manual(
      values = c(
        "principal" = 1.8,
        "secundario" = 1.8,
        "normal" = 1.1
      ),
      guide = "none"
    ) +

    labs(
      x = expression(lambda),
      y = "Distância KS (D)"
    ) +

    theme_minimal(base_size = 18, base_family = fonte_base) +
    theme(
      legend.position = "none",
      plot.margin = margin(10, 70, 10, 10),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "#666666", size = 18),
      axis.text = element_text(color = "#666666", size = 16),
      strip.text = element_text(
        size = 17,
        family = fonte_base,
        color = "black"
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
    ) +

    # Permite que rótulos ultrapassem a área do painel.
    coord_cartesian(clip = "off") +

    # Configura eixo x com vírgula decimal.
    scale_x_continuous(
      breaks = c(0.1, 0.3, 0.6, 0.9, 1),
      labels = scales::label_number(decimal.mark = ","),
      expand = expansion(mult = c(0.02, 0.14))
    ) +

    # Configura eixo y com vírgula decimal.
    scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",")
    )
}

#--------------------------------------------------
# Função para preparar dados da ECDF
#--------------------------------------------------
# Amostra observações por grupo e calcula a função de distribuição
# acumulada empírica das probabilidades.
prep_ecdf <- function(df, n_por_grupo = 5000) {
  df %>%
    
    # Agrupa por modelo e jogo para preservar a estrutura das comparações.
    dplyr::group_by(modelo, jogo) %>%
    dplyr::group_modify(~ {
      
      # Garante que a amostra não ultrapasse o tamanho disponível.
      n_sample <- min(n_por_grupo, nrow(.x))

      .x %>%
        # Sorteia uma amostra para reduzir o peso computacional do gráfico.
        dplyr::slice_sample(n = n_sample) %>%
        
        # Ordena as probabilidades para construir a ECDF.
        dplyr::arrange(prob) %>%
        
        # Calcula a fração acumulada.
        dplyr::mutate(ecdf = dplyr::row_number() / dplyr::n())
    }) %>%
    dplyr::ungroup()
}

#--------------------------------------------------
# Função para gráfico ECDF individual
#--------------------------------------------------
# Constrói uma curva ECDF para um modelo específico,
# comparando BSG e MEG.
plot_ecdf_custom_tag <- function(df_modelo,
                                 tag_label,
                                 pal_jogo,
                                 fonte_base = NULL,
                                 base_size = 18) {
  ggplot(df_modelo, aes(x = prob, y = ecdf, color = jogo)) +
    
    # Desenha a função acumulada empírica em formato de degraus.
    geom_step(linewidth = 1.2) +
    
    # Fixa os limites das probabilidades e da fração acumulada.
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    
    # Aplica paleta manual para diferenciar os jogos.
    scale_color_manual(
      values = pal_jogo,
      breaks = c("BSG", "MEG"),
      labels = c("BSG", "MEG")
    ) +
    
    # Usa vírgula como separador decimal nos eixos.
    scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
    
    labs(
      x = "Probabilidade de escolha",
      y = "Fração acumulada",
      color = NULL,
      tag = tag_label
    ) +
    
    # Aplica o tema acadêmico padrão do projeto.
    theme_academic(base_size = base_size, base_family = fonte_base) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      plot.tag.position = "bottom",
      plot.tag = element_text(color = "black", margin = margin(t = 10))
    )
}

#--------------------------------------------------
# Função para montar os painéis + patchwork
#--------------------------------------------------
# Cria um conjunto de gráficos ECDF para os modelos EWA, RL e BL
# e organiza os painéis em uma linha com patchwork.
make_ecdf_subpatch <- function(df_probs,
                               prefixo = "a",
                               modelos_ordem = c("EWA", "RL", "BL"),
                               pal_jogo = c(BSG = color_main[1], MEG = color_main[2]),
                               fonte_base = NULL,
                               base_size = 18,
                               n_por_grupo = 5000,
                               preparar = TRUE) {

  # Prepara os dados, caso a base ainda não esteja amostrada.
  df_plot <- if (isTRUE(preparar)) {
    prep_ecdf(df_probs, n_por_grupo = n_por_grupo)
  } else {
    df_probs
  }

  # Cria tags internas, como (a1), (a2), (a3).
  tags_internas <- paste0("(", prefixo, 1:3, ")")

  # Gera um gráfico ECDF para cada modelo.
  plots <- purrr::map2(
  modelos_ordem,
  tags_internas,
  ~ df_plot %>%
    dplyr::filter(modelo == .x) %>%
    plot_ecdf_custom_tag(
      tag_label = paste0(.y, " ", .x),
      pal_jogo = pal_jogo,
      fonte_base = fonte_base,
      base_size = base_size
    )
)

  # Nomeia a lista de gráficos pelos modelos.
  names(plots) <- modelos_ordem

  # Junta os gráficos em uma única linha.
  subpatch <- patchwork::wrap_plots(plots, ncol = 3) &
  theme(
    legend.position = "top",
    legend.margin = margin(b = 10),
    legend.box.margin = margin(b = 10)
  )

  # Retorna dados preparados, gráficos individuais e painel composto.
  list(
    dados_plot = df_plot,
    plots = plots,
    patch = subpatch
  )
}