# Ativando pacotes --------------------------------------------------------
# install.packages("bibliometrix")

library(bibliometrix)

# install.packages("dplyr")
library(tidyverse)

# install.packages("openxlsx")
library(openxlsx)

# Importando dados --------------------------------------------------------
scopus <- convert2df(file = "bibliometrix/dados/scopus.csv", dbsource = "scopus",
                     format = "csv")

wos <- convert2df(file = "bibliometrix/dados/savedrecs.txt", dbsource = "wos",
                  format = "plaintext")

# JUNTANDO AS DUAS BASES --------------------------------------------------
#prod_cientifica <- mergeDbSources(scopus, wos, remove.duplicated = TRUE)

keywords <- wos |>
            select(SC, DE, TI, PY) |>
            tidyr::separate_rows(SC, sep = "; ") |>
            tidyr::separate_rows(DE, sep = "; ") |>
            #dplyr::distinct(SC) |>
            group_by(SC, DE) |>
            mutate(n = n())

keywords <- wos |>
  select(SC) |>
  tidyr::separate_rows(SC, sep = "; ") |>
  #dplyr::distinct(SC) |>
  group_by(SC) |>
  mutate(n = n())

              #knitr::kable(row.names = F)

# SÍNTESE DOS RESULTADOS --------------------------------------------------

resultados <- biblioAnalysis(wos)

resumo <- summary(object = resultados, k = 10)

plots <- plot(resultados, k = 10)

# EXPORTANDO BASE DE DADOS PARA LEITURA NO BIBLIOSHINY --------------------

openxlsx::write.xlsx(x = wos, file = "bibliometrix/dados/dados.xlsx", rowNames = FALSE)

artigos_recentes <- wos |>
                      filter(PY > 2020)

round((nrow(artigos_recentes) / nrow(wos)) * 100, 2)

contexto_estudo <- wos |> filter(
                             str_detect(TI, "BUYER") & str_detect(TI, "SELLER") |
                             str_detect(AB, "BUYER") & str_detect(AB, "SELLER") |
                             str_detect(DE, "BUYER") & str_detect(DE, "SELLER") &
                             str_detect(TI, "MARKET") & str_detect(TI, "ENTRY") |
                             str_detect(AB, "MARKET") & str_detect(AB, "ENTRY") |
                             str_detect(DE, "MARKET") & str_detect(DE, "ENTRY"))


