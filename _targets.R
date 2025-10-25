# _targets.R (opcional)
library(targets)
library(here)

tar_option_set(
  packages = c(
    "tidyverse", "rgamer", "ggplot2", "dplyr", "readr",
    "janitor", "glue", "patchwork", "paletteer", "ggtext",
    "beautyxtrar", "extrafont", "showtext", "sysfonts",
    "kableExtra", "webshot", "webshot2", "flextable", "magick", "grid", 
    "shiny", "bslib"
  )
)

list(
  # Executa o script principal
  tar_target(run_main, {
    source(here::here("main.R"))
    # lista os arquivos que o main.R gera
    c(
      here::here("figuras","figura_3.pdf"),
      here::here("figuras","figura_4.pdf"),
      here::here("figuras","figura_18.pdf"),
      here::here("figuras","figura_19.pdf"),
      here::here("figuras","figura_20.pdf"),
      here::here("figuras","figura_20a.pdf"),
      here::here("figuras","figura_20b.pdf"),
      here::here("figuras","figura_21.pdf"),
      here::here("figuras","figura_22.pdf"),
      here::here("tabelas","tabela_1.pdf"),
      here::here("tabelas","tabela_2.pdf"),
      here::here("tabelas","tabela_3.png"),
      here::here("tabelas","tabela_3.tex"),
      here::here("tabelas","tabela_4.png"),
      here::here("tabelas","tabela_4.tex"),
      here::here("tabelas","tabela_5.png"),
      here::here("tabelas","tabela_5.tex"),
      here::here("tabelas","tabela_6.png"),
      here::here("tabelas","tabela_6.tex"),
      here::here("tabelas","tabela_7.png"),
      here::here("tabelas","tabela_7.tex"),
      here::here("tabelas","tabela_8.png"),
      here::here("tabelas","tabela_8.tex"),
      here::here("tabelas","tabela_9.tex")
    )
  }, format = "file")
)