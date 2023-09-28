## Setup
library(tidyverse)
library(onsvplot)
library(ggflags)
library(camcorder)
library(readODS)
library(gt)

source("R/plots.R")
source("R/table.R")

taxa_paises <- read_ods("data/taxa_paises.ods")

tabela3 <- read_ods("data/tabela3.ods") |>
  janitor::clean_names()

tabela_6 <- read_ods("data/tabela6.ods") |>
  janitor::clean_names()

tabela_7 <- read_ods("data/tabela7.ods") |> 
  janitor::clean_names()

tabela_8 <- read_ods("data/tabela8.ods") |> 
  janitor::clean_names()

theme_set(theme_onsv())

# Gráficos ----

## Gráfico 2
taxa_modais <- make_taxa_modais_data()

modais_plot <- plot_modais(taxa_modais)

## Gráfico 3
taxa_mortes <- make_taxas_data()

taxas_plot <- plot_taxa_mortes(taxa_mortes)

## Gráfico 4

taxa_alta <- calc_taxa_alta(taxa_paises)
taxa_alta_plot <- plot_taxa_alta(taxa_alta)

## Exportando os gráficos

ggsave(
  taxas_plot,
  filename = "plot/fig2.png", 
  width = 6, 
  height = 3.5, 
  dpi = 300
)

ggsave(
  modais_plot,
  filename = "plot/fig3.png",
  width = 6,
  height = 4,
  dpi = 300
)

ggsave(
  taxa_alta_plot,
  filename = "plot/fig4.png",
  width = 6,
  height = 4,
  dpi = 300
)

# Tabelas ----

## Tabela 3

tabela_uf <- arrange_tabela_uf(tabela3) |> 
  add_regiao(uf)

gt_table_3 <- make_gt_uf(tabela_uf)

## Tabela 6

tabela_decada <- clean_tabela_6(tabela_6) |> 
  add_pais_id(pais)

gt_table_6 <- make_gt_decada(tabela_decada)

## Tabela 7

tabela_var <- tabela_7 |> 
  arrange_tabela_7() |>
  add_pais_id(pais)

gt_table_7 <- make_gt_var(tabela_var)

## Tabela 8

tabela_comparacao <- tabela_8 |> 
  arrange_tabela_8() |> 
  add_pais_id(pais) |> 
  add_regiao(uf)

gt_table_8 <- make_gt_comparacao(tabela_comparacao)  

## Exportando as tabelas

gt_tables <- list(
  gt_table_3,
  gt_table_6,
  gt_table_7,
  gt_table_8
)

tables_path <- paste0("table/tab", c(3, 6, 7, 8), ".png")

walk2(gt_tables, tables_path, gtsave, vwidth = 1800, vheight = 2400)
