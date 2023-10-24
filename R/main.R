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

tabela_1 <- read_ods("data/tabela1.ods") |> 
  janitor::clean_names()

tabela_2 <- read_ods("data/tabela2.ods") |>
  janitor::clean_names()

tabela_3 <- read_ods("data/tabela3.ods") |> 
  janitor::clean_names()

tabela_4 <- read_ods("data/tabela4.ods") |> 
  janitor::clean_names()

tabela_5 <- read_ods("data/tabela5.ods") |>
  janitor::clean_names()

tabela_6 <- read_ods("data/tabela6.ods") |> 
  janitor::clean_names()

tabela_7 <- read_ods("data/tabela7.ods") |> 
  janitor::clean_names()

theme_set(theme_onsv())

# Gráficos ----

## Gráfico 1

taxa_decada <- make_taxa_decada()

graf1 <- plot_taxa_decada(taxa_decada)    

## Gráfico 2
taxa_modais <- make_taxa_modais_data()

modais_plot <- plot_modais(taxa_modais)

## Gráfico 3
taxa_mortes <- make_taxas_data()

taxas_plot <- plot_taxa_mortes(taxa_mortes, flag_size = 5)

## Gráfico 4

taxa_alta <- calc_taxa_alta(taxa_paises)
taxa_alta_plot <- plot_taxa_alta(taxa_alta)

## Exportando os gráficos

ggsave(
  graf1,
  filename = "plot/graf1.png",
  width = 6,
  height = 3.5,
  dpi = 300
)

ggsave(
  taxas_plot,
  filename = "plot/graf3.png", 
  width = 7, 
  height = 5, 
  dpi = 300
)

ggsave(
  modais_plot,
  filename = "plot/graf2.png",
  width = 6,
  height = 4,
  dpi = 300
)

ggsave(
  taxa_alta_plot,
  filename = "plot/graf4.png",
  width = 6,
  height = 4,
  dpi = 300
)

# Tabelas ----

## Tabela 1

gt_table_1 <- make_gt_tab1(tabela_1)

## Tabela 2

tabela_uf <- arrange_tabela_uf(tabela_2) |> 
  add_regiao(uf)

gt_table_2 <- make_gt_uf(tabela_uf)

## Tabela 3

gt_table_3 <- make_gt_tab3(tabela_3)

## Tabela 4

gt_table_4 <- make_gt_tab3(tabela_4)

## Tabela 5

tabela_decada <- clean_tabela_5(tabela_5) |> 
  add_pais_id(pais)

gt_table_5 <- make_gt_decada(tabela_decada)

## Tabela 6

tabela_var <- tabela_6 |> 
  arrange_tabela_6() |>
  add_pais_id(pais)

gt_table_6 <- make_gt_var(tabela_var)

## Tabela 7

tabela_comparacao <- tabela_7 |> 
  arrange_tabela_7() |> 
  add_pais_id(pais) |> 
  add_regiao(uf)

gt_table_7 <- make_gt_comparacao(tabela_comparacao)

gt_table_7

## Exportando as tabelas

gt_tables <- list(
  gt_table_1,
  gt_table_2,
  gt_table_3,
  gt_table_4,
  gt_table_5,
  gt_table_6,
  gt_table_7
)

tables_path <- paste0("table/tab", seq(1, 7, 1), ".png")

walk2(gt_tables, tables_path, gtsave, vwidth = 1800, vheight = 2400)
