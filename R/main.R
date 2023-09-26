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
  filename = "plot/taxas_plot.png", 
  width = 6, 
  height = 3.5, 
  dpi = 300
)

ggsave(
  modais_plot,
  filename = "plot/modais_plot.png",
  width = 6,
  height = 4,
  dpi = 300
)

ggsave(
  taxa_alta_plot,
  filename = "plot/taxa_alta_plot.png",
  width = 6,
  height = 4,
  dpi = 300
)

# Tabelas ----

## Tabela 3

tabela_uf <- arrange_tabela_uf(tabela3)

gt_table_3 <- tabela_uf |> 
  pivot_wider(names_from = ano, values_from = c(taxa_bilhao, rank)) |> 
  group_by(regiao) |> 
  gt(rowname_col = "uf") |> 
  tab_spanner(
    label = "2011",
    columns = ends_with("2011")
  ) |> 
  tab_spanner(
    label = "2012",
    columns = ends_with("2012")
  ) |> 
  tab_spanner(
    label = "2013",
    columns = ends_with("2013")
  ) |> 
  tab_spanner(
    label = "2014",
    columns = ends_with("2014")
  ) |> 
  tab_spanner(
    label = "2015",
    columns = ends_with("2015")
  ) |> 
  tab_spanner(
    label = "2016",
    columns = ends_with("2016")
  ) |> 
  tab_spanner(
    label = "2017",
    columns = ends_with("2017")
  ) |> 
  tab_spanner(
    label = "2018",
    columns = ends_with("2018")
  ) |> 
  tab_spanner(
    label = "2019",
    columns = ends_with("2019")
  ) |> 
  tab_spanner(
    label = "2020",
    columns = ends_with("2020")
  ) |> 
  cols_label(
    starts_with("taxa") ~ "",
    starts_with("rank") ~ ""
  ) |>   
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |> 
  cols_align(
    align = "center",
    columns = starts_with("rank")
  ) |> 
  data_color(
    columns = starts_with("taxa"),
    method = "numeric",
    palette = "viridis"
  ) |> 
  fmt_number(
    columns = starts_with("taxa"),
    dec_mark = ",",
    sep_mark = "."
  )

gtsave(gt_table_3, "table/gt_table_3.png", vwidth = 1800, vheight = 2400)
