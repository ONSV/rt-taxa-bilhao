## Setup
library(tidyverse)
library(onsvplot)
library(ggflags)
library(camcorder)
library(readODS)

source("R/plots.R")

taxa_paises <- read_ods("data/taxa_paises.ods")

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
