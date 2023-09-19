## load libraries tidyverse, onsvplot, ggflags, and camcorder
library(tidyverse)
library(onsvplot)
library(ggflags)
library(camcorder)

source("R/plots.R")

## Gráfico 2
taxa_modais <- make_taxa_modais_data()

modais_plot <- plot_modais(taxa_modais)

## Gráfico 3
taxa_mortes <- make_taxas_data()

taxas_plot <- plot_taxa_mortes(taxa_mortes)

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
