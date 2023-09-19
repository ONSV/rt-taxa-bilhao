## load libraries tidyverse, onsvplot, ggflags, and camcorder
library(tidyverse)
library(onsvplot)
library(ggflags)
library(camcorder)

source("R/plots.R")

## Gráfico 3
taxa_mortes <- make_tibble_data()

g3 <- plot_taxa_mortes(taxa_mortes)

## Exportando os gráficos

ggsave(g3, filename = "plot/g3.png", width = 6, height = 3.5, dpi = 300)
