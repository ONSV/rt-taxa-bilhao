## load libraries tidyverse, onsvplot, ggflags, and camcorder
library(tidyverse)
library(onsvplot)
library(ggflags)
library(camcorder)

## Set theme_onsv as ggplot theme
theme_set(theme_onsv())

## Make a tibble with country data

paises <- c(
  "Noruega", "Islândia", "Suécia", "Suíça", "Irlanda", "Dinamarca",
  "Alemanha", "Austrália", "Eslovênia", "Finlânda", "Canadá", "França", 
  "Austria", "Japão", "Israel", "Estados Unidos", "Nova Zelândia",
  "República Tcheca", "Coreia do Sul", "Polônia", "Hungria", "México",
  "Brasil"
)

paises_siglas <- c(
  "no", "is", "se", "ch", "ie", "dk", "de", "au", "es", "fi", "ca", "fr",
  "at", "jp", "is", "us", "nz", "cz", "kr", "pl", "hu", "mx", "br")

taxa_mortes <- c(
  2.1, 2.1, 2.6, 2.7, 3, 3.6, 4.0, 4.0, 4.5, 4.6, 4.7, 4.8, 4.9, 5.3, 5.6, 6.9,
  7.2, 9.9, 10.2, 11.7, 12.8, 23.9, 32.7
)

taxa_mortes <- tibble(
  pais = paises,
  n = taxa_mortes,
  sigla = paises_siglas
)

## Start gg_record
gg_record(width = 6, height = 3.5, dpi = 300)

## Make a ggplot of the data with segment and points
g2 <- ggplot(
  data = taxa_mortes %>% mutate(pais = fct_reorder(pais, -n)), 
  aes(x = pais, y = n, country = sigla)
) +
  geom_segment(aes(xend = pais, yend = 0), color = "grey70", size = 0.4) +
  geom_flag(size = 4) + 
  geom_text(
    aes(label = scales::number(
      n, 
      accuracy = 0.1, 
      decimal.mark = ",", 
      big.mark = ".")
    ), 
    size = 2.5, 
    nudge_y = 1.6,
    color = "grey20"
  ) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  ## Remove x grid axis
  theme(panel.grid.major.y = element_blank())

g2

## Save g2 in plot/

ggsave(g2, filename = "plot/g2.png", width = 6, height = 3.5, dpi = 300)




