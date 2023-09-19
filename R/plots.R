
## Set theme_onsv as ggplot theme
theme_set(theme_onsv())

## Make a tibble with type of transport data

make_taxa_modais_data <- function() {
  taxa_mortes_modais <- tibble(
    ano = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    Automóvel = c(
      23.22, 22.6, 17.93, 20.4, 17.64, 15.86, 15.6, 14.25, 12.78, 13.17
    ),
    Motocicleta = c(
      47.85, 46.47, 41.03, 46.74, 42.44, 51.5, 44.44, 39.91, 36.06, 44.33
    ),
    Caminhão = c(8.64, 8.35, 6.24, 6.5, 6.05, 6.36, 7.3, 7.19, 6.64, 6.12),
    Ônibus = c(3.34, 3.14, 2.5, 3.08, 3.12, 1.13, 2.25, 1.76, 1.89, 1.44)
  )

  taxa_mortes_modais %>% 
    pivot_longer(-ano, names_to = "modal", values_to = "taxa")
}

## Plot taxa modais

plot_modais <- function(taxa_modais) {
  ggplot(
    taxa_modais %>% mutate(modal = fct_reorder(modal, -taxa)), 
    aes(x = ano, y = taxa, fill = modal)
  ) +
    geom_col(alpha = 0.8, aes(fill = modal), position = "dodge") +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_x_reverse(breaks = seq(2011, 2020)) +
    scale_discrete_onsv() +
    theme_onsv(basesize = 8) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.margin = margin(0, 0, 0, -30),
      legend.key.size = unit(0.3, "cm")
    ) +
    guides(
      color = guide_legend(reverse = TRUE), 
      fill = guide_legend(reverse = TRUE)
    )
}


## Make a tibble with country data

make_taxas_data <- function() {
  paises <- c(
    "Noruega", "Islândia", "Suécia", "Suíça", "Irlanda", "Dinamarca",
    "Alemanha", "Austrália", "Eslovênia", "Finlânda", "Canadá", "França", 
    "Austria", "Japão", "Israel", "Estados Unidos", "Nova Zelândia",
    "República Tcheca", "Coreia do Sul", "Polônia", "Hungria", "México",
    "Brasil"
  )

  paises_siglas <- c(
    "no", "is", "se", "ch", "ie", "dk", "de", "au", "es", "fi", "ca", "fr",
    "at", "jp", "is", "us", "nz", "cz", "kr", "pl", "hu", "mx", "br"
  )

  taxa_mortes <- c(
    2.1, 2.1, 2.6, 2.7, 3, 3.6, 4.0, 4.0, 4.5, 4.6, 4.7, 4.8, 4.9, 5.3, 5.6, 
    6.9, 7.2, 9.9, 10.2, 11.7, 12.8, 23.9, 32.7
  )

  taxa_mortes <- tibble(
    pais = paises,
    n = taxa_mortes,
    sigla = paises_siglas
  )

  return(taxa_mortes)
}

## Make a ggplot of the data with segment and points

plot_taxa_mortes <- function(taxa_mortes) {
  ggplot(
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
        big.mark = "."
      )), 
      size = 2.5, 
      nudge_y = 1.6,
      color = "grey20"
    ) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    ## Remove x grid axis
    theme(panel.grid.major.y = element_blank())
  
}