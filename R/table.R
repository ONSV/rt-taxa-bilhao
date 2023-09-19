library(tidyverse)
library(readODS)
library(gt)

tabela3 <- read_ods("data/tabela3.ods") %>%
  janitor::clean_names()

## pivot longer across all years

tab_uf <- tabela3 %>% 
  pivot_longer(x2011:x2020, names_to = "ano", values_to = "taxa") %>% 
  separate_wider_delim(
    cols = taxa, 
    delim = "\n", 
    names = c("taxa_bilhao", NA)
  ) %>% 
  mutate(
    ano = str_sub(ano, 2, 5),
    taxa_bilhao = as.numeric(str_replace(taxa_bilhao, ",", ".")),
    ## Insere a regiao de cada estado
    regiao = case_match(
      unidade_da_federacao,
      c(
        "Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"
      ) ~ "Norte",
      c(
        "Alagoas", "Bahia", "Ceará", "Maranhão", 
        "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe"
      ) ~ "Nordeste",
      c(
        "Espirito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo"
      ) ~ "Sudeste",
      c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul",
      c(
        "Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal"
      ) ~ "Centro-Oeste"
    )
  ) %>% 
  rename(uf = unidade_da_federacao)

## Make a ranking per 'ano' on tab_uf:

tab_ranking <- tab_uf %>% 
  filter(uf != "Brasil") %>% 
  group_by(ano) %>% 
  mutate(pos = rank(taxa_bilhao))

tab_ranking %>% 
  filter(regiao == "Norte") %>%
  select(-regiao) %>% 
  pivot_wider(names_from = ano, values_from = c(taxa_bilhao, pos)) %>% 
  gt()
