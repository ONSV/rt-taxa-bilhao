arrange_tabela_uf <- function(tab_uf) {
  tab_uf |> 
    pivot_longer(x2011:x2020, names_to = "ano", values_to = "taxa") |> 
    separate_wider_delim(
      cols = taxa, 
      delim = "\n", 
      names = c("taxa_bilhao", "rank")
    ) |> 
    mutate(
      ano = str_sub(ano, 2, 5),
      taxa_bilhao = as.numeric(str_replace(taxa_bilhao, ",", ".")),
      ## Insere a regiao de cada estado
      regiao = case_match(
        unidade_da_federacao,
        c(
          "Acre", "Amapá", "Amazonas", "Pará",
           "Rondônia", "Roraima", "Tocantins"
        ) ~ "Norte",
        c(
          "Alagoas", "Bahia", "Ceará", "Maranhão", 
          "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe"
        ) ~ "Nordeste",
        c(
          "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo"
        ) ~ "Sudeste",
        c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul",
        c(
          "Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal"
        ) ~ "Centro-Oeste",
        .default = ""
      ),
      rank = gsub("\\(|\\)", "", rank)
    ) |> 
    rename(uf = unidade_da_federacao)
}
