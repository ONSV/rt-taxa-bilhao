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

make_gt_uf <- function(tab_uf) {
  tab_uf |>
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
}

clean_tabela_6 <- function(tab_6) {
  tab_6 |> 
    pivot_longer(cols = x2011:x2020, names_to = "ano", values_to = "taxa") |> 
    mutate(
      ano = str_sub(as.character(ano), 2, 5),
      taxa = as.double(taxa),
      pais_id = case_match(
        pais,
        "Alemanha" ~ "DE",
        "Australia" ~ "AU",
        "Áustria" ~ "AT",
        "Bélgica" ~ "BE",
        "Canadá" ~ "CA",
        "Coréia do Sul" ~ "KR",
        "Dinamarca" ~ "DK",
        "Eslovênia" ~ "SI",
        "EUA" ~ "US",
        "Finlândia" ~ "FI",
        "França" ~ "FR",
        "Holanda" ~ "NL",
        "Hungria" ~ "HU",
        "Irlanda" ~ "IE",
        "Islândia" ~ "IS",
        "Israel" ~ "IL",
        "Japão" ~ "JP",
        "Malásia" ~ "MY",
        "México" ~ "MX",
        "Noruega" ~ "NO",
        "Nova Zelândia" ~ "NZ",
        "Polônia" ~ "PL",
        "Reino Unido" ~ "GB",
        "República Tcheca" ~ "CZ",
        "Suécia" ~ "SE",
        "Suíça" ~ "CH",
        "Brasil" ~ "BR",
      )
    )
}

make_gt_decada <- function(tab_decada) {
  tab_decada |>
    pivot_wider(names_from = ano, values_from = taxa) |>
    select(pais_id, pais, starts_with("20"), diferenca_decada) |>
    gt() |> 
    fmt_flag(columns = pais_id) |> 
    fmt_number(
      columns = starts_with("20"),
      decimals = 2,
      dec_mark = ",",
      sep_mark = "."
    ) |> 
    fmt_percent(
      columns = diferenca_decada,
      decimals = 2,
      dec_mark = ",",
      sep_mark = "."
    ) |> 
    sub_missing(
      columns = starts_with("20"),
      missing_text = "-"
    ) |> 
    cols_label(
      pais_id = "",
      pais = "País",
      diferenca_decada = "Variação"
    ) |> 
    cols_align(
      columns = diferenca_decada,
      align = "right"
    ) |> 
    tab_footnote(
      footnote = "Com base no período disponível",
      locations = cells_column_labels(columns = diferenca_decada)
    ) |> 
    tab_style(
      style = cell_text(
        weight = "bold"
      ),
      locations = cells_body(rows = nrow(tabela_6))
    ) |> 
    tab_style(
      style = cell_borders(
        sides = "left",
        color = "grey80",
        weight = px(2)
      ),
      locations = cells_body(
        columns = diferenca_decada
      )
    ) |> 
    data_color(
      columns = starts_with("20"),
      palette = "Reds",
      na_color = "white"
    ) |> 
    data_color(
      columns = diferenca_decada,
      palette = "BrBG",
      domain = c(-1, 1),
      reverse = "TRUE"
    )
}
