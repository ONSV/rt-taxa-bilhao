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
      rank = gsub("\\(|\\)", "", rank)
    ) |> 
    rename(uf = unidade_da_federacao)
}

add_regiao <- function(uf_table, uf) {
  uf_table |>
    mutate(
      regiao = case_match(
        {{ uf }},
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
      )
    )
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
      palette = "Oranges"
    ) |> 
    fmt_number(
      columns = starts_with("taxa"),
      dec_mark = ",",
      sep_mark = "."
    )
}

add_pais_id <- function(table, pais_col) {
  table |>
    mutate(pais_id = case_match(
      {{ pais_col }},
      "Alemanha" ~ "DE",
      c("Australia", "Austrália") ~ "AU",
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
      "Brasil" ~ "BR"      
    ))
}

clean_tabela_6 <- function(tab_6) {
  tab_6 |> 
    pivot_longer(cols = x2011:x2020, names_to = "ano", values_to = "taxa") |> 
    mutate(
      ano = str_sub(as.character(ano), 2, 5),
      taxa = as.double(taxa)
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
      palette = "Oranges",
      na_color = "white"
    ) |> 
    data_color(
      columns = diferenca_decada,
      palette = "BrBG",
      domain = c(-1, 1),
      reverse = "TRUE"
    )
}

arrange_tabela_7 <- function(tab_7) {
  tab_7 |> 
    pivot_longer(
      cols = x1970:x2020,
      names_to = "ano",
      values_to = "taxa"
    ) |> 
    mutate(
      ano = as.numeric(str_sub(ano, 2, 5)),
      taxa = if_else(taxa == "-", NA, taxa),
      variacao_total = if_else(variacao_total == "-", NA, variacao_total),
      variacao_total = as.double(variacao_total),
      taxa = str_replace_all(taxa, ",", ".")
    ) |> 
    separate_wider_delim(
      taxa, 
      delim = " (", 
      names = c("taxa", "var"), 
      too_few = "align_start"
    ) |> 
    mutate(
      taxa = as.numeric(taxa),
      var = as.numeric(str_remove(var, "%\\)")) / 100
    )
}

make_gt_var <- function(table_var) {
  table_var |> 
    pivot_wider(
    names_from = ano,
    values_from = c("taxa", "var")
    ) |> 
    select(
      pais_id, 
      pais, 
      starts_with("taxa"), 
      starts_with("var_"), 
      variacao_total
    ) |> 
    gt() |>
    fmt_flag(
      columns = pais_id
    ) |> 
    sub_missing(
      columns = taxa_1970:variacao_total,
      missing_text = ""
    ) |> 
    fmt_number(
      columns = starts_with("taxa"),
      decimals = 1,
      dec_mark = ",",
      sep_mark = "."
    ) |> 
    fmt_percent(
      columns = var_1970:variacao_total,
      decimals = 1,
      dec_mark = ",",
      sep_mark = "."
    ) |> 
    tab_spanner(
      label = "Taxa",
      columns = starts_with("taxa")
    ) |> 
    tab_spanner(
      label = "Variação na década",
      columns = starts_with("var_")
    ) |> 
    cols_label(
      pais_id = "",
      pais = "País",
      taxa_1970 = "1970",
      taxa_1980 = "1980",
      taxa_1990 = "1990",
      taxa_2000 = "2000",
      taxa_2010 = "2010",
      taxa_2020 = "2020",
      var_1970 = "1970",
      var_1980 = "1980",
      var_1990 = "1990",
      var_2000 = "2000",
      var_2010 = "2010",
      var_2020 = "2020",
      variacao_total = "Variação" 
    ) |> 
    tab_style(
      style = cell_text(
        weight = "bold"
      ),
      locations = cells_body(rows = nrow(tabela_7))
    ) |> 
    tab_footnote(
      footnote = "Com base no período disponível",
      locations = cells_column_labels(columns = variacao_total)
    ) |> 
    data_color(
      columns = starts_with("taxa"),
      palette = "Oranges",
      na_color = "white"
    ) |> 
    data_color(
      columns = var_1980:variacao_total,
      palette = "Greens",
      na_color = "white",
      domain = c(-1, 0),
      reverse = "TRUE"
    ) |> 
    cols_hide(columns = var_1970)
}

arrange_tabela_8 <- function(table_8) {
  table_8 |> 
    rename(taxa = mortes_por_bilhao_km_2020, pais_eq = taxa_equivalente) |> 
    mutate(pais = str_sub(pais_eq, 1, -8))
}

make_gt_comparacao <- function(table_comparacao) {
  table_comparacao |> 
    select(uf, taxa, pais_id, pais_eq, anos_de_atraso, pais, regiao) |> 
    group_by(regiao) |> 
    gt(rowname_col = "uf") |> 
    cols_hide(columns = pais) |> 
    fmt_flag(columns = pais_id) |> 
    cols_merge(columns = c("pais_id", "pais_eq")) |> 
    cols_label(
      uf = "Unidade da Federação",
      taxa = "Taxa",
      pais_id = "País equivalente (ano)",
      anos_de_atraso = "Anos de atraso"
    ) |> 
    tab_style(
      style = cell_text(
        weight = "bold"
      ),
      locations = cells_body(rows = nrow(tabela_comparacao))
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |> 
    fmt_number(columns = taxa, dec_mark = ",", sep_mark = ".") |> 
    data_color(
      columns = taxa,
      palette = "Oranges"
    ) |> 
    cols_align(columns = anos_de_atraso, align = "center")
}
