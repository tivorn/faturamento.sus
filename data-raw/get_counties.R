request_url <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios"

counties <- request_url %>%
  httr::GET() %>%
  httr::content() %>%
  as_tibble_col(column_name="municipios") %>%
  hoist(municipios,
        id_municipio = "id",
        nome_municipio = "nome",
        nome_microrregiao = c("microrregiao", "nome"),
        nome_mesorregiao = c("microrregiao", "mesorregiao", "nome"),
        id_estado = c("microrregiao", "mesorregiao", "UF", "id"),
        nome_estado = c("microrregiao", "mesorregiao", "UF", "nome")) %>%
  mutate(id_municipio = str_sub(id_municipio, 1, 6)) %>%
  select(-municipios)

