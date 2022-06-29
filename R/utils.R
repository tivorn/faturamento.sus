# Helpers ----------------------------------------------------------------------

get_counties_by_state <- function(state_abbr) {
  base_url <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados"

  state_id <- base_url %>%
    httr::GET() %>%
    httr::content() %>%
    tibble() %>%
    unnest_auto(col=".") %>%
    filter(sigla == state_abbr) %>%
    pull(id)

  request_url <- str_glue("{base_url}/{state_id}/municipios")

  response_content <- request_url %>%
    httr::GET() %>%
    httr::content() %>%
    tibble() %>%
    unnest_auto(col=".") %>%
    unnest_wider(col="microrregiao", names_sep="_") %>%
    unnest_wider(col="microrregiao_mesorregiao", names_sep="_") %>%
    select(id, nome, microrregiao_nome, microrregiao_mesorregiao_nome) %>%
    rename(nm_mun = nome, id_mun = id,
           nm_micror = microrregiao_nome,
           nm_mesor = microrregiao_mesorregiao_nome) %>%
    mutate(id_mun = str_sub(id_mun, 1, 6))

  return(response_content)
}
