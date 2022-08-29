#' Create SUS-SIA/SIH database
#'
#' @description
#' [create_output] preprocess microdata files from SIA and SIH information systems (DATASUS)
#' and match with CNES and SIGTAP information.
#'
#' @param year_start Ano de início da realização do procedimento
#' @param month_start Mês de início da realização do procedimento
#' @param year_end Ano de término da realização do procedimento
#' @param month_end Mês de término da realização do procedimento
#' @param state_abbr Sigla da Unidade Federativa
#' @param county_id Código(s) do Município de Atendimento
#' @param health_establishment_id Código(s) do estabelecimento de saúde
#'
#' @examples
#' \dontrun{create_output(year_start=2021,
#'                   month_start=1,
#'                   year_end=2021,
#'                   month_end=3,
#'                   state_abbr="CE",
#'                   county_id="230440",
#'                   health_establishment_id=c("2561492","2481286")
#'                   )}


#' @export
create_output <- function(year_start, month_start,
                          year_end, month_end, state_abbr,
                          county_id="all",
                          health_establishment_id="all") {

  outputSIA <- get_datasus(
    year_start,
    month_start,
    year_end,
    month_end,
    state_abbr,
    county_id,
    information_system = "SIA",
    health_establishment_id
  )

  outputSIH_AIH <- get_datasus(
    year_start,
    month_start,
    year_end,
    month_end,
    state_abbr,
    county_id,
    information_system = "SIH-AIH",
    health_establishment_id
  )

  outputSIH_SP <- get_datasus(
    year_start,
    month_start,
    year_end,
    month_end,
    state_abbr,
    county_id,
    information_system = "SIH-SP",
    health_establishment_id
  )

  write.csv2(outputSIA, "outputSIA.csv", na="", row.names=FALSE)
  write.csv2(outputSIH_AIH, "outputSIH_AIH.csv", na="", row.names=FALSE)
  write.csv2(outputSIH_SP, "outputSIH_SP.csv", na="", row.names=FALSE)

  return(list(outputSIA, outputSIH_AIH, outputSIH_SP))
}

#' Create SUS-SIA/SIH database from local DBC files
#'
#' @description
#' [create_output_from_local] preprocess DBC files from SIA and SIH information systems (DATASUS)
#' and match with CNES and SIGTAP information.
#'

#' @param dbc_dir_path Diretório que contêm os arquivos DBC
#' @param health_establishment_id Código(s) do estabelecimento de saúde
#'
#' @examples
#' \dontrun{create_output(tempdir(),
#'                        health_establishment_id=c("2561492","2481286"),
#'                        county_id="230440",
#'                        )}


#' @export
create_output_from_local <- function(dbc_dir_path, health_establishment_id, county_id) {
  outputSIA <- get_datasus_from_local(
    dbc_dir_path,
    information_system = "SIA",
    county_id,
    health_establishment_id
  )

  outputSIH_AIH <- get_datasus_from_local(
    dbc_dir_path,
    information_system = "SIH-AIH",
    county_id,
    health_establishment_id
  )

  outputSIH_SP <- get_datasus_from_local(
    dbc_dir_path,
    information_system = "SIH-SP",
    county_id,
    health_establishment_id
  )

  write.csv2(outputSIA, "outputSIA.csv", na="", row.names=FALSE)
  write.csv2(outputSIH_AIH, "outputSIH_AIH.csv", na="", row.names=FALSE)
  write.csv2(outputSIH_AIH, "outputSIH_SP.csv", na="", row.names=FALSE)

  return(list(outputSIA, outputSIH_AIH, outputSIH_SP))
}





