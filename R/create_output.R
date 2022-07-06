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
#' @param health_establishment_id Código(s) do estabelecimento de saúde
#'
#' @examples
#' \dontrun{create_output(year_start=2021,
#'                   month_start=1,
#'                   year_end=2021,
#'                   month_end=3,
#'                   state_abbr="CE",
#'                   health_establishment_id=c("2561492","2481286")
#'                   )}


#' @export
create_output <- function(year_start, month_start,
                          year_end, month_end, state_abbr,
                          health_establishment_id="all",
                          chunk_size) {

  outputSIA <- get_datasus(
    year_start,
    month_start,
    year_end,
    month_end,
    state_abbr,
    information_system = "SIA",
    health_establishment_id,
    chunk_size
  )

  outputSIH <- get_datasus(
    year_start,
    month_start,
    year_end,
    month_end,
    state_abbr,
    information_system = "SIH",
    health_establishment_id,
    chunk_size
  )

  write.csv2(outputSIA, "outputSIA.csv", na="", row.names=FALSE)
  write.csv2(outputSIH, "outputSIH.csv", na="", row.names=FALSE)

  return(list(outputSIA, outputSIH))
}





