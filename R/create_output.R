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
#' @param uf Sigla da Unidade Federativa
#' @param health_establishment_id Código(s) do estabelecimento de saúde
#'
#' @examples
#' \dontrun{create_output(year_start=2021,
#'                   month_start=1,
#'                   year_end=2021,
#'                   month_end=3,
#'                   uf="CE",
#'                   health_establishment_id=c("2561492","2481286")
#'                   )}


#' @export
create_output <- function(year_start, month_start,
                          year_end, month_end, uf,
                          health_establishment_id) {

  counties <- get_counties_by_state(uf)

  procedure_start = str_glue("{year_start}-{month_start}")

  # ---- Produção Ambulatorial ---- #

  raw_SIA <- fetch_datasus(year_start,
                           month_start,
                           year_end,
                           month_end,
                           uf,
                           information_system = "SIA-PA")

  outputSIA <- raw_SIA %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    select(cnes = pa_coduni,
           anomes_mvm = pa_mvm,
           anomes_cmp = pa_cmp,
           proc_cod = pa_proc_id,
           proc_tipo_financ_cod = pa_tpfin,
           proc_sub_financ_cod = pa_subfin,
           proc_complex_cod = pa_nivcpl,
           qtd = pa_qtdpro,
           qtd_apr = pa_qtdapr,
           val = pa_valpro,
           val_apr = pa_valapr,
           docorig = pa_docorig,
           cbo = pa_cbocod,
           cnpj_executor = pa_cnpjcpf,
           id_mun = pa_munpcn)   %>%
    left_join(counties, by="id_mun") %>%
    mutate(across(c(anomes_mvm, anomes_cmp),ym),
           qtd_ato_prof = qtd,
           base = "Ambulatorial",
           id_uf = str_sub(id_mun, 1, 2),
           across(c(nm_mun, nm_micror, nm_mesor),
                  ~ case_when(id_uf == 23 ~ .x,
                              id_uf == 99 ~ "Não informado",
                              id_uf != 23 ~ "Outros")),
           proc_registro = case_when(docorig == "C" ~ "BPA - Consolidado",
                                     docorig == "I" ~ "BPA - Individualizado",
                                     docorig == "P" ~ "APAC - Procedimento Principal",
                                     docorig == "S" ~ "APAC - Procedimento Secundário"),
           proc_sub_financ_cod = if_else(proc_sub_financ_cod == "0000", NA_character_,
                                         str_c(proc_tipo_financ_cod, proc_sub_financ_cod)),
           ano_mvm = year(anomes_mvm),
           mes_mvm = month(anomes_mvm),
           ano_cmp = year(anomes_cmp),
           mes_cmp = month(anomes_cmp),
           proc_grupo_cod = str_sub(proc_cod, 1, 2),
           proc_sub_grupo_cod = str_sub(proc_cod, 3, 4),
           proc_forma_org_cod = str_sub(proc_cod, 5, 6)) %>%
    filter(cnes %in% health_establishment_id,
           anomes_cmp >= ym(procedure_start)) %>%
    mutate(cbo = as.character(cbo), cnes = as.character(cnes)) %>%
    left_join(cnes_df, by=c("cnes" = "CNES")) %>%
    left_join(procedure_details, by = c("proc_cod" = "CO_PROCEDIMENTO")) %>%
    left_join(cbo, by=c("cbo" = "CO_OCUPACAO")) %>%
    mutate(nm_cbo = str_c(as.character(cbo), NO_OCUPACAO, sep="-"),
           estabelecimento = str_c(cnes, FANTASIA, sep="-")) %>%
    select(Procedimentos = NO_PROCEDIMENTO,
           Grupo = NO_GRUPO,
           `Sub-grupo` = NO_SUB_GRUPO,
           `Tipo de financiamento` = NO_FINANCIAMENTO,
           `Tipo de sub-financiamento` = NO_SUB_FINANCIAMENTO,
           Complexidade = complexidade,
           `Forma de organização` = NO_FORMA_ORGANIZACAO,
           ` Qtde. produzida` = qtd,
           ` Qtde. aprovada` = qtd_apr,
           ` Financeiro produzido` = val,
           ` Financeiro aprovado` = val_apr,
           `Mês/Ano de proces.` = anomes_mvm,
           `Ano de proces` = ano_mvm,
           `Mês de realiz.` = mes_cmp,
           `Mês/Ano de realiz.` = anomes_cmp,
           `Ano de realiz.` = ano_cmp,
           `Mês de process.` = mes_mvm,
           `Tipo de registro` = proc_registro,
           `CBO do profissional` = nm_cbo,
           Estabelecimento = estabelecimento,
           `Município` = nm_mun,
           `Microrregião` = nm_micror,
           `Mesorregião` = nm_mesor
    )

  # ---- Produção Hospitalar ---- #

  raw_RD <- fetch_datasus(year_start,
                          month_start,
                          year_end,
                          month_end,
                          uf,
                          information_system="SIH-RD")

  raw_RJ <- fetch_datasus(year_start,
                          month_start,
                          year_end,
                          month_end,
                          uf="CE",
                          information_system="SIH-RJ")

  outputSIH <- raw_RD %>%
    as_tibble() %>%
    bind_rows(raw_RJ, .id = "TIPO") %>%
    mutate(TIPO = ifelse(TIPO == "1", "Aprovada", "Rejeitada"),
           DT_CMPT = ym(str_c(ANO_CMPT, MES_CMPT, sep="-")),
           QTD_AIH = 1) %>%
    filter(DT_CMPT >= ym(procedure_start),
           CNES %in% health_establishment_id) %>%
    left_join(counties, by=c("MUNIC_RES" = "id_mun")) %>%
    left_join(cnes_df, by="CNES") %>%
    left_join(procedure_details, by = c("PROC_REA" = "CO_PROCEDIMENTO")) %>%
    mutate(CNES = str_c(CNES, FANTASIA, sep="-")) %>%
    select(`Situação da AIH` = TIPO,
           `Ano de processamento` = ANO_CMPT,
           `Mês de processamento` = MES_CMPT,
           `Mês/Ano de processamento` = DT_CMPT,
           Estabelecimento = CNES,
           Procedimentos = NO_PROCEDIMENTO,
           Grupo = NO_GRUPO,
           `Sub-grupo` = NO_SUB_GRUPO,
           `Forma de organização` = NO_FORMA_ORGANIZACAO,
           Complexidade = complexidade,
           `Tipo de financiamento` = NO_FINANCIAMENTO,
           `Tipo de sub-financiamento` = NO_SUB_FINANCIAMENTO,
           Financeiro = VAL_TOT,
           Quantidade = QTD_AIH,
           `N° da AIH` = N_AIH,
           `Município` = nm_mun,
           `Mesorregião` = nm_mesor,
           `Microrregião` = nm_micror
    )

  writexl::write_xlsx(
    list(outputSIA = outputSIA,
         outputSIH = outputSIH),
    path = "output.xlsx"
  )

  return(list(outputSIA, outputSIH))
}




