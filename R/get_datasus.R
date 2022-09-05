check_filter <- function(df, var_value, var_name) {

  if (length(var_value) == 1) {
    if (var_value == "all") {

      return(df)
    } else {
      filtered_df <- filter(df, .data[[var_name]] == var_value)

      return(filtered_df)
    }
  } else {
    filtered_df <- filter(df, .data[[var_name]] %in% var_value)

    return(filtered_df)
  }
}

preprocess_SIA <- function(raw_SIA,
                           health_establishment_id,
                           county_id,
                           publication_date_start,
                           procedure_details,
                           cbo,
                           cid) {
  outputSIA <- raw_SIA %>%
    as_tibble() %>%
    rename(CNES = PA_CODUNI) %>%
    check_filter(var_value=health_establishment_id,
                 var_name="CNES") %>%
    check_filter(var_value=county_id,
                 var_name="PA_UFMUN") %>%
    filter(ym(PA_CMP) >= publication_date_start) %>%
    left_join(counties, by=c("PA_MUNPCN" = "id_municipio")) %>%
    left_join(procedure_details, c("PA_PROC_ID" = "CO_PROCEDIMENTO",
                                   "PA_CMP" = "file_version_id")) %>%
    left_join(cbo, by=c("PA_CBOCOD" = "CO_OCUPACAO",
                        "PA_CMP" = "file_version_id")) %>%
    left_join(cid, by=c("PA_CIDPRI" = "CO_CID",
                        "PA_CMP" = "file_version_id")) %>%
    left_join(health_establishment, by="CNES") %>%
    mutate(DT_CMP = ym(PA_CMP),
           ANO_CMP = year(DT_CMP),
           MES_CMP = month(DT_CMP),
           NM_MES_CMP = str_to_title(month(DT_CMP, label=TRUE, abbr = FALSE)),
           NM_MES_CMP = str_glue("{sprintf('%02d', MES_CMP)} - {NM_MES_CMP}"),
           DT_MVM = ym(PA_MVM),
           ANO_MVM = year(DT_MVM),
           MES_MVM = month(DT_MVM),
           NM_MES_MVM = str_to_title(month(DT_MVM, label=TRUE, abbr = FALSE)),
           NM_MES_MVM = str_glue("{sprintf('%02d', MES_MVM)} - {NM_MES_MVM}"),
           TIPO_REGISTRO = case_when(PA_DOCORIG == "C" ~ "BPA - Consolidado",
                                     PA_DOCORIG == "I" ~ "BPA - Individualizado",
                                     PA_DOCORIG == "P" ~ "APAC - Procedimento Principal",
                                     PA_DOCORIG == "S" ~ "APAC - Procedimento Secundário"),
           PA_UFUNI = str_sub(PA_UFMUN, 1, 2),
           PA_UFPCN = str_sub(PA_MUNPCN, 1, 2),
           NO_CID = if_else(is.na(NO_CID), "0000-Não informado", NO_CID),
           across(c(nome_estado, nome_microrregiao, nome_mesorregiao, nome_municipio),
                  ~ case_when(PA_UFUNI == PA_UFPCN ~ .x,
                              PA_UFUNI != PA_UFPCN ~ "Outros",
                              PA_UFUNI == 99 | PA_UFPCN == 99 ~ "Não informado"
                  ))
    ) %>%
    select(`Mês/Ano de Atendimento` = DT_CMP,
           `Ano de Atendimento` = ANO_CMP,
           `Mês de Atendimento (Número)` = MES_CMP,
           `Mês de Atendimento` = NM_MES_CMP,
           `Ano/Mês de Atendimento` = PA_CMP,
           `Ano/Mês de Processamento` = PA_MVM,
           `Mês/Ano de Processamento` = DT_MVM,
           `Ano de Processamento` = ANO_MVM,
           `Mês de Processamento (Número)` = MES_MVM,
           `Mês de Processamento` = NM_MES_MVM,
           `Nome da Morbidade (CID)` = NO_CID,
           `Procedimentos realizados` = NO_PROCEDIMENTO,
           `Grupo de Procedimentos` = NO_GRUPO,
           `SubGrupo de Procedimentos` = NO_SUB_GRUPO,
           `Forma de organização` = NO_FORMA_ORGANIZACAO,
           `Tipo de Financiamento` = NO_FINANCIAMENTO,
           `TipoFin/Subtipo Financiamento` = NO_SUB_FINANCIAMENTO,
           `Profissional - CBO` = NO_OCUPACAO,
           `Instrumento de registro` = TIPO_REGISTRO,
           `Complexidade Procedimento` = COMPLEXIDADE,
           `Frequência` = PA_QTDAPR,
           `Quantidade Apresentada` = PA_QTDPRO,
           `Valor Aprovado` = PA_VALAPR,
           `Valor Apresentado` = PA_VALPRO,
           `Estabelecimento CNES` = NO_ESTABELECIMENTO,
           `Município Residência` = nome_municipio,
           `Micro IBGE Residência` = nome_microrregiao,
           `Meso IBGE Residência` = nome_mesorregiao,
           `Estado Residência` = nome_estado,
    )

  return(outputSIA)
}

preprocess_SIH <- function(raw_SIH,
                           health_establishment_id,
                           county_id,
                           publication_date_start,
                           procedure_details,
                           cbo,
                           file_type,
                           cid) {
  outputSIH <- raw_SIH %>%
    as_tibble() %>%
    left_join(file_type, by="file_id") %>%
    mutate(TIPO = ifelse(file_type == "RD", "Aprovada", "Rejeitada"),
           DT_CMPT = ym(str_c(ANO_CMPT, MES_CMPT, sep="-")),
           NM_MES_CMPT = str_to_title(month(DT_CMPT, label=TRUE, abbr=FALSE)),
           NM_MES_CMPT = str_glue("{sprintf('%02d', month(DT_CMPT))} - {NM_MES_CMPT}"),
           QTD_AIH = 1,
           ANOMES_CMPT = format(DT_CMPT, "%Y%m"),
           UF_RES = str_sub(MUNIC_RES, 1, 2),
           UF_GESTOR = str_sub(UF_ZI, 1, 2)) %>%
    check_filter(var_value=health_establishment_id,
                 var_name="CNES") %>%
    check_filter(var_value=county_id,
                 var_name="UF_ZI") %>%
    filter(DT_CMPT >= publication_date_start) %>%
    left_join(counties, by=c("MUNIC_RES" = "id_municipio")) %>%
    left_join(procedure_details, c("PROC_REA" = "CO_PROCEDIMENTO",
                                   "ANOMES_CMPT" = "file_version_id")) %>%
    left_join(cid, by=c("DIAG_PRINC" = "CO_CID",
                        "ANOMES_CMPT" = "file_version_id")) %>%
    left_join(health_establishment, by="CNES") %>%
    mutate(NO_CID = if_else(is.na(NO_CID), "0000-Não informado", NO_CID),
           across(c(nome_estado, nome_microrregiao, nome_mesorregiao, nome_municipio),
                  ~ case_when(UF_GESTOR == UF_RES ~ .x,
                              UF_GESTOR != UF_RES ~ "Outros",
                              UF_GESTOR == 99 | UF_RES == 99 ~ "Não informado"
                  ))) %>%
    select(`Situação da AIH` = TIPO,
           `Ano processamento` = ANO_CMPT,
           `Mês processamento (Número)` = MES_CMPT,
           `Mês processamento` = NM_MES_CMPT,
           `Mês/Ano processamento` = DT_CMPT,
           `Ano/Mês processamento` = ANOMES_CMPT,
           `Hospital (CNES)` = NO_ESTABELECIMENTO,
           `Procedimentos realizados` = NO_PROCEDIMENTO,
           `Grupo de Procedimentos` = NO_GRUPO,
           `SubGrupo de Procedimentos` = NO_SUB_GRUPO,
           `Forma de organização` = NO_FORMA_ORGANIZACAO,
           `Complexidade do Procedimento` = COMPLEXIDADE,
           `Financiamento` = NO_FINANCIAMENTO,
           `SubTp FAEC` = NO_SUB_FINANCIAMENTO,
           `Nome da Mobirdade (CID)` = NO_CID,
           `Valor Total` = VAL_TOT,
           `Frequência` = QTD_AIH,
           `Sequencial` = N_AIH,
           `Município de Residência` = nome_municipio,
           `Mesorregião IBGE de Resid.` = nome_mesorregiao,
           `Microrregião IBGE de Resid.` = nome_microrregiao,
           `Estado de Residência` = nome_estado
    )

  return(outputSIH)
}

preprocess_SIH_SP <- function(raw_SIH_SP,
                              health_establishment_id,
                              county_id,
                              publication_date_start,
                              procedure_details,
                              cbo,
                              cid) {
  outputSIH_SP <- raw_SIH_SP %>%
    as_tibble() %>%
    rename(CNES = SP_CNES) %>%
    check_filter(var_value=health_establishment_id,
                 var_name="CNES") %>%
    check_filter(var_value=county_id,
                 var_name="SP_M_HOSP") %>%
    filter(SP_AA >= year(publication_date_start)) %>%
    mutate(ANOMES_MVM = str_c(SP_AA, SP_MM),
           MESANO_MVM = str_c(SP_MM, SP_AA, sep="-"),
           DT_MVM = ym(str_c(SP_AA, SP_MM, sep="-")),
           NM_MES_MVM = str_to_title(month(DT_MVM, label=TRUE, abbr=FALSE)),
           NM_MES_MVM = str_glue("{sprintf('%02d', month(DT_MVM))} - {NM_MES_MVM}"),
           DT_INTER = ymd(SP_DTINTER),
           ANO_INT = str_sub(SP_DTINTER, 1, 4),
           MES_INT = str_sub(SP_DTINTER, 5, 6),
           ANOMES_INT = str_c(ANO_INT, MES_INT),
           MESANO_INT = str_c(MES_INT, ANO_INT, sep="-")) %>%
    left_join(counties, by=c("SP_M_PAC" = "id_municipio")) %>%
    left_join(procedure_details, c("SP_PROCREA" = "CO_PROCEDIMENTO",
                                   "ANOMES_MVM" = "file_version_id")) %>%
    left_join(cbo, by=c("SP_PF_CBO" = "CO_OCUPACAO",
                        "ANOMES_MVM" = "file_version_id")) %>%
    left_join(health_establishment, by="CNES") %>%
    left_join(cid, by=c("SP_CIDPRI" = "CO_CID",
                        "ANOMES_MVM" = "file_version_id")) %>%
    mutate(SP_UF_HOSP = str_sub(SP_M_HOSP, 1, 2),
           SP_UF_PAC = str_sub(SP_M_PAC, 1, 2),
           IN_TP_VAL = case_when(IN_TP_VAL == 1 ~ "Serviço Hospitalar",
                                 IN_TP_VAL == 2 ~ "Serviço Profissional"),
           NO_OCUPACAO = str_c(SP_PF_CBO, NO_OCUPACAO, sep="-"),
           NO_CID = if_else(is.na(NO_CID), "0000-Não informado", NO_CID),
           across(c(nome_estado, nome_microrregiao, nome_mesorregiao, nome_municipio),
                  ~ case_when(SP_UF_HOSP == SP_UF_PAC ~ .x,
                              SP_UF_HOSP != SP_UF_PAC ~ "Outros",
                              SP_UF_HOSP == 99 | SP_UF_PAC == 99 ~ "Não informado"
                  ))
    ) %>%
    select(`Data Internação` = DT_INTER,
           `Mês/Ano Internação` = MESANO_INT,
           `Ano/Mês Internação` = ANOMES_INT,
           `Ano Internação` = ANO_INT,
           `Mês Internação` = MES_INT,

           `Mês/Ano Processamento` = MESANO_MVM,
           `Ano/Mês Processamento` = ANOMES_MVM,
           `Ano Processamento` = SP_AA,
           `Mês Processamento (Número)` = SP_MM,
           `Mês Processamento` = NM_MES_MVM,

           `Quantidade Aprovada` = SP_QT_PROC,
           `Valor Aprovado` = SP_VALATO,
           `Quantidade de Ato` = SP_QTD_ATO,
           `Frequência` = SP_U_AIH,

           `N° da AIH` = SP_NAIH,

           `Procedimentos Secundários` = NO_PROCEDIMENTO,
           `Grupo Proc. Secundário` = NO_GRUPO,
           `Sub-grupo Proc. Secundário` = NO_SUB_GRUPO,
           `Forma de organização Secundária` = NO_FORMA_ORGANIZACAO,

           `Tipo de financiamento do ato profissional` = NO_FINANCIAMENTO,
           `Subtipo FAEC do ato profissional` = NO_SUB_FINANCIAMENTO,
           `Complexidade do ato profissional` = COMPLEXIDADE,

           `Tipo de Valor` = IN_TP_VAL,

           `Ocupação` = NO_OCUPACAO,
           `Nome da Mobirdade (CID)` = NO_CID,

           Estabelecimento = NO_ESTABELECIMENTO,

           `Município` = nome_municipio,
           `Microrregião` = nome_microrregiao,
           `Mesorregião` = nome_mesorregiao,
           Estado = nome_estado,
    )

  return(outputSIH_SP)
}

get_datasus <- function(year_start, month_start,
                        year_end, month_end, state_abbr, county_id,
                        information_system, health_establishment_id) {

  publication_date_start <- ym(str_glue("{year_start}-{month_start}"))
  publication_date_end <- ym(str_glue("{year_end}-{month_end}"))

  download_sigtap_files(year_start, month_start,
                        year_end, month_end, newer=FALSE)

  procedure_details <- get_procedure_details()
  cbo <- get_detail("CBO")
  cid <- get_detail("CID") %>%
    mutate(NO_CID = iconv(NO_CID, "latin1", "UTF-8"),
           across(ends_with("CID"), str_trim),
           NO_CID = str_c(CO_CID, NO_CID, sep="-"))

  tmp_dir <- tempdir()

  data_source = str_sub(information_system, 1, 3)
  data_type = switch(information_system,
                     "SIA" = "PA",
                     "SIH-AIH" = c("RD", "RJ"),
                     "SIH-SP" = "SP")

  information_system_dir <- str_glue("{tmp_dir}\\{information_system}")

  dir.create(information_system_dir)

  base_url <- str_glue("ftp://ftp.datasus.gov.br/dissemin/publicos/{data_source}SUS/200801_/Dados/")

  connection <- curl(base_url)

  dir_files <- connection %>%
    readLines() %>%
    str_sub(start=-12) %>%
    tibble::as_tibble_col(column_name = "file_name") %>%
    mutate(state = str_sub(file_name, 3, 4),
           publication_date = ym(str_sub(file_name, 5, 8)),
           file_type = str_sub(file_name, 1, 2)) %>%
    filter(file_type %in% data_type,
           state == state_abbr,
           publication_date >= publication_date_start,
           publication_date <= publication_date_end)

  close(connection)

  files_name <- pull(dir_files, file_name)
  n_files <- files_name %>% length()
  chunk_size <- ceiling(0.2*n_files)
  n_chunks <- ceiling(n_files/chunk_size)

  files_chunks <- list()
  chunk = 1
  while (chunk <= n_chunks) {
    upper_limit <- ifelse(chunk*chunk_size > n_files, n_files, chunk*chunk_size)
    files_chunks[[chunk]] <- files_name[(chunk_size*(chunk-1)+1):upper_limit]
    names(files_chunks)[chunk] = str_glue("chunk_{chunk}")
    chunk = chunk + 1
  }

  for (chunk in 1:n_chunks) {
    dir.create(str_glue("{tmp_dir}\\{information_system}\\chunk_{chunk}"))
    download_files_url <- str_glue("{base_url}{files_chunks[[chunk]]}")
    output_files_path <- str_glue("{tmp_dir}\\{information_system}\\{names(files_chunks)[chunk]}\\{files_chunks[[chunk]]}")
    walk2(download_files_url, output_files_path, curl_download)

    if (information_system == "SIA") {
      raw_SIA <- map_dfr(output_files_path, read.dbc, as.is=TRUE)

      output <- preprocess_SIA(
        raw_SIA,
        health_establishment_id,
        county_id,
        publication_date_start,
        procedure_details,
        cbo,
        cid
      )
    }

    if (information_system == "SIH-AIH") {
      file_type <- output_files_path %>%
        str_sub(start=-12) %>%
        tibble::as_tibble_col(column_name = "file_name") %>%
        mutate(file_type = str_sub(file_name, 1, 2),
               file_id = as.character(row_number()))
      select(file_type, file_id)

      raw_SIH <- map_dfr(output_files_path, read.dbc, as.is=TRUE, .id="file_id")

      output <- preprocess_SIH(
        raw_SIH,
        health_establishment_id,
        county_id,
        publication_date_start,
        procedure_details,
        cbo,
        file_type,
        cid
      )
    }

    if (information_system == "SIH-SP") {
      raw_SIH_SP <- map_dfr(output_files_path, read.dbc, as.is=TRUE)

      output <- preprocess_SIH_SP(
        raw_SIH_SP,
        health_establishment_id,
        county_id,
        publication_date_start,
        procedure_details,
        cbo,
        cid
      )
    }

    output_path <- str_glue("{tmp_dir}\\{information_system}\\{names(files_chunks)[chunk]}\\output{information_system}_chunk_{chunk}.rds")

    saveRDS(output,
            file = output_path)
  }

  output <- tempdir() %>%
    list.files(information_system,
               full.names=TRUE,
               recursive=TRUE) %>%
    map_dfr(readRDS)

  return(output)
}

get_datasus_from_local <- function(dbc_dir_path, information_system, county_id,
                                   health_establishment_id) {

  data_type = switch(information_system,
                     "SIA" = "PA",
                     "SIH-AIH" = c("RD", "RJ"),
                     "SIH-SP" = "SP")

  files_path <- dbc_dir_path %>%
    str_c("\\", information_system) %>%
    list.files(full.names=TRUE, recursive=TRUE) %>%
    str_subset("dbc")

  dir_files <- files_path %>%
    str_sub(start=-12) %>%
    tibble::as_tibble_col(column_name = "file_name") %>%
    mutate(state = str_sub(file_name, 3, 4),
           publication_date = ym(str_sub(file_name, 5, 8)),
           file_type = str_sub(file_name, 1, 2)) %>%
    filter(file_type %in% data_type) %>%
    mutate(file_id = as.character(row_number()))

  file_type <- select(dir_files, file_type, file_id)

  publication_date <- pull(dir_files, publication_date)
  publication_date_start <- min(publication_date)
  publication_date_end <- max(publication_date)

  download_sigtap_files(year_start = year(publication_date_start),
                        month_start = month(publication_date_start),
                        year_end = year(publication_date_end),
                        month_end = month(publication_date_end),
                        newer=FALSE)

  procedure_details <- get_procedure_details()
  cbo <- get_detail("CBO")
  cid <- get_detail("CID") %>%
    mutate(#NO_CID = iconv(NO_CID, "latin1", "UTF-8"),
           across(ends_with("CID"), str_trim),
           NO_CID = str_c(CO_CID, NO_CID, sep="-"))

  if (information_system == "SIA") {
    raw_SIA <- map_dfr(files_path, read.dbc, as.is=TRUE)

    output <- preprocess_SIA(
      raw_SIA,
      health_establishment_id,
      county_id,
      publication_date_start,
      procedure_details,
      cbo,
      cid
    )
  }

  if (information_system == "SIH-AIH") {
    raw_SIH <- map_dfr(files_path, read.dbc, as.is=TRUE, .id="file_id")

    output <- preprocess_SIH(
      raw_SIH,
      health_establishment_id,
      county_id,
      publication_date_start,
      procedure_details,
      cbo,
      file_type,
      cid
    )
  }

  if (information_system == "SIH-SP") {
    raw_SIH_SP <- map_dfr(files_path, read.dbc, as.is=TRUE)

    output <- preprocess_SIH_SP(
      raw_SIH_SP,
      health_establishment_id,
      county_id,
      publication_date_start,
      procedure_details,
      cbo,
      cid
    )
  }

  return(output)
}
