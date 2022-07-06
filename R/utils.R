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

check_health_establishment <- function(df, health_establishment_id) {
  if (length(health_establishment_id) == 1) {
    if (health_establishment_id == "all") {

      return(df)
    } else {
      filtered_df <- filter(df, cnes == health_establishment_id)

      return(filtered_df)
    }
  } else {
    filtered_df <- filter(df, cnes %in% health_establishment_id)

    return(filtered_df)
  }
}

get_datasus <- function(year_start, month_start,
                        year_end, month_end, state_abbr,
                        information_system, health_establishment_id,
                        chunk_size) {

  publication_date_start <- ym(str_glue("{year_start}-{month_start}"))
  publication_date_end <- ym(str_glue("{year_end}-{month_end}"))

  counties <- get_counties_by_state(state_abbr)

  tmp_dir <- tempdir()

  information_system_dir <- str_glue("{tmp_dir}\\{information_system}")

  data_type = switch(information_system,
                     "SIA" = "PA",
                     "SIH" = c("RD", "RJ"))

  dir.create(information_system_dir)

  base_url <- str_glue({"ftp://ftp.datasus.gov.br/dissemin/publicos/{information_system}SUS/200801_/Dados/"})

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
  n_chunks <- ceiling(n_files/chunk_size)

  files_chunks <- list()
  chunk = 1
  while (chunk <= n_chunks) {
    files_chunks[[chunk]] <- files_name[(chunk_size*(chunk-1)+1):(chunk*chunk_size)]
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

      output <- raw_SIA %>%
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
        check_health_establishment(health_establishment_id) %>%
        filter(anomes_cmp >= publication_date_start) %>%
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
    }

    if (information_system == "SIH") {
      raw_SIH <- map_dfr(output_files_path, read.dbc, as.is=TRUE, .id="TIPO")

      output <- raw_SIH %>%
        as_tibble() %>%
        janitor::clean_names() %>%
        mutate(tipo = ifelse(tipo == "1", "Aprovada", "Rejeitada"),
               dt_cmpt = ym(str_c(ano_cmpt, mes_cmpt, sep="-")),
               qtd_aih = 1) %>%
        check_health_establishment(health_establishment_id) %>%
        filter(dt_cmpt >= publication_date_start) %>%
        left_join(counties, by=c("munic_res" = "id_mun")) %>%
        left_join(cnes_df, by=c("cnes" = "CNES")) %>%
        left_join(procedure_details, by = c("proc_rea" = "CO_PROCEDIMENTO")) %>%
        mutate(CNES = str_c(cnes, FANTASIA, sep="-")) %>%
        select(`Situação da AIH` = tipo,
               `Ano de processamento` = ano_cmpt,
               `Mês de processamento` = mes_cmpt,
               `Mês/Ano de processamento` = dt_cmpt,
               Estabelecimento = CNES,
               Procedimentos = NO_PROCEDIMENTO,
               Grupo = NO_GRUPO,
               `Sub-grupo` = NO_SUB_GRUPO,
               `Forma de organização` = NO_FORMA_ORGANIZACAO,
               Complexidade = complexidade,
               `Tipo de financiamento` = NO_FINANCIAMENTO,
               `Tipo de sub-financiamento` = NO_SUB_FINANCIAMENTO,
               Financeiro = val_tot,
               Quantidade = qtd_aih,
               `N° da AIH` = n_aih,
               `Município` = nm_mun,
               `Mesorregião` = nm_mesor,
               `Microrregião` = nm_micror
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
