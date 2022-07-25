download_sigtap_files <- function(year_start, month_start, year_end, month_end, newer) {
  base_url <- "ftp://ftp2.datasus.gov.br/pub/sistemas/tup/downloads/"

  output_dir <- str_c(tempdir(), "SIGTAP", sep="\\")

  dir.create(output_dir)

  connection <- curl(base_url)

  dir_files <- connection %>%
    readLines() %>%
    str_subset("TabelaUnificada*") %>%
    str_sub(57) %>%
    as_tibble_col(column_name="file_name") %>%
    mutate(file_version_id = str_sub(file_name, 17, 22),
           publication_date = ym(file_version_id))

  close(connection)

  if (newer == TRUE & missing(year_start) &
      missing(month_start) & missing(year_end) &
      missing(month_end)) {

    dir_files <- dir_files %>%
      filter(publication_date == max(publication_date))
  } else {

    publication_date_start <- ym(str_glue("{year_start}-{month_start}"))
    publication_date_end <- ym(str_glue("{year_end}-{month_end}"))

    dir_files <- dir_files %>%
      filter(publication_date >= publication_date_start,
             publication_date <= publication_date_end)
  }

  file_version_id <- pull(dir_files, file_version_id)

  walk(str_glue("{output_dir}/{file_version_id}"), dir.create)

  files_name <- pull(dir_files, file_name)

  download_files_url <- str_glue("{base_url}{files_name}")
  output_files_path <- str_glue("{output_dir}/{file_version_id}/{files_name}")
  output_zip_files_path <- str_glue("{output_dir}/{file_version_id}")

  walk2(download_files_url, output_files_path, curl_download)

  walk2(output_files_path, output_zip_files_path, ~ unzip(.x, exdir=.y))
}

get_sigtap_file <- function(file_name, type) {
  loc <- str_locate(file_name, "SIGTAP")

  file_version_id <- str_sub(file_name, loc[2] + 2, loc[2] + 7)

  raw_file <- file_name %>% readLines()

  if (type == "raw") {
    raw_file <- tibble_row(raw_detail = list(raw_file),
                           file_version_id = file_version_id)

    return(raw_file)
  }

  if (type == "layout") {
    layout_file <- raw_file %>%
      as_tibble() %>%
      add_column(file_version_id)

    return(layout_file)
  }
}

get_detail <- function(detail_name) {
  dir_files <- str_c(tempdir(), "SIGTAP", sep="\\")

  files_name <- switch(detail_name,
                       "Procedimento" = c("tb_procedimento.txt", "tb_procedimento_layout.txt"),
                       "Grupo" = c("tb_grupo.txt", "tb_grupo_layout.txt"),
                       "Subgrupo" = c("tb_sub_grupo.txt", "tb_sub_grupo_layout.txt"),
                       "Forma de organização" = c("tb_forma_organizacao.txt", "tb_forma_organizacao_layout.txt"),
                       "Financiamento" = c("tb_financiamento.txt", "tb_financiamento_layout.txt"),
                       "Rubrica" = c("tb_rubrica.txt", "tb_rubrica_layout.txt"),
                       "CBO" = c("tb_ocupacao.txt", "tb_ocupacao_layout.txt"))

  raw_detail_file_name <- files_name[1]
  detail_layout_file_name <- files_name[2]

  raw_detail <- list.files(dir_files,full.name=TRUE, recursive=TRUE) %>%
    str_subset(raw_detail_file_name) %>%
    map(get_sigtap_file, type="raw") %>%
    bind_rows()

  detail_layout <- list.files(dir_files,full.name=TRUE, recursive=TRUE) %>%
    str_subset(detail_layout_file_name) %>%
    map(get_sigtap_file, type="layout") %>%
    bind_rows() %>%
    separate(value, into=c("column_name", "size", "start", "end", "type"), sep=",") %>%
    filter(column_name != "Coluna")

  details <- raw_detail %>%
    left_join(detail_layout, by="file_version_id") %>%
    mutate(detail = pmap(list(raw_detail, start, end), ~ str_sub(..1, ..2, ..3))) %>%
    select(column_name, file_version_id, detail) %>%
    pivot_wider(names_from=column_name, values_from=detail) %>%
    unnest(-file_version_id) %>%
    mutate(across(everything(), str_trim))

  return(details)
}

get_procedure_details <- function() {

  # TO-DO ---------------------------------------------------------------------#

  # 1. Verificar se os arquivos SIGTAP existem

  procedure_main_details <- get_detail("Procedimento")
  procedure_group <- get_detail("Grupo")
  procedure_sub_group <- get_detail("Subgrupo")
  procedure_organization_form <- get_detail("Forma de organização")
  procedure_funding <- get_detail("Financiamento")
  procedure_rubric <- get_detail("Rubrica")

  procedure_details <- procedure_main_details %>%
    mutate(CO_GRUPO = str_sub(CO_PROCEDIMENTO, 1, 2),
           CO_SUB_GRUPO = str_sub(CO_PROCEDIMENTO, 3, 4),
           CO_FORMA_ORGANIZACAO = str_sub(CO_PROCEDIMENTO, 5, 6)) %>%
    left_join(procedure_group, by=c("file_version_id", "CO_GRUPO")) %>%
    left_join(procedure_sub_group, by=c("file_version_id", "CO_GRUPO", "CO_SUB_GRUPO")) %>%
    left_join(procedure_organization_form, by=c("file_version_id", "CO_GRUPO", "CO_SUB_GRUPO", "CO_FORMA_ORGANIZACAO")) %>%
    left_join(procedure_funding, by=c("file_version_id", "CO_FINANCIAMENTO")) %>%
    left_join(procedure_rubric, by=c("file_version_id", "CO_RUBRICA")) %>%
    select(-starts_with("DT_COMPETENCIA")) %>%
    group_by(CO_PROCEDIMENTO) %>%
    mutate(CO_GRUPO = str_sub(CO_PROCEDIMENTO, 1, 2),
           CO_SUB_GRUPO = str_sub(CO_PROCEDIMENTO, 1, 4),
           CO_FORMA_ORGANIZACAO = str_sub(CO_PROCEDIMENTO, 1, 6),
           NO_PROCEDIMENTO = str_c(CO_PROCEDIMENTO, NO_PROCEDIMENTO, sep="-"),
           NO_GRUPO = str_c(CO_GRUPO, NO_GRUPO, sep="-"),
           NO_SUB_GRUPO = str_c(CO_SUB_GRUPO, NO_SUB_GRUPO, sep="-"),
           NO_FINANCIAMENTO = str_c(CO_FINANCIAMENTO, NO_FINANCIAMENTO, sep="-"),
           NO_FORMA_ORGANIZACAO = str_c(CO_FORMA_ORGANIZACAO, NO_FORMA_ORGANIZACAO, sep="-"),
           NO_SUB_FINANCIAMENTO = str_c(CO_RUBRICA, NO_RUBRICA, sep="-"),
           COMPLEXIDADE = case_when(TP_COMPLEXIDADE == 0 ~ "Não se Aplica",
                                    TP_COMPLEXIDADE == 1 ~ "Atenção Básica",
                                    TP_COMPLEXIDADE == 2 ~ "Média Complexidade",
                                    TP_COMPLEXIDADE == 3 ~ "Alta Complexidade"),
           COMPLEXIDADE = str_c(TP_COMPLEXIDADE, COMPLEXIDADE, sep="-"),
           across(everything(), str_trim))

  return(procedure_details)
}
