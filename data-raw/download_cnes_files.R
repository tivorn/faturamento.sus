download_cnes_files <- function(year_start, month_start, year_end, month_end, newer, state_abbr="all") {
  base_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/"
  output_dir <- here("data-raw", "CNES", "ST")

  connection <- curl(base_url)
  dir_files <- connection %>%
    readLines() %>%
    str_sub(start=-12) %>%
    as_tibble_col(column_name = "file_name") %>%
    mutate(state = str_sub(file_name, 3, 4),
           publication_date = ym(str_sub(file_name, 5, 8)),
           format_file = str_sub(file_name, -3)) %>%
    filter(format_file == "dbc")

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
      filter(publication_date >= publication_date_start & publication_date <= publication_date_end)
  }

  if (state_abbr != "all") {
    dir_files <- dir_files %>%
      filter(state == state_abbr)
  }

  files_name <- pull(dir_files, file_name)

  download_files_url <- str_glue("{base_url}{files_name}")
  output_files_path <- str_glue("{output_dir}/{files_name}")

  walk2(download_files_url, output_files_path, curl_download)
}

download_cnes_files(newer=TRUE)

health_establishment <- here("data-raw", "CNES", "ST") %>%
  list.files(full.names = TRUE) %>%
  map_dfr(read.dbc, as.is=TRUE)

health_establishment_details <- here("data-raw", "CNES", "CADGER") %>%
  list.files(full.names = TRUE) %>%
  map_dfr(read.dbf, as.is=TRUE)

health_establishment <- health_establishment %>%
  left_join(health_establishment_details, by="CNES") %>%
  select(CNES, FANTASIA, COMPETEN) %>%
  mutate(NO_ESTABELECIMENTO = str_c(CNES, FANTASIA, sep="-")) %>%
  as_tibble()
