

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  setwd(dsn)
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

get_unique_sites <- function(in_file) {
  dat <- readr::read_csv(in_file)
  return(unique(dat$site_id))
}

rds_to_csv <- function(rds_file, csv_file) {
  readr::write_csv(x = readRDS(rds_file), file = csv_file)
}

csv_to_zip <- function(csv_file, zip_file) {
  zip::zip(zip_file, csv_file)
}

sf_subset <- function(sites_file, sites) {
  subset <- readRDS(sites_file) %>%
    filter(site_id %in% sites & source %in% 'nwis_dv' & original_source %in% 'USGS') %>%
    select(site_id, geometry) %>% distinct() %>%
    sf::st_transform(crs = 4326)
  
  return(subset)
}