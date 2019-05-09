## This is an R script used to retrieve all checklists within
## a buffered polygon/greenspace
## necessary to calculate Ugamma
## this calls the eBird database hosted on a remote MySQL server
## Hence, this script will not actually work for anyone - unless you set up a database
## as we have done
## however, we have included the necessary data in the database, based on this script
## but include this script in the repository for completeness
## To run these commands you need to connect to a ebird database, setup like in the database folder:
## and have an .Renviron file with your database credentials 
## follow the steps outlined here: https://db.rstudio.com/best-practices/managing-credentials/#use-environment-variables
options(readr.show_progress = FALSE)
library(dbplyr)
library(dplyr, quietly = T, warn.conflicts = F)
library(RMariaDB)
library(sf)
get_con  <- function () {
  dbConnect(RMariaDB::MariaDB(), host = 'KESTREL', dbname='ebird',user = Sys.getenv('userid'), password = Sys.getenv('pwd'))
} 

get_wkt_of_greenspace <- function (geojson_data){ 
  polys <- lapply(geojson_data$features, function(feature) {
    list(feature$geometry$coordinates)
  })
  names(polys) <- sapply(geojson_data$features, function(feature) {
    list(feature$geometry$type)
  })
  wellknown::geojson2wkt(polys)
}

parse_gs_file <- function (filename) {
  data.frame(filename = filename)
}

counts_from_file <- function (con, gs_file) {
  print(gs_file)
  buff <- st_read(gs_file)
  geom <- st_geometry(buff)
  wkt <- st_as_text(geom)
  query_ <- paste0(
    "
   SELECT
      *
    FROM checklists as lists, sites as sites, samples as samples, species as species
      WHERE
          samples.TAXONOMIC_ORDER = species.TAXONOMIC_ORDER
        AND
          samples.SAMPLING_EVENT_IDENTIFIER = lists.SAMPLING_EVENT_IDENTIFIER
        AND
          lists.LOCALITY_ID = sites.LOCALITY_ID 
        AND
          MBRContains( GeomFromText(
            \'", wkt,"\'
          ), sites.pt);")
  lists <- dbGetQuery(con, query_)
  lists <- as.data.frame(lists) %>%
    distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all=TRUE) %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, LATITUDE, LONGITUDE) %>%
    filter(OBSERVATION_DATE >= "2010-01-01")
  meta <- parse_gs_file(gs_file)
  if(nrow(lists) == 0) {
    cat(paste(meta$file_country, meta$gs_id, 0, 'species \n'))
    lists
  } else {
    cat(paste(meta$file_country, meta$gs_id, nrow(lists), 'species \n'))
    cbind(meta, lists)
  }
}

write_all_buffer  <- function (greenspace_folder = 'Data/AUS_site_geojsons'){
  greenspaces <- dir(greenspace_folder, pattern='buffer')
  write_samples(greenspaces)
}

write_samples <- function (files, greenspace_folder = 'Data/AUS_site_geojsons/', out_folder = 'Data/AUS_data_site_rds/') {
  require(purrr)
  con <- get_con()
  print(paste('processing', length(files), 'greenspaces'))
  files %>% walk(function(gs){
    infile <- paste0(greenspace_folder, gs)
    outfile <- paste0(out_folder, gsub('.geojson$','', gs) ,'.RDS')
    #print(paste(gs, infile, outfile))
    saveRDS(counts_from_file(con, infile), outfile)
  })
  dbDisconnect(con)
}

write_all_buffer()