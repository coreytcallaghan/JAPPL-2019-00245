## This is an R script to get a list of species for the regional pool
## for each of the 10 Australian greenspaces
## this brings in a split df for all of Australia data split so the data will fit in a GitHub repository
## for purposes of reproducibility
## This relies on rds files which are already subsetted to all lists >2010-01-01 for each greenspace's
## 50 km buffer - using a script titled "spatial_query_get_lists_in_buffer.R"
## it then subsets the species on those lists to the 'species' pool' - Ugamma

# packages
library(readr)
library(dplyr)

## Read in raw eBird data for all of Australia
## split into 8 dfs that will fit into GitHub for purposes of archiving this analysis
df1 <- readRDS("Data/ebird_data1.RDS")
df2 <- readRDS("Data/ebird_data2.RDS")
df3 <- readRDS("Data/ebird_data3.RDS")
df4 <- readRDS("Data/ebird_data4.RDS")
df5 <- readRDS("Data/ebird_data5.RDS")
df6 <- readRDS("Data/ebird_data6.RDS")
df7 <- readRDS("Data/ebird_data7.RDS")
df8 <- readRDS("Data/ebird_data8.RDS")
ebird_AUS_data <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

## read in urban scores for Australian birds
indices <- read_csv("Data/australian_indices.csv")

### do the below for each greenspace in the folder

files <- list.files(path="Data/AUS_data_site_rds")

Ugamma <- list()

for (i in files) {
  
data <- readRDS(paste0("Data/AUS_data_site_rds/", i))

LOCALITY_ID <- unique(sub(".*dropchop_buffer_", "", data$filename))
LOCALITY_ID <- gsub(".geojson", "", LOCALITY_ID)

lists <- data$SAMPLING_EVENT_IDENTIFIER

n_lists <- length(lists)

list_data <- ebird_AUS_data %>%
  dplyr::filter(`SAMPLING EVENT IDENTIFIER` %in% lists)

colnames(list_data) <- gsub(" |/", "_", colnames(list_data))

## collapse to just species and do some filtering for only
## those species which are on > 2.5 % of checklists
## very subjective filter!!!!
## also keep in mind the 'urban scores' are already filtered to a certain extent
species <- list_data %>%
  filter(CATEGORY %in% c("species","issf", "domestic")) %>% 
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>%
  summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
  rename(OBSERVATION_COUNT = COUNT_SPP) %>%
  group_by(COMMON_NAME) %>%
  summarise(N=n()) %>%
  mutate(total_lists = n_lists) %>%
  mutate(percent_lists = (N/total_lists)*100) %>%
  filter(percent_lists >= 2.5) %>%
  inner_join(., indices, by="COMMON_NAME")

n_species <- nrow(species)

Ugamma[[i]] <- data.frame(Ugamma = quantile(species$urban_index, 0.25),
                     LOCALITY_ID = LOCALITY_ID,
                     pool_size = n_species)

}

# convert the list into a dataframeS
Ugamma <- bind_rows(Ugamma)


## write this out as a csv
write_csv(Ugamma, "Data/Ugamma.csv")

