# R script used to clean the associated data for Australia
# essentially filters the data from all data to the 'best quality' lists

## packages
library(dplyr)
library(readr)
library(vegan)
library(tidyr)
library(lubridate)


## source global functions
source("R/global_functions.R")

## read data in
indices <- read_csv("Data/australian_indices.csv")
AUS_data <- readRDS("Data/AUS_data.RDS")
colnames(AUS_data) <- gsub(" |/", "_", colnames(AUS_data))

## clean data
## here is where you add all the columns needed for the analysis (that don't vary within checklist)
sampling_event_info <- AUS_data %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LOCALITY, LOCALITY_ID, OBSERVATION_DATE,
                PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, 
                DURATION_MINUTES, OBSERVER_ID) %>%
  distinct()


## Counts how many 'x's per checklist
X_missing <- AUS_data %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION_COUNT=="X"))


## combines species and subspecies into one count
clean_data <- AUS_data %>%
  filter(CATEGORY %in% c("species","issf", "domestic")) %>% 
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>%
  summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
  rename(OBSERVATION_COUNT = COUNT_SPP) %>% 
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER")%>%
  inner_join(., X_missing, by="SAMPLING_EVENT_IDENTIFIER") %>%
  # join the urbanization indices to the clean dataset
  right_join(dplyr::select(indices, COMMON_NAME, urban_index), ., by="COMMON_NAME")


## now filter checklists based on criteria, to select the 'best possible checklists'
## Getting all the data with the associated checklists to be used in analysis
analysis_data <- clean_data %>%
  dplyr::filter(ALL_SPECIES_REPORTED == 1, # only complete checklists
                PROTOCOL_TYPE !="Incidental", # get rid of 'incidental' lists
                PROTOCOL_TYPE !="Historical", # get rid of 'historical' lists
                number_X==0) %>% ## Get rid of any checklists that had a single X (Could alter this)
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(species_richness=length(unique(COMMON_NAME)), 
            species_diversity=exp(diversity(OBSERVATION_COUNT)), 
            species_abundance=sum(OBSERVATION_COUNT, na.rm=TRUE),
            urban_index = quantile(urban_index, 0.25, na.rm=TRUE)) %>%
  inner_join(sampling_event_info, ., by="SAMPLING_EVENT_IDENTIFIER") %>%
  # get rid of any checklists prior to 2010 - rather arbitrary cutoff, but gets rid of older stuff a bit
  filter(OBSERVATION_DATE >= "2010-01-01") %>%
  # get rid of any 'missing' values - odd occurence that this would happen
  filter(complete.cases(urban_index)) %>%
  # filter out any values below the 0.05 quantile
  # and any values greater than the 0.95 quantile
  # but by LOCALITY!
  group_by(LOCALITY_ID) %>%
  filter(species_diversity > quantile(.$species_diversity, 0.025) & 
           species_diversity < quantile(.$species_diversity, 0.975)) %>%
  ungroup() %>%
  # filter based on time and distance travelled to get rid of any
  # extreme checklists
  dplyr::filter(DURATION_MINUTES >= 5) %>%
  dplyr::filter(DURATION_MINUTES <= 240) %>%
  replace_na(list(EFFORT_DISTANCE_KM = 0)) %>%
  dplyr::filter(EFFORT_DISTANCE_KM <= 5) %>%
  replace_na(list(EFFORT_AREA_HA = 0)) %>%
  dplyr::filter(as.numeric(as.character(EFFORT_AREA_HA)) <= 500) %>%
  # add season for each checklist
  mutate(season = getSeason_southern(OBSERVATION_DATE)) %>%
  mutate(season_num = case_when(season=="Summer" ~ "1",
                                season=="Autumn" ~ "2",
                                season=="Winter" ~ "3",
                                season=="Spring" ~ "4")) %>%
  mutate(season_num = as.numeric(as.character(.$season_num))) %>%
  # turn OBSERVER_ID into a factor
  mutate(OBSERVER_ID = as.factor(.$OBSERVER_ID))

## save this data as AUS_analysis_data
saveRDS(analysis_data, file="Data/AUS_analysis_data.RDS")








