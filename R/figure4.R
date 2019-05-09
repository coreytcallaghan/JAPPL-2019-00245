#### A script for a potential figure 4


#### packages
library(readr)
library(dplyr)
library(ggplot2)
library(geosphere)
library(ggmap)
library(vegan)
library(data.table)
library(RANN)
library(tidyr)


##### read in all data
## Read in raw eBird data for all of Australia
## split into 8 different datasets so it could fit onto my machine

df1 <- readRDS("Data/ebird_data1.RDS")
df2 <- readRDS("Data/ebird_data2.RDS")
df3 <- readRDS("Data/ebird_data3.RDS")
df4 <- readRDS("Data/ebird_data4.RDS")
df5 <- readRDS("Data/ebird_data5.RDS")
df6 <- readRDS("Data/ebird_data6.RDS")
df7 <- readRDS("Data/ebird_data7.RDS")
df8 <- readRDS("Data/ebird_data8.RDS")
ebird_AUS_data <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)
indices <- read_csv("Data/australian_indices.csv")

### get coords for each of the three major cities
### Need to navigate google maps API in order to do this
#melbourne <- geocode("Melbourne, Australia")
#sydney <- geocode("Sydney, Australia")
#brisbane <- geocode("Brisbane, Australia")

# Alternatively can do it manually - because there are only three cities
melbourne <- data.frame(lat=-37.811689, lon=144.961853)
sydney <- data.frame(lat=-33.875906, lon=151.209483)
brisbane <- data.frame(lat=-27.469684, lon=153.025282)

### filter and collapse all checklists to the best quality checklists
colnames(ebird_AUS_data) <- gsub(" |/", "_", colnames(ebird_AUS_data))

## clean data
## here is where you add all the columns needed for the analysis (that don't vary within checklist)
sampling_event_info <- ebird_AUS_data %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LOCALITY, LOCALITY_ID, OBSERVATION_DATE,
                PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, 
                DURATION_MINUTES, OBSERVER_ID, LATITUDE, LONGITUDE) %>%
  distinct()


## Counts how many 'x's per checklist
X_missing <- ebird_AUS_data %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION_COUNT=="X"))


## combines species and subspecies into one count
clean_data <- ebird_AUS_data %>%
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
  # filter out any values below the 0.025 quantile
  # and any values greater than the 0.975 quantile
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
  dplyr::filter(as.numeric(as.character(EFFORT_AREA_HA)) <= 500)





### Calculate the nearest city for each checklist
cities <- bind_rows(melbourne, brisbane, sydney) %>%
  mutate(city=c("Melbourne", "Brisbane", "Sydney"))
  
  
  
  
closest <- as.data.frame(nn2(cities[,2:1], analysis_data[, 12:11], 1))


data_closest <- cbind(analysis_data, closest)

city_id <- as.data.frame(cities$city)
city_id$id <- 1:nrow(city_id)

locality_closest_city <- data_closest %>%
  rename(id=nn.idx) %>%
  inner_join(., city_id, by="id") %>%
  rename(city = "cities$city") %>%
  inner_join(., cities, by="city")

## brisbane dataset
brisbane <- locality_closest_city %>% 
  filter(city == "Brisbane")
  
setDT(brisbane)[, distance.km := distGeo(matrix(c(lon, lat), ncol=2),
                                           matrix(c(LONGITUDE, LATITUDE), ncol=2))/1000]

brisbane <- brisbane %>%
  filter(distance.km <= 100)


## sydney dataset
sydney <- locality_closest_city %>%
  filter(city == "Sydney")

setDT(sydney)[, distance.km := distGeo(matrix(c(lon, lat), ncol=2),
                                         matrix(c(LONGITUDE, LATITUDE), ncol=2))/1000]


sydney <- sydney %>%
  filter(distance.km <= 100)

## melbourne dataset
melbourne <- locality_closest_city %>%
  filter(city == "Melbourne")

setDT(melbourne)[, distance.km := distGeo(matrix(c(lon, lat), ncol=2),
                                       matrix(c(LONGITUDE, LATITUDE), ncol=2))/1000]


melbourne <- melbourne %>%
  filter(distance.km <= 100)

## combine the datasets
plot_data <- bind_rows(brisbane, sydney, melbourne)

ggplot(plot_data)+
  geom_smooth(aes(x=distance.km, y=urban_index, color=city, group=city), method="lm", se=FALSE)+
  theme_classic()+
  theme(legend.position=c(0.8, 0.75))+
  theme(legend.background = element_rect(color="black", linetype="solid"))+
  theme(axis.text = element_text(size=10, color="black"))+
  theme(axis.title = element_text(size=12, color="black"))+
  xlab("Distance from city (km)")+
  ylab("Checklist urban index")+
  scale_color_manual("      City", breaks=c("Melbourne", "Sydney", "Brisbane"), values=c("seagreen4","dodgerblue1", "chocolate2"))+
  guides(linetype=FALSE)




