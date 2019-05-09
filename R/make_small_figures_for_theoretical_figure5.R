## An R script to make a small figures for 
## Centennial Park which will be inserted into Figure 5

### packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)


### read in data to calculate Ubeta
Ugamma <- read_csv("Data/Ugamma.csv")
Ualpha <- read_csv("Data/Ualpha.csv")

### subset to CENPARK and combine dataframes
### combine files and calculate Ubeta
Ubeta <- Ualpha %>%
  inner_join(., Ugamma, by="LOCALITY_ID") %>%
  mutate(Ubeta = Ugamma/Ualpha) %>%
  filter(LOCALITY_ID == "L945869")

### plot of the metrics for each season
Ubeta %>%
  gather(., key="key", value="value", Ubeta, Ualpha, Ugamma) %>%
  ggplot(., aes(x=season, y=value, color=key, group=key))+
  geom_point()+
  geom_line()+
  xlab("")+
  ylab("")+
  scale_x_discrete("season", labels=c("Summer", "Autumn", "Winter", "Spring"))+
  theme_classic()+
  facet_wrap(~key, scales="free")


### get the list of species and their scores used to calculate Ugamma
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

data <- readRDS("Data/AUS_data_site_rds/dropchop_buffer_L945869.RDS")

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


## plot species pool distribution of urban scores
Ugammma <- unname(quantile(species$urban_index, 0.25))
Ualpha <- 2.27


ggplot(species, aes(urban_index))+
  geom_density(fill="gold3", alpha=0.2)+
  theme_classic()+
  ylab("Density")+
  xlab("Urban score")+
  geom_vline(xintercept = 0.76, linetype="solid", size=1.5, color="green")+
  geom_vline(xintercept = 2.27, linetype="solid", size=1.5, color="red")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7))




## Make a second figure showing change through time

ggplot(species, aes(urban_index))+
  geom_density(fill="gold3", alpha=0.2)+
  theme_classic()+
  ylab("Density")+
  xlab("Urban score")+
  geom_vline(xintercept = 0.76, linetype="solid", size=0.8, color="green")+
  geom_vline(xintercept = 2.27, linetype="solid", size=0.8, color="red")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7)) +
  ggtitle("Year 0")


ggplot(species, aes(urban_index))+
  geom_density(fill="gold3", alpha=0.2)+
  theme_classic()+
  ylab("")+
  xlab("Urban score")+
  geom_vline(xintercept = 0.76, linetype="solid", size=0.8, color="green")+
  geom_vline(xintercept = 2.00, linetype="solid", size=0.8, color="red")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7)) +
  ggtitle("Year 1")

ggplot(species, aes(urban_index))+
  geom_density(fill="gold3", alpha=0.2)+
  theme_classic()+
  ylab("")+
  xlab("Urban score")+
  geom_vline(xintercept = 0.76, linetype="solid", size=0.8, color="green")+
  geom_vline(xintercept = 1.64, linetype="solid", size=0.8, color="red")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7)) +
  ggtitle("Year 2")



ggplot(species, aes(urban_index))+
  geom_density(fill="gold3", alpha=0.2)+
  theme_classic()+
  ylab("")+
  xlab("Urban score")+
  geom_vline(xintercept = 0.76, linetype="solid", size=0.8, color="green")+
  geom_vline(xintercept = 1.21, linetype="solid", size=0.8, color="red")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7)) +
  ggtitle("Year 3")









