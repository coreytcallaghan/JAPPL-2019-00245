library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

load("Data/species_urban.RData")


buffer_1 <- read_csv("Data/Additional buffers for night-time lights/AUSTRALIA_EBIRD_UPTOFEB2018_BUFFER_1000_ERROR_500_REDUCTION_SCALE_500.csv") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, avg_rad) %>%
  rename(avg_rad_1 = avg_rad)

buffer_2 <- read_csv("Data/Additional buffers for night-time lights/AUSTRALIA_EBIRD_UPTOFEB2018_BUFFER_2000_ERROR_500_REDUCTION_SCALE_500.csv") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, avg_rad) %>%
  rename(avg_rad_2 = avg_rad)

buffer_10 <- read_csv("Data/Additional buffers for night-time lights/AUSTRALIA_EBIRD_UPTOFEB2018_BUFFER_10000_ERROR_500_REDUCTION_SCALE_500.csv") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, avg_rad) %>%
  rename(avg_rad_10 = avg_rad)

buffer_15 <- read_csv("Data/Additional buffers for night-time lights/AUSTRALIA_EBIRD_UPTOFEB2018_BUFFER_15000_ERROR_500_REDUCTION_SCALE_500.csv") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, avg_rad) %>%
  rename(avg_rad_15 = avg_rad)


different_buffers <- species_urban %>%
  rename(avg_rad_5 = avg_rad) %>%
  left_join(., buffer_1, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., buffer_2, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., buffer_10, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., buffer_15, by="SAMPLING_EVENT_IDENTIFIER")


species_comparison <- different_buffers %>%
  group_by(COMMON_NAME) %>%
  summarise(urban_score_5 = median(avg_rad_5),
            urban_score_1 = median(avg_rad_1),
            urban_score_10 = median(avg_rad_10),
            urban_score_2 = median(avg_rad_2),
            urban_score_15 = median(avg_rad_15)) %>%
  gather(key="Buffer_size", value=value, 2:6)


sd <- species_comparison %>%
  group_by(COMMON_NAME) %>%
  summarise(sd=sd(value))

ggplot(sd, aes(sd))+
  geom_histogram(fill="black", color="white", bins=50)+
  theme_classic()+
  xlab("Standard deviation of urban scores")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("Standard deviation of urban scores from 1, 2, 5, 10, 15 km buffers")+
  geom_label(x=4, y=300, label=paste0("Median of the standard deviation values:", round(median(sd$sd), digits=3)))


sd2 <- species_comparison %>%
  dplyr::filter(Buffer_size != "urban_score_15") %>%
  group_by(COMMON_NAME) %>%
  summarise(sd=sd(value))

ggplot(sd2, aes(sd))+
  geom_histogram(fill="black", color="white", bins=50)+
  theme_classic()+
  xlab("Standard deviation of urban scores")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("Standard deviation of urban scores from 1, 2, 5, 10 km buffers")+
  geom_label(x=4, y=300, label=paste0("Median of the standard deviation values:", round(median(sd2$sd), digits=3)))

sd3 <- species_comparison %>%
  dplyr::filter(Buffer_size != "urban_score_15") %>%
  dplyr::filter(Buffer_size != "urban_score_10") %>%
  group_by(COMMON_NAME) %>%
  summarise(sd=sd(value))

ggplot(sd3, aes(sd))+
  geom_histogram(fill="black", color="white", bins=50)+
  theme_classic()+
  xlab("Standard deviation of urban scores")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("Standard deviation of urban scores from 1, 2, 5 km buffers")+
  geom_label(x=4, y=300, label=paste0("Median of the standard deviation values:", round(median(sd3$sd), digits=3)))


library(GGally)


species_comparison2 <- different_buffers %>%
  group_by(COMMON_NAME) %>%
  summarise(urban_score_5 = median(avg_rad_5),
            urban_score_1 = median(avg_rad_1),
            urban_score_10 = median(avg_rad_10),
            urban_score_2 = median(avg_rad_2),
            urban_score_15 = median(avg_rad_15))


ggpairs(species_comparison2[2:6])+
  theme_classic()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))
