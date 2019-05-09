### An R script to calculate Ubeta - UGII

### packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)

### read in data to calculate Ubeta
Ugamma <- read_csv("Data/Ugamma.csv")
Ualpha <- read_csv("Data/Ualpha.csv")

### combine files and calculate Ubeta
Ubeta <- Ualpha %>%
  inner_join(., Ugamma, by="LOCALITY_ID") %>%
  mutate(Ubeta = Ugamma/Ualpha)

### plot the metrics - scatterplot style
Ubeta %>%
  ggpairs(., mapping=ggplot2::aes(color=season, alpha=0.7), 
          columns=c("Ualpha", "Ubeta", "Ugamma"))


### plot by locality
Ubeta %>%
  gather(., key="key", value="value", Ubeta, Ualpha, Ugamma) %>%
  ggplot(., aes(x=season, y=value, color=key, group=key))+
  geom_point()+
  geom_line()+
  facet_wrap(~LOCALITY_ID, scales="free")+
  xlab("")+
  ylab("")+
  scale_x_discrete("season", labels=c("Summer", "Autumn", "Winter", "Spring"))




### Area/size of each greenspace
size <- data.frame(LOCALITY_ID = c("L967148", "L945869", "L923357", "L915566", "L3033858", "L1896576", "L1788977", "L1314800", "L1177484", "L1161973"),
                   Area_Ha = c(324, 285, 425, 106, 21, 24, 59, 38, 15, 397))


### collapse values to a single value and then merge
### with the size
### and then plot them
Ubeta %>%
  gather(., key="key", value="value", Ubeta, Ualpha, Ugamma) %>%
  group_by(LOCALITY_ID, key) %>%
  summarise(mean=mean(value)) %>%
  inner_join(., size, by="LOCALITY_ID") %>%
  ggplot(., aes(x=Area_Ha, y=mean, group=key, color=key))+
  geom_point()+
  facet_wrap(~key, scales="free")+
  geom_smooth(method="lm")+
  scale_x_log10()





