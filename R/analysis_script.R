# An R script for the analysis of the greenspace data
# this script has multiple parts
# part 1: show the raw correlation between Ualpha and Halpha
# part 2: use a gam to show the predicted correlation between Ualpha and Halpha
# part 3: Calculate Ualpha, Ugamma, and Ubeta for each greenspace

## packages
library(dplyr)
library(readr)
library(mgcv)
library(ggplot2)
library(purrr)

## source functions
source("R/global_functions.R")

## read data in
dataset <- readRDS("Data/AUS_analysis_data.RDS")


## plot raw data for the ten Australian urban greenspaces with a geom_smooth = lm
ggplot(dataset, aes(x=urban_index, y=species_diversity, color=LOCALITY_ID))+
  facet_wrap(~season, scales="free")+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  xlab("Urban index on a checklist")+
  ylab("Shannon entropy on a checklist")

## we know (and show below) that Australia doesn't have much effect of season on the diversity values
## so let's plot them without season considered
ggplot(dataset)+
  geom_smooth(aes(x=urban_index, y=species_diversity, linetype=LOCALITY_ID), color="gray80", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=species_diversity), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote('Urban index on a checklist ('*~U[s]~')'))+
  ylab("Shannon entropy on a checklist (exp(H'))")

## Will come back to this figure at the end!!


## fit an overall gam with all the data
## then predict out the average checklist among the sequence of urban indices
## and plot the predictions
## first convert LOCALITY_ID to a factor
dataset$LOCALITY_ID <- as.factor(dataset$LOCALITY_ID)

## fit the gamm
all_gamm <- mgcv::gamm(species_diversity ~ s(urban_index, bs = "cs") +
                         s(season_num, bs="cc", k=4) + 
                         s(DURATION_MINUTES, bs = "tp") + 
                         s(EFFORT_DISTANCE_KM, bs = "tp") + 
                         s(OBSERVER_ID, bs="re") +
                         s(LOCALITY_ID, bs="re"), 
                         data=dataset, method = "REML")

## investigate gamm plots
#plot.gam(all_gamm$gam)


## predict new data based on an 'average checklist' across the range of 
## potential urban indices
## but stratify this to each LOCALITY_ID
new.data_effort <- dataset %>%
  group_by(LOCALITY_ID) %>%
  summarise(DURATION_MINUTES = mean(DURATION_MINUTES),
            EFFORT_DISTANCE_KM = mean(EFFORT_DISTANCE_KM),
            min_ui = min(urban_index),
            max_ui = max(urban_index),
            N=n())


# probably a safer way rather than relying on explicit index?
new.data_temp <- lapply(lapply(1:nrow(new.data_effort), function(x, data) data[x,], new.data_effort),
                        seq_function)

## extract observer ID even though we will exclude this
OBSERVER_season <- dataset %>%
  dplyr::select(OBSERVER_ID, season_num, LOCALITY_ID) %>%
  arrange(LOCALITY_ID) %>%
  dplyr::select(OBSERVER_ID, season_num)

## create new data for prediction
new.data <- bind_cols(
  bind_rows(new.data_temp) %>%
  inner_join(., new.data_effort, by="LOCALITY_ID") %>%
  dplyr::select(urban_index, LOCALITY_ID, DURATION_MINUTES, EFFORT_DISTANCE_KM) %>%
  arrange(LOCALITY_ID), 
  OBSERVER_season)

## get prediction
prediction_season <- bind_cols(
  data.frame(predicted_diversity = predict.gam(all_gamm$gam, newdata = new.data, exclude = "s(OBSERVER_ID)")),
  new.data) %>%
  mutate(season = case_when(
    season_num==1 ~ "Summer",
    season_num==2 ~ "Autumn",
    season_num==3 ~ "Winter",
    season_num==4 ~ "Spring"))


## plot the predicted relationship between 
## shannon entropy and urban indices on a checklist
ggplot(prediction_season, aes(x=urban_index, y=predicted_diversity, color=season))+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~LOCALITY_ID, scales="free")+
  theme_classic()+
  xlab("Urban index")+
  ylab("Predicted Shannon entropy")

### we will show the above and demonstrate that seasonality is minimal in Australia
### which we know from a priori knowledge anyway that there is little seasonal effect in AUS
### this will enable us to show one plot with all greenspaces on there - cleaner.
### we include a plot demonstrating the minimal impact of season in supplementary material
prediction <- bind_cols(
  data.frame(predicted_diversity = predict.gam(all_gamm$gam, newdata = new.data, exclude = c("s(OBSERVER_ID)", "s(season_num)"))),
  new.data)

ggplot(prediction)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity, linetype=LOCALITY_ID), color="gray80", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote('Urban index on a checklist ('*~U[s]~')'))+
  ylab("Predicted Shannon entropy on a checklist (exp(H'))")

##-----------------------------------------------------------------------------##
## Now let's repeat this process to demonstrate the same thing
## but with species richness and urban index on a checklist
## we follow from above and 'neglect' season here
## so let's plot them without season considered
ggplot(dataset)+
  geom_smooth(aes(x=urban_index, y=species_richness, linetype=LOCALITY_ID), color="gray80", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=species_richness), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote('Urban index on a checklist ('*~U[s]~')'))+
  ylab("Species richness on a checklist (SR)")  


## now let's fit a gamm and get predicted results out
## fit the gamm
all_gamm_SR <- mgcv::gamm(species_richness ~ s(urban_index, bs = "cs") +
                         s(season_num, bs="cc", k=4) + 
                         s(DURATION_MINUTES, bs = "tp") + 
                         s(EFFORT_DISTANCE_KM, bs = "tp") + 
                         s(OBSERVER_ID, bs="re") +
                         s(LOCALITY_ID, bs="re"), 
                       data=dataset, method = "REML")

## investigate gamm plots
#plot.gam(all_gamm_SR$gam)

## the gam plots look pretty similar to the model fit for species diversity - as expected!
## predict new data based on an 'average checklist' across the range of 
## potential urban indices
## but stratify this to each LOCALITY_ID
## this 'new.data' will be the same as above, the only difference now
## is a different response variable

## get prediction
prediction_SR_season <- bind_cols(
  data.frame(predicted_richness = predict.gam(all_gamm_SR$gam, newdata = new.data, exclude = "s(OBSERVER_ID)")),
  new.data) %>%
  mutate(season = case_when(
    season_num==1 ~ "Summer",
    season_num==2 ~ "Autumn",
    season_num==3 ~ "Winter",
    season_num==4 ~ "Spring"))


## plot the predicted relationship between 
## shannon entropy and urban indices on a checklist
ggplot(prediction_SR_season, aes(x=urban_index, y=predicted_richness, color=season))+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~LOCALITY_ID, scales="free")+
  theme_classic()+
  xlab("Urban index")+
  ylab("Predicted species richness")

### we will show the above and demonstrate that seasonality is minimal in Australia
### which we know from a priori knowledge anyway that there is little seasonal effect in AUS
### this will enable us to show one plot with all greenspaces on there - cleaner.
### can put a plot demonstrating the minimal impact of season in supplementary material
prediction_SR <- bind_cols(
  data.frame(predicted_diversity = predict.gam(all_gamm_SR$gam, newdata = new.data, exclude = c("s(OBSERVER_ID)", "s(season_num)"))),
  new.data)

ggplot(prediction_SR)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity, linetype=LOCALITY_ID), color="gray80", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote('Urban index on a checklist ('*~U[s]~')'))+
  ylab("Predicted species richness on a checklist (SR)")



### Now, I think we have the four panels for figure 2 for the paper
### lets use patchwork to put the figure together
library(patchwork)

## remake the panels, labeled a, b, c, d
# raw lm fit for species diversity versus urban index
a <- ggplot(dataset)+
  geom_smooth(aes(x=urban_index, y=species_diversity, linetype=LOCALITY_ID), color="gray50", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=species_diversity), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote(''))+
  ylab("Checklist Shannon entropy (exp(H'))")+
  guides(linetype=guide_legend(ncol=2))+
  theme(legend.position="top")+
  theme(legend.background = element_rect(color="black", linetype="solid"))+
  theme(axis.text = element_text(size=10, color="black"))+
  theme(axis.title = element_text(size=12, color="black"))

# predicted lm fit for species diversity and urban index
b <- ggplot(prediction)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity, linetype=LOCALITY_ID), color="gray50", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote(''))+
  ylab("")+
  guides(linetype=FALSE)+
  theme(axis.text = element_text(size=10, color="black"))+
  theme(axis.title = element_text(size=12, color="black"))

# raw lm fit for species richness versus urban index
c <- ggplot(dataset)+
  geom_smooth(aes(x=urban_index, y=species_richness, linetype=LOCALITY_ID), color="gray50", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=species_richness), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote('Checklist urban index ('*~U[s]~')'))+
  ylab("Checklist species richness (SR)")+
  guides(linetype=FALSE)+
  theme(axis.text = element_text(size=10, color="black"))+
  theme(axis.title = element_text(size=12, color="black"))  

# predicted lm fit for species richness versus urban index
d <- ggplot(prediction_SR)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity, linetype=LOCALITY_ID), color="gray50", method="lm", se=FALSE)+
  geom_smooth(aes(x=urban_index, y=predicted_diversity), method="lm", se=FALSE, size=3, color="black")+
  theme_classic()+
  xlab(bquote('Checklist urban index ('*~U[s]~')'))+
  ylab("")+
  guides(linetype=FALSE)+
  theme(axis.text = element_text(size=10, color="black"))+
  theme(axis.title = element_text(size=12, color="black"))

library(ggpubr)
ggarrange(a, b, c, d, ncol=2, nrow=2, common.legend=TRUE, legend="bottom")


##------------------------------------------------------------------------------##

## Now we want to calculate Ualpha for every greenspace in the data
## this is the mean urban index across all checklists at a greenspace
## collapsed to 1 value
## But, still probably necessary to fit a gamm because we expect effects of sampling
## on the urban index at a checklist level... I think anyway? 
## Alternatively, we can show that sampling does not effect the urban index and thus
## we can just take the mean of all urban indices across all checklists for each greenspace

## let's plot relationship between urban index and sampling time
ggplot(dataset, aes(x=urban_index, y=DURATION_MINUTES))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~LOCALITY_ID, scales="free")

## looks like that generally the longer you spend birding the lower the urban score will be...
## justification for modelling this out using a gamm

## I'll try copying the method from above first
## fit the gamm
urban_alpha_gamm <- mgcv::gamm(urban_index ~ s(season_num, bs="cc", k=4) + 
                                 s(DURATION_MINUTES, bs = "tp") + 
                                 s(EFFORT_DISTANCE_KM, bs = "tp") + 
                                 s(OBSERVER_ID, bs="re") +
                                 s(LOCALITY_ID, bs="re"), 
                                 data=dataset, method = "REML")

## look at the plots
plot.gam(urban_alpha_gamm$gam)

## interesting - looks like time does matter but distance has little effect!
## Now let's create new data frame for calculating Ualpha for each greenspace
Ualpha_new.data <- dataset %>% 
  group_by(LOCALITY_ID, season_num) %>%
  summarise(DURATION_MINUTES = mean(DURATION_MINUTES),
            EFFORT_DISTANCE_KM = mean(EFFORT_DISTANCE_KM)) %>%
  mutate(OBSERVER_ID = "obsr110354")

## Now predict new data
Ualpha_prediction <- bind_cols(data.frame(Ualpha=predict.gam(urban_alpha_gamm$gam, newdata = Ualpha_new.data, exclude = "s(OBSERVER_ID)")),
                        Ualpha_new.data) %>%
  mutate(season = case_when(
    season_num==1 ~ "Summer",
    season_num==2 ~ "Autumn",
    season_num==3 ~ "Winter",
    season_num==4 ~ "Spring"))

## make a quick plot
ggplot(Ualpha_prediction, aes(x=season, y=Ualpha, color=LOCALITY_ID, group=LOCALITY_ID))+
  geom_point()+
  geom_line()+
  theme_classic()


## write out Ualpha as its own dataframe
Ualpha_prediction %>%
  dplyr::select(Ualpha, LOCALITY_ID, season) %>%
  write_csv(., "Data/Ualpha.csv")








