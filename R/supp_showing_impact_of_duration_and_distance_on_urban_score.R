# Figure randomly sampling the median
# showing that the biases associated with the median are
# likely to be minimal

library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)

load("Data/species_urban.RData")




resample_median_function <- function(X) {
  
  df <- species_urban %>%
    group_by(COMMON_NAME) %>%
    summarise(N = n()) %>%
    dplyr::filter(N > 500) %>%
    ungroup() %>%
    inner_join(., species_urban, by="COMMON_NAME") %>%
    dplyr::filter(COMMON_NAME %in% list_of_species) %>%
    group_by(COMMON_NAME) %>%
    sample_n(X) %>%
    summarise(urban_score=median(avg_rad),
              mean_duration=mean(DURATION_MINUTES, na.rm=TRUE),
              mean_distance=mean(EFFORT_DISTANCE_KM, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(sample_size=X)
  
  return(df)
}

list_of_species <- unique(species_urban$COMMON_NAME)

sampling_vector <- seq.int(10, 500, by=10)


list_of_results_by_sample_size <- lapply(sampling_vector, function(X) {resample_median_function(X)})

df_of_results_by_sample_size <- bind_rows(list_of_results_by_sample_size) 



ggplot(df_of_results_by_sample_size, aes(x=sample_size, y=urban_score, group=COMMON_NAME))+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  xlab("Number of observations used")+
  ylab("Urban score")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("N=456 species")

#example
glance(summary(lm(urban_score ~ mean_duration + mean_distance, data=filter(df_of_results_by_sample_size, COMMON_NAME=="Australian Raven"))))

# run for all
nested_df <- df_of_results_by_sample_size %>%
  group_by(COMMON_NAME) %>%
  nest()


apply_lm <- function(df){
  lm(urban_score ~ mean_duration + mean_distance, data=df)      
}

nested_lms <- nested_df %>%
  mutate(lm = map(.f=apply_lm, .x=data))

tidy <- nested_lms %>%
  mutate(glance = map(.f=glance, .x=lm)) %>%
  unnest(glance)





