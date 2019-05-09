library(dplyr)
library(ggplot2)

load("Data/species_urban.RData")

scores_both <- species_urban %>%
  group_by(COMMON_NAME) %>%
  summarise(pop_score = median(population_density_mean),
            light_score = median(avg_rad))

ggplot(scores_both, aes(x=pop_score, y=light_score))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  ylab("Urban score based on lights")+
  xlab("Urban score based on pop. density")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("N=581 species")

