library(ggplot2)

com_a <- data.frame(value=c(12.49, 1.23, 0.22, 2.61))
com_b <- data.frame(value=c(12.49, 9.58, 7.66, 2.61))

a_urban_index <- quantile(com_a$value, 0.25)
b_urban_index <- quantile(com_b$value, 0.25)

ggplot(com_a, aes(value))+
  geom_density(fill="green", alpha=0.2)+
  theme_classic()+
  ylab("Density")+
  xlab("Urban score")+
  geom_vline(xintercept = a_urban_index, linetype="solid", size=1.5, color="gray25")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7))


ggplot(com_b, aes(value))+
  geom_density(fill="red", alpha=0.2)+
  theme_classic()+
  ylab("Density")+
  xlab("Urban score")+
  geom_vline(xintercept = b_urban_index, linetype="solid", size=1.5, color="gray25")+
  theme(axis.text = element_text(size=5, color="black"))+
  theme(axis.title = element_text(size=7))+
  scale_y_continuous(position="right")


