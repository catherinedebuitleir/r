library(dplyr)
library(ggplot2)

# Load data
df <- read.csv(
  file="~/Downloads/bank-additional-full.csv", 
  header=TRUE, 
  sep=";"
)

View(df)


df$Engaged <- as.numeric(as.factor(df$y))-1

engagementRate <- df %>% 
  group_by(Engaged) %>% 
  summarise(Count=n()) %>%
  mutate(Percentage=Count/nrow(df)*100.0)

View(engagementRate)

# Transpose
transposed <- t(engagementRate)

colnames(transposed) <- engagementRate$Engaged
transposed <- transposed[-1,]

Education <- df %>% 
  group_by(Engaged, Education = education) %>% 
  summarise(Count=n())

# pie chart
ggplot(Education, aes(x="", y=Count, fill=Education)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Engaged) +
  ggtitle('Education (0: Not Engaged, 1: Engaged)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )


