
library(rpart)
library(rattle)

df <- read.csv(
  file="../data/bank-full.csv", 
  header=TRUE, 
  sep=";"
)


View(df)

# convert yes and no to 1 and 0 
df$conversion <- as.numeric(as.factor(df$y))-1

View(df$conversion)

# get conversion rate
sprintf("conversion rate: %0.2f%%", sum(df$conversion)/nrow(df)*100.0)


# group conversion rate by job 
conversionsByJob <- df %>% 
  group_by(Job=job) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)

View(conversionsByJob)

# getplot of summary 
ggplot(conversionsByJob, aes(x=Job, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  coord_flip() +
  ggtitle('Conversion Rates by Job') +
  xlab("Job") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# get deafult rate by conversions 
defaultByConversion <- df %>% 
  group_by(Default=default, Conversion=conversion) %>% 
  summarise(Count=n())
View(defaultByConversion)

# get pie chart 
ggplot(defaultByConversion, aes(x="", y=Count, fill=Default)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Conversion) +
  ggtitle('Default (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )



# get difference of distribution in duration 
ggplot(df, aes(x="", y=duration)) + 
  geom_boxplot() +
  facet_wrap(~conversion) +
  ylab("balance") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: duration") +
  theme(plot.title=element_text(hjust=0.5))


conversionsByNumContacts <- df %>% 
  group_by(Campaign=campaign) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)


unique(df$month)

months = lapply(month.abb, function(x) tolower(x))
df$month <- match(df$month, months)

View(months)

df %>% 
  group_by(month) %>% 
  summarise(Count=n())

df$job <- factor(df$job)
df$housing <- factor(df$housing)
df$marital <- factor(df$marital)


fit <- rpart(
  conversion ~ age + campaign + previous + housing + job + marital,
  method="class", 
  data=df,
  control=rpart.control(maxdepth=4, cp=0.0001)
)

fancyRpartPlot(fit)