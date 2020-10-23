# import the data

d <- read.csv("D:\\Machine\\accidents_2017.csv")

head(d)

colnames(d)

library(dplyr)
library(ggplot2)

d1 <- d %>% 
  group_by(Weekday) %>% 
  summarise(count = n())
d2 <- d %>% 
  group_by(Weekday) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n)*100) %>% 
  arrange(desc(freq))
d2

sd(d2$freq)

# devation from mean and adding new columns to it
d2$std <- sweep(d2[,3],2,mean(d2$freq))

# adding new which devides the std
d2 <- transform(d2, new = std / 2.92)
d2
d2$freq.2 <- NULL

# # Inference
# checked how many standard deviations is our data above or below the mean. 
# This clearly gives us some interesting insights:
#   
# Fridays record the most number of accidents.almost 0.95(STD) above the average accidents per weak.
# Saturday, Sunday encounter the least.
# Sunday lower among the both. What might be the reason? No Jobs ? 
# Not sure Sunday's also mean more amount of outings. But still,
# since weekends are seeing a sudden decline in number of accidents,
# it might mean due to no job goers.



# Now let's look at the part of the day when accidents occur the most.
d3 <- d %>% 
  select(Weekday,Part.of.the.day)

head(d3)

# as we know friday has the most accident occured so select only friday
d4 <- d3 %>% 
      filter(d3$Weekday=='Friday')
n_distinct(d4)
table(d4$Part.of.the.day)
prop.table(table(d4$Part.of.the.day))*100

# Inference
# So if we look closely, on Friday's (The most accident prone day) 
# more than half of the accidents occur during afternoons.
# About 40% occur during Morning and very less at nights (as normally expected)

table(d3$Part.of.the.day)
prop.table(table(d3$Part.of.the.day))*100


summary(d$Day)
describe(d$Day)


d5 <- data.frame(d$Day)
library(lubridate)




# for (i in 1:nrow(d)){
#   if(d$Day[i]  Day <= 7){
#     d$D.day[i] <- "w1"
#   }else if(d$Day[i] Day <= 14){
#     d$D.day[i] <- "w2")
#   }else {
#     d$D.day[i] <- "w4")
#   }
# }
# rm(d5)

w1 = d[d['Day']<=7]
w2 = d[(d['Day']>7) & (d['Day']<=14)]
w3 = d[(d['Day']>14) & (d['Day']<=21)]
w4 = d[(d['Day']>21) & (d['Day']<=31)]
# w1 = w1.describe()

# sum(w1)

d5 <- d %>% 
    select(Day) %>% 
    filter(Day<= 7)

d6 <- d %>% 
  select(Day) %>% 
  filter(Day> 7 & Day<=14)
d7 <- d %>% 
  select(Day) %>% 
  filter(Day >14 & Day<=21 )
d8 <- d %>% 
  select(Day) %>% 
  filter(Day>21 & Day<=31)

d5$weeksday <- d5[d5$weeksday<=7] <- "W1"
d6$weeksday <- d5[d5$weeksday<=14] <- "W2"
d7$weeksday <- d5[d5$weeksday<=21] <- "W3"
d8$weeksday <- d5[d5$weeksday<=31] <- "W4"

DD <- rbind(d5,d6,d7,d8)

rm(d5,d6,d7,d8)
table(DD$weeksday)

summary(DD)
describe(DD)

sapply(split(DD$Day,DD$weeksday),mean)

ba <- DD %>% 
  group_by(weeksday) %>% 
  summarise(Avg = mean(Day),
            med = median(Day),
            Std = sd(Day),
            minm = min(Day),
            Max = max(Day))
ba
quantile(DD$Day)
IQR(DD$Day)
prop.table(table(DD$weeksday))*100

# Looking at the results there is not much deviation of accidents in 4 weeks.
# However week 4 experiences a sudden increase in number of accidents.
# So probabilistically, week 4 Friday Afternoons are the worst for accidents.

# serious injuries
ds <- d %>%
      filter(d$Serious.injuries == 1)
names(ds)
ds <- ds[,c(2,4,5,7,9,11)]
head(ds)

prop.table(table(ds$Part.of.the.day))*100
prop.table(table(d$Part.of.the.day))*100

table(ds$Part.of.the.day)/table(d$Part.of.the.day)*100


dp <- ds$Day/table(ds$Part.of.the.day)
xtabs(d$Day~d$Part.of.the.day)/xtabs(ds$Day~ds$Part.of.the.day)
207/xtabs(ds$Day~ds$Part.of.the.day)*100
207/table(ds$Part.of.the.day)

table(ds$Weekday)

sapply(split(d$Day,d$Part.of.the.day),mean)
xtabs(ds$Day~ds$Part.of.the.day)/100









