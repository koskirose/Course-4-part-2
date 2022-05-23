####-----------Evaluate Techniques for WiFi Locating-----------####
####--------------by Sherri Koski------------------------------####
####-----------Initial Data Exploration------------------------####



library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(gridExtra) #used with grid.arrange()
library(caret)

####--------------Import training and validation data---------

WlanTrainingData <- read.csv("trainingData.csv", header = TRUE)

WlanValidationData <- read.csv("validationData.csv", header = TRUE)

####-------------Preprocessing ----

## Change the Timestamp Data Type
WlanTrainingData %>%
  mutate_at(c("TIMESTAMP"), as.character) %>%
  mutate_at(c("TIMESTAMP"), as.numeric) -> WlanTrainingData

WlanValidationData %>%
  mutate_at(c("TIMESTAMP"), as.character) %>%
  mutate_at(c("TIMESTAMP"), as.numeric) -> WlanValidationData


#Convert to Unix Time

WlanTrainingData$TIMESTAMP <- as.POSIXct((WlanTrainingData$TIMESTAMP), 
                               origin = "1970-01-01",
                               tz = "GMT")

WlanValidationData$TIMESTAMP <- as.POSIXct((WlanValidationData$TIMESTAMP),
                                           origin = "1970-01-01",
                                           tz = "GMT")

####--------Data Exploration & Visualization

WAPStrain <- WlanTrainingData[,1:520]
WAPStrain[WAPStrain == 100] <- -105
WAPSrowmean <- data.frame(rowMeans(WAPStrain, na.rm=FALSE, dims=1))

##Create a Data Frame with the no-signal rows.
colnames(WAPSrowmean ) <- c("Row_Avg")

No_Signal_Train<- cbind(WlanTrainingData, WAPSrowmean) %>%
  filter(Row_Avg == -105)%>% 
  select(USERID, PHONEID, BUILDINGID, FLOOR, TIMESTAMP)


WAPSValid <- WlanValidationData[,1:520]
WAPSValid[WAPSValid == 100] <- -105
WAPSValidrowmeans <- data.frame(rowMeans(WAPSValid, na.rm=FALSE, dims = 1))

colnames(WAPSValidrowmeans)<-c("Row_Avg")


WAPSwithTIME <- cbind(WAPSrowmean, 
                      WlanTrainingData$USERID,
                      WlanTrainingData$PHONEID,
                      WlanTrainingData$BUILDINGID,
                      WlanTrainingData$FLOOR,
                      WlanTrainingData$TIMESTAMP) 

colnames(WAPSwithTIME) <- c("signal",
                            "user",
                            "phone",
                            "building",
                            "floor",
                            "time")

########Graph of which user have the strongest signal.  Closest to 0 is better.
##User 8 and 17 have poor signal 
WAPSwithTIME %>% 
  group_by(user) %>%
  summarize (avg = mean(signal)) %>%
  ggplot(aes(x=user, y = avg)) + 
  geom_point(size = 4)+ 
  scale_x_continuous(name= "user", breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  ggtitle("Average Signal Strength per User")

ggplot(WAPSwithTIME2, aes(x=user, y= phone))+
  geom_point()

WAPSwithTIME %>%
  group_by(building, floor) %>%
  summarise(avg=mean(signal))%>%
  ggplot(aes(x=building, y=avg, color=as.factor(floor))) + 
 geom_point(size= 5) + 
  ggtitle("Signal Strength in each Building and Floor")+
  scale_x_continuous(name= "Building Number", breaks = c(0,1,2))


 WAPSwithTIME %>%
  group_by(building, floor) %>%
  summarise(avg=mean(signal))%>%
  ggplot(aes(x=floor, y=avg, label = round(avg, 2)))+ 
  geom_point(size= 6, color = "pink")+facet_wrap(~building) + 
  geom_text(aes(label=round(avg, 2)))+
  ggtitle("Signal Strength in each building per Floor")
 
 WAPSwithTIME %>%
 ggplot(mapping = aes(group = building, y = signal))+ 
   geom_boxplot() +
   ggtitle("Signal strength in each Building")

WAPSwithTIME %>%
  group_by(day = day(time), user, phone, building, floor) %>%
  count(user) -> WAPSwithTIME2

WAPSwithTIME %>%
  group_by(day = date(time), user, phone, building, floor) %>%
  count(user) -> WAPSwithTIME3

write.csv(WAPSwithTIME3, file = "Wlangoupedwithtime.csv")

WAPSwithTIME %>%
  group_by(user, building, floor) %>%
  count(user) %>%
  ggplot(aes(x=user,y=floor )) + 
  geom_point(size = 4, color="purple")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  facet_wrap(~building)+ 
  ggtitle("Users on each building and each floor")
  
Total_Counts_users <- WAPSwithTIME2 %>%
  group_by(user)%>%
  summarise(total =sum(n)) %>%
  ggplot(aes(x=user, y=total))+
  geom_col(fill = 'blue')+
  ggtitle("Number of Data Points per user")

Total_Counts_users


  
# Signals by Time Visualizations
WAPSwithTIME %>% 
  filter(day(time) == 30) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "30 May 2013 Thursday - Signals by Time User 11") -> plot30may

WAPSwithTIME %>% 
  filter(day(time) == 31) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "31 May 2013 Thursday - Signals by Time User 11") -> plot31may

WAPSwithTIME %>% 
  filter(day(time) == 4) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "4 June 2013 Thursday - Signals by Time User 11") -> plot4june

WAPSwithTIME %>% 
  filter(day(time) == 10) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "10 June 2013 Thursday - Signals by Time User 1") -> plot10june

WAPSwithTIME %>% 
  filter(day(time) == 12) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "12 June 2013 Thursday - Signals by Time User 1") -> plot12june

WAPSwithTIME %>% 
  filter(day(time) == 20) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "20 June 2013 Thursday - Signals by Time everybody") -> plot20june

grid.arrange(plot30may, plot31may, plot4june, plot10june, plot12june, plot20june)

# Users by Time Visualizations
WAPSwithTIME %>% 
  filter(day(time) == 30) %>% 
  ggplot(aes(time)) +
  geom_point(aes(y = user), color = "red") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  labs(title = "30 May 2013 Thursday ") -> plot30may_users

WAPSwithTIME %>% 
  filter(day(time) == 31) %>% 
  ggplot(aes(time)) +
  geom_point(aes(y = user), color = "red") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  labs(title = "31 May 2013 Thursday ") -> plot31may_users

WAPSwithTIME %>% 
  filter(day(time) == 4) %>% 
  ggplot(aes(time)) +
  geom_point(aes(y = user), color = "red") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  labs(title = "4 June 2013 Thursday ") -> plot4june_users

WAPSwithTIME %>% 
  filter(day(time) == 10) %>% 
  ggplot(aes(time)) +
  geom_point(aes(y = user), color = "red") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  labs(title = "10 June 2013 Thursday ") -> plot10june_users

WAPSwithTIME %>% 
  filter(day(time) == 12) %>% 
  ggplot(aes(time)) +
  geom_point(aes(y = user), color = "red") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  labs(title = "12 June 2013 Thursday ") -> plot12june_users

WAPSwithTIME %>% 
  filter(day(time) == 20) %>% 
  ggplot(aes(time)) +
  geom_point(aes(y = user), color = "red") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  labs(title = "20 June 2013 Thursday ") -> plot20june_users
plot20june_users

grid.arrange(plot30may_users, plot31may_users, plot4june_users, plot10june_users, plot12june_users, plot20june_users)

## Floor Analysis
WlanTrainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "blue") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 by Floor")

WlanTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "blue") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 by Floor")

WlanTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "blue") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 2 by Floor")

#Compare latitude/longitude points between the 3 buildings
Lat_Long_plot_0 <- WlanTrainingData %>%
  filter(BUILDINGID == 0)%>%
  ggplot(aes(LONGITUDE, LATITUDE)) + 
  geom_point(color = "darkgreen") + 
  ggtitle("Latitude Longitude Building 0")

Lat_Long_plot_1 <- WlanTrainingData %>%
  filter(BUILDINGID == 1)%>%
  ggplot(aes(LONGITUDE, LATITUDE)) + 
  geom_point(color = "darkgreen") +
  ggtitle("Latitude Longitude Building 1")

Lat_Long_plot_2 <- WlanTrainingData %>%
  filter(BUILDINGID == 2)%>%
  ggplot(aes(LONGITUDE, LATITUDE)) + 
  geom_point(color = "darkgreen")+
  ggtitle("Latitude Longitude Building 2")

grid.arrange(Lat_Long_plot_0, Lat_Long_plot_1, Lat_Long_plot_2)

WlanTrainingData %>%
  ggplot(aes(LONGITUDE, LATITUDE, color = as.factor(BUILDINGID))) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1")+
  ggtitle("Latitude and Longitude between Buildings")

WlanTrainingData %>%
  filter(USERID == 1|  USERID == 2|
         USERID == 3|  USERID == 4|
         USERID == 5|  USERID == 6|
         USERID == 7|  USERID == 8|
         USERID == 9) %>%
  ggplot(aes(LONGITUDE, LATITUDE, color = as.factor(USERID))) + 
  geom_point() + 
  scale_colour_brewer(palette = "Set1")+
  ggtitle("Latitude and Longitude between where users 1-9 went")

WlanTrainingData %>%
  filter(USERID == 10|  USERID == 11|
           USERID == 12|  USERID == 13|
           USERID == 14|  USERID == 15|
           USERID == 16|  USERID == 17|
           USERID == 18) %>%
  ggplot(aes(LONGITUDE, LATITUDE, color = as.factor(USERID))) + 
  geom_point() + 
  scale_colour_brewer(palette = "Set1")+
  ggtitle("Latitude and Longitude between where users 9-18 went")




a<-WlanTrainingData %>%
  filter(BUILDINGID == 0) %>%
  summarize(min(LONGITUDE), max(LONGITUDE))

b<-WlanTrainingData %>% 
  filter(BUILDINGID == 1) %>%
  summarise(min(LONGITUDE), max(LONGITUDE))

c <- WlanTrainingData %>% 
  filter(BUILDINGID == 2) %>%
  summarise(min(LONGITUDE), max(LONGITUDE))

d <-rbind(a,b,c)
d

WlanTrainingData %>%
  group_by(USERID) %>%
  ggplot(aes(x= BUILDINGID)) + geom_bar() + 
  facet_wrap(~USERID) + 
  labs(title = "What buildings the user was in")

WlanTrainingData %>%
  group_by(USERID) %>%
  ggplot(aes(x= USERID)) + geom_bar() + 
  facet_wrap(~BUILDINGID) +
  labs(title = "Who was in this building")

WlanTrainingData_w_Means <- cbind(WlanTrainingData, WAPSrowmean)
WlanTrainingData_w_Means %>%
  group_by(USERID)%>%
  ggplot(aes(x= as.factor(USERID), y= Row_Avg)) + 
  geom_boxplot()


summary(WlanTrainingData[,520:529])
str(WlanTrainingData[,520:529])

summary(WlanValidationData[, 520:529])
str(WlanTrainingData[,520:529])

WlanTrainingData %>%
  group_by(BUILDINGID, FLOOR, USERID, PHONEID)%>%
  count()-> GroupedData

write.csv(GroupedData, file = "FilteredWlanData.csv")

WlanValidationData %>%
  group_by(BUILDINGID, FLOOR, USERID, PHONEID)%>%
  count() -> GroupedValidData

write.csv(GroupedValidData, file = "FilteredWlanValidData.csv")











