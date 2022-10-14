#library(haven)
library(dplyr)
library(caret)
library(keras)
library(tensorflow)

setwd("~/Documents/GitHub/DA401_SuicideRisk_research")
load("NSDUH_2020.RData")
load("NSDUH_2019.RData")
load("NSDUH_2018.RData")

#Data cleaning
true20<- NSDUH_2020%>%
  filter(auinpyr == 1|auoptyr == 1)
true19<-PUF2019_100920 %>%
  filter(auinpyr == 1|auoptyr == 1)
true18<-PUF2018_100819 %>%
  filter(auinpyr == 1|auoptyr == 1)
true18$QUESTID2<-as.integer(true18$QUESTID2)
true19$QUESTID2<-as.integer(true19$QUESTID2)
SuicideRisk<-bind_rows(true20, true19, true18)



