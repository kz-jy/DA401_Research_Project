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

SR<-SuicideRisk %>%
  select(DSTHOP30, mhsuithk, mhsuipln, mhsuitry, K6SCMON, SMIPP_U, AUNMPSY2, AUNMPGE2, AUNMMED2, AUNMAHS2, AUNMRES2,
         AUNMSFA2, AUNMMEN2, AUNMTHE2, AUNMDOC2, AUNMCLN2, AUNMDTM2, AUNMOTO2, amdelt, ASDSOVL2, CATAG6, irmarit,
         irsex, IREDUHIGHST2, NEWRACE2, wrkdpstyr, income)  # alcohol use disorder

#Assign risk level
SR<-SR %>% mutate(risk_level = case_when(as.numeric(DSTHOP30 != 98 & DSTHOP30<3) + as.numeric(mhsuithk==1)+ 
                                           as.numeric(mhsuipln == 1) + as.numeric(mhsuitry == 1) +
                                           as.numeric(K6SCMON > 16) + as.numeric(SMIPP_U > 0.65) >= 3 ~"high",
                                         as.numeric(DSTHOP30 != 98 & DSTHOP30 == 3) + as.numeric(mhsuithk == 1) +
                                           as.numeric(mhsuipln == 1 | mhsuitry == 1) + 
                                           as.numeric(K6SCMON <= 16 & K6SCMON >= 8) + 
                                           as.numeric(SMIPP_U <= 0.65 & SMIPP_U > 0.3) >= 3 ~"moderate",
                                         as.numeric(DSTHOP30 != 98 & DSTHOP30>3) + as.numeric(K6SCMON < 8) +
                                           as.numeric(mhsuithk== 1 | mhsuipln == 1 | mhsuitry == 1) +
                                           as.numeric(SMIPP_U <= 0.3) >= 2 ~ "low"))
SR$risk_level[is.na(SR$risk_level)] <- "no"


table(SR$risk_level)
sum(is.na(SR$risk_level))
