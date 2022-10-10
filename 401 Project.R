#setwd("~/Desktop/DA 401/Code相关/ICPSR_06693 2/DS0001")
#load("35067-0001-Data.rda")

#library(haven)
library(dplyr)
#ncs<-read_dta("06693-0001-Data.dta")

setwd("~/Desktop/DA 401/Code相关")
load("NSDUH_2020.RData")
load("NSDUH_2019.RData")
load("NSDUH_2018.RData")

t20<-NSDUH_2020 %>%
  filter(auinpyr == 1|auoptyr == 1)
t19<-PUF2019_100920 %>%
  filter(auinpyr == 1|auoptyr == 1)
t18_yes<-PUF2018_100819 %>%
  filter(auinpyr == 1|auoptyr == 1)

t<-merge(t20, t19, t18)
