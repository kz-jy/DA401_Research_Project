library(dplyr)
library(ggplot2)
library(caret)
library(jtools)
library(nnet)
library(interactions)
library(rpart)
library(rpart.plot)
library(tensorflow)
library(keras)


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
  dplyr::select(DSTHOP30, mhsuithk, mhsuipln, mhsuitry, K6SCMON, SMIPP_U, AUNMPSY2, AUNMPGE2, AUNMMED2, AUNMAHS2, AUNMRES2,
         AUNMSFA2, AUNMMEN2, AUNMTHE2, AUNMDOC2, AUNMCLN2, AUNMDTM2, AUNMOTO2, amdelt, ASDSOVL2, CATAG6, irmarit,
         irsex, IREDUHIGHST2, NEWRACE2, wrkdpstyr, income, ALCWD2SX, alcndmor)

val1 = c(83,91,93,94,97,98,99) #value for the invalid data (unknow, no repsonse...)
SR <- as.data.frame(sapply(SR, function(x) replace(x, x %in% val1, 0)))
SR$DSTHOP30=as.numeric(factor(SR$DSTHOP30, levels = c(5,4,3,2,1), labels = c(1,2,3,4,5)))

#Assign risk level
SR<-SR %>% mutate(risk_level = case_when(as.numeric(DSTHOP30>4) + as.numeric(mhsuithk==1)+ 
                                           as.numeric(mhsuipln == 1) + as.numeric(mhsuitry == 1) +
                                           as.numeric(K6SCMON > 16) + as.numeric(SMIPP_U > 0.65) >= 3 ~"high",
                                         as.numeric(DSTHOP30<5 & DSTHOP30>1) + as.numeric(mhsuithk == 1) +
                                           as.numeric(mhsuipln == 1 | mhsuitry == 1) + 
                                           as.numeric(K6SCMON <= 16 & K6SCMON >= 8) + 
                                           as.numeric(SMIPP_U <= 0.65 & SMIPP_U > 0.3) >= 3 ~"moderate",
                                         as.numeric(DSTHOP30<3) + as.numeric(K6SCMON < 8) +
                                           as.numeric(mhsuithk== 1 | mhsuipln == 1 | mhsuitry == 1) +
                                           as.numeric(SMIPP_U <= 0.3) >= 2 ~ "low"))
SR$risk_level[is.na(SR$risk_level)] <- "no"

#Combine treatment from multiple facilities into a single var
val2 = c(985,994,997,998,999)
SR[7:18] <- sapply(SR[7:18], function(x) replace(x, x %in% val2, 0))
SR$inpatient<-rowSums(SR[7:12])
SR$outpatient <- rowSums(SR[13:18])


# Regression

#### Caluculate risk score
SR_reg<-SR
SR_reg<-SR_reg[c(-7:-18)]
SR_reg[c(7:17)] <- lapply(SR_reg[c(7:17)], factor)

SR_reg$K6SCMON = as.numeric(scale(SR_reg$K6SCMON))
SR_reg <- SR_reg %>% rowwise() %>%
  mutate(risk_score = sum(c_across(DSTHOP30:SMIPP_U)))

#### ANOVA
library(tidyverse)
library(ggpubr)
library(rstatix)

set.seed(123)
SR_reg %>% sample_n_by(ASDSOVL2, alcndmor, ALCWD2SX, income, wrkdpstyr, size = 1)

normality <- lm(risk_score ~ ASDSOVL2*alcndmor*ALCWD2SX*income*wrkdpstyr*IREDUHIGHST2, data = SR_reg) #Check Normality
# Create a QQ plot of residuals
ggqqplot(residuals(normality))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(normality))

# Homogeneity of variance
SR_reg %>% levene_test(risk_score ~ ASDSOVL2*alcndmor*ALCWD2SX*income*wrkdpstyr**IREDUHIGHST2, data = SR_reg)



#corlinearity
cormat <- round(cor(SR_reg),2)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()

# Two-way ANOVA Test
aov<- aov(data = SR_reg, risk_score ~ (amdelt+ASDSOVL2+CATAG6+irmarit+irsex+IREDUHIGHST2+NEWRACE2+wrkdpstyr+
                                         income+alcndmor+ALCWD2SX+inpatient+outpatient)^2)
summary(aov)
post_hoc<-TukeyHSD(aov, which = c('amdelt','ASDSOVL2','CATAG6','IREDUHIGHST2','income','alcndmor','ALCWD2SX',
                                  'irsex','NEWRACE2'))


#Plots for Interactive Effects
mde_in<-lm(data = SR_reg, risk_score ~ amdelt*inpatient)
in_out<-lm(data = SR_reg, risk_score ~ inpatient*outpatient)
mde_out<-lm(data = SR_reg, risk_score ~ amdelt*outpatient)
severity_in<-lm(data = SR_reg, risk_score ~ inpatient*ASDSOVL2 )
age_sex<-lm(data = SR_reg, risk_score ~ CATAG6*irsex)
age_edu<-lm(data = SR_reg, risk_score ~ IREDUHIGHST2*CATAG6)
mar_emp<-lm(data = SR_reg, risk_score ~ irmarit*wrkdpstyr)
mar_drink<-lm(data = SR_reg, risk_score ~ irmarit*alcndmor)
mar_in<-lm(data = SR_reg, risk_score ~ irmarit*inpatient)
sex_symp<-lm(data = SR_reg, risk_score ~ irsex*ALCWD2SX)
edu_in<-lm(data = SR_reg, risk_score ~ IREDUHIGHST2*inpatient)
edu_out<-lm(data = SR_reg, risk_score ~ IREDUHIGHST2*outpatient)

in_outP<-interact_plot(in_out, pred = inpatient , modx = outpatient)#no
mde_outP<-interact_plot(mde_out, pred = outpatient , modx = amdelt)
mde_inP<-interact_plot(mde_in, pred = inpatient , modx = amdelt)
severity_inP<-interact_plot(severity_in, pred = inpatient , modx = ASDSOVL2)
age_sexP<-cat_plot(age_sex, pred = CATAG6, modx = irsex, geom = "line") #no
age_eduP<-cat_plot(age_edu, pred = IREDUHIGHST2, modx = CATAG6,geom = "line", point.shape = TRUE)
mar_empP<-cat_plot(mar_emp, pred = irmarit, modx = wrkdpstyr, geom = "line") #no
mar_drinkP<-cat_plot(mar_drink, pred = irmarit, modx = alcndmor, geom = "line") #算
mar_inP<-interact_plot(mar_in, pred = inpatient , modx = irmarit) #算
sex_sympP<-cat_plot(sex_symp, pred = irsex, modx = ALCWD2SX, geom = "line") #no
edu_inP<-interact_plot(edu_in, pred = inpatient, modx = IREDUHIGHST2,colors="Rainbow") #yes
edu_outP<-interact_plot(edu_out, pred = outpatient, modx = IREDUHIGHST2, colors="Rainbow") #yes



## Classification
SR<-SR[c(-1:-18)]
SR<-SR[c(1:11, 13, 14, 12)] #change order of the column
SR<-na.omit(SR[c(1,3:14)]) #omit the variable with too much NA and all rows w/ NA (drop ASDSOVL2)
SR[c(1,3,4,6,7,9,10,13)] <- lapply(SR[c(1,3,4,6,7,9,10,13)], factor)

#### Training Testing set
set.seed(1)
index = createDataPartition(y=SR$risk_level, p=0.7, list=FALSE) 
sr_train =SR[index,]
sr_test = SR[-index,]

#Attempts on different algorithms---Model Training
ctrl = trainControl(method="repeatedcv", number = 10, repeats = 5)

#Naive Bayes
train_nb<-sr_train
train_nb$risk_level<-factor(train_nb$risk_level, levels = c("no","low","moderate","high"), 
                            labels = c(1,2,3,4))
ctrl = trainControl(method="repeatedcv", number = 10, repeats = 5)

nb = train(sr_train[1:12], sr_train$risk_level, method = 'nb', trControl=ctrl)
nb

#Decision Tress
ctrl_dt = trainControl(method="repeatedcv",number=10, repeats=5)

dtree = train(risk_level~., data = sr_train, method = "rpart", trControl = ctrl_dt,tuneLength = 30)
dtree
prp(dtree$finalModel)


#Random Forest
ctrl_rf = trainControl(method = "oob")
rf = caret::train(risk_level ~., data = sr_train, method = "rf", trControl = ctrl_rf,
           tuneLength = 8, ntree = 150, importance = TRUE)

#Neural Network
train_nn<-sr_train
train_nn$risk_level = factor(train_nn$risk_level, levels = c("no","low","moderate","high"), labels = c(1,2,3,4))
dummy <- dummyVars(" ~ .", data=train_nn[c(1,3,4,6,7,9,10,13)])
newdata <- data.frame(predict(dummy, newdata = train_nn[c(1,3,4,6,7,9,10,13)]))
train_nn<-cbind(train_nn[c(2,5,8,11,12)],newdata)
#train_nn[c(1,3,4,6,7,9,10,13)] <-lapply(train_nn[c(1,3,4,6,7,9,10,13)], function(x) to_categorical(as.numeric(x)))

nn <- keras_model_sequential() %>%  
  layer_flatten(input_shape = c(29)) %>%  
  layer_dense(units = 10, activation = "relu")%>%
  layer_dense(units = 6, activation = "relu")%>%
  layer_dense(units = 4, activation = "softmax")
nn%>% compile(
    loss = "categorical_crossentropy",
    optimizer = 'rmsprop',
    metrics = 'accuracy')
nn %>% fit(x = as.matrix(train_nn[,c(1:29)]), 
          y = as.matrix(train_nn[,c(30:33)]), 
          epochs = 15, validation_split = 0.3,verbose = 2)


#Testing

#pred_nb = predict(nb,sr_test)
pred_knn = predict(knn,sr_test)
pred_dtree = predict(dtree,sr_test)
pred_rf = predict(rf,sr_test)

test_nn<-sr_test
test_nn$risk_level = factor(test_nn$risk_level, levels = c("no","low","moderate","high"), labels = c(1,2,3,4))
dummy1 <- dummyVars(" ~ .", data=test_nn[c(1,3,4,6,7,9,10,13)])
newdata1 <- data.frame(predict(dummy1, newdata = test_nn[c(1,3,4,6,7,9,10,13)]))
test_nn<-cbind(test_nn[c(2,5,8,11,12)],newdata1)

pred_nn = nn %>% predict(as.matrix(test_nn[,c(1:29)]))
pred_nn1 <- data.frame(pred_nn)
pred_nn1$risk_level <- apply(pred_nn1,1,which.max)
pred_nn1$risk_level = as.factor(pred_nn1$risk_level)
pred_nn1$risk_level <- factor(pred_nn1$risk_level, levels = c(levels(pred_nn1$risk_level), "1"))

confusionMatrix(as.factor(pred_nn1$risk_level),as.factor(sr_test$risk_level), positive = '1')






write.csv(pred_nn1, file = "~/Desktop/DA 401/pred_nn1.csv")



