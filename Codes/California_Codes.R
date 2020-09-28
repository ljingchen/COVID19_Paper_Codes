library(ggplot2)
library(ggpubr)
library(dplyr)
library(caret)
library(DMwR)
library(glmnet)
library(e1071)
library("readxl")
library(corrplot)
library(missForest)
library(Hmisc)
library(DMwR)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(partykit)
library(caret)
library(pROC)
library(tidyverse)


california <- read.csv("Desktop/Covid19/California_Data.csv")

dim(california)   # 11169 obs and 46 cols
# View(california)
# str(california)

# check how many missing values
NAcol <- which(colSums(is.na(california)) > 0)
missing <- sort(colSums(sapply(california[NAcol], is.na)), decreasing = TRUE)

# colnames(california)
## eliminate useless features
california1 <- california[,-c(1, 42:45)]
dim(california1)   # 11169 obs and 41 cols

# check the Covid-19 testing results
table(california1$covid19_test_results)    # 10854 negative, 315 positive
prop.table(table(california1$covid19_test_results))   # 0.9718% negative, 0.0282% negative

# if we only check 315 positive cases
# clinical.pos only contains positive cases
clinical.pos <- subset(california1, covid19_test_results=="Positive")
dim(clinical.pos) 
# clinical.neg only contain negative cases
clinical.neg <- subset(california1, covid19_test_results=="Negative")
dim(clinical.neg) 
# View(clinical.pos)

table(clinical.pos$high_risk_exposure_occupation)
table(clinical.pos$high_risk_interactions)
table(clinical.pos$diabetes)
table(clinical.pos$chd)
table(clinical.pos$cancer)
table(clinical.pos$asthma)
table(clinical.pos$htn)
table(clinical.pos$copd)
table(clinical.pos$smoker)
table(clinical.pos$autoimmune_dis)
table(clinical.pos$ctab)
table(clinical.pos$labored_respiration)
table(clinical.pos$rhonchi)
table(clinical.pos$wheezes)
table(clinical.pos$days_since_symptom_onset)
table(clinical.pos$cough)
table(clinical.neg$cough)
table(clinical.pos$cough_severity)
table(clinical.pos$fever)
table(clinical.pos$sob)
table(clinical.pos$sob_severity)
table(clinical.pos$diarrhea)
table(clinical.pos$fatigue)
table(clinical.pos$headache)
table(clinical.pos$loss_of_smell)
table(clinical.pos$loss_of_taste)
table(clinical.pos$runny_nose)
table(clinical.pos$muscle_sore)
table(clinical.pos$sore_throat)
table(clinical.pos$er_referral)



table(clinical.neg$high_risk_exposure_occupation)
table(clinical.neg$high_risk_interactions)
table(clinical.neg$diabetes)
table(clinical.neg$chd)
table(clinical.neg$htn)
table(clinical.neg$cancer)
table(clinical.neg$asthma)
table(clinical.neg$copd)
table(clinical.neg$smoker)
table(clinical.neg$autoimmune_dis)
table(clinical.neg$ctab)
table(clinical.neg$labored_respiration)
table(clinical.neg$rhonchi)
table(clinical.neg$wheezes)
table(clinical.neg$days_since_symptom_onset)
table(clinical.neg$cough)
table(clinical.neg$cough_severity)
table(clinical.neg$fever)
table(clinical.neg$sob)
table(clinical.neg$sob_severity)
table(clinical.neg$diarrhea)
table(clinical.neg$fatigue)
table(clinical.neg$headache)
table(clinical.neg$loss_of_smell)
table(clinical.neg$loss_of_taste)
table(clinical.neg$runny_nose)
table(clinical.neg$muscle_sore)
table(clinical.neg$sore_throat)
table(clinical.neg$er_referral)


tapply(california1$age, california1$covid19_test_results, summary)
tapply(california1$temperature, california1$covid19_test_results, summary)
tapply(california1$pulse, california1$covid19_test_results, summary)
tapply(california1$sys, california1$covid19_test_results, summary)
tapply(california1$dia, california1$covid19_test_results, summary)
tapply(california1$rr, california1$covid19_test_results, summary)
tapply(california1$sats, california1$covid19_test_results, summary)


##### t.test
### numerical
numerical <- subset(california1, select = c("temperature","covid19_test_results","age","pulse","sys","dia","rr","sats"))
# remove missing values
numerical_new <- na.omit(numerical)  # 6420

#numerical_pos <- subset(numerical_new,numerical_new$covid19_test_results == "Positive")
#numerical_neg <- subset(numerical_new,numerical_new$covid19_test_results == "Negative")

t.test(temperature ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "less",conf.level = 0.95, data=numerical_new)

t.test(age ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "greater",conf.level = 0.95, data=numerical_new)


t.test(pulse ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "less",conf.level = 0.95, data=numerical_new)

t.test(sys ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "less",conf.level = 0.95, data=numerical_new)

t.test(dia ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "less",conf.level = 0.95, data=numerical_new)

t.test(rr ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "less",conf.level = 0.95, data=numerical_new)

t.test(sats ~ relevel(as.factor(covid19_test_results),ref="Negative"), 
       alternative = "greater",conf.level = 0.95, data=numerical_new)


##### two proportion z-test
### categorical

categorical <- subset(california1, select = c("diabetes","chd","htn","cancer","asthma","copd","autoimmune_dis"
                                             ,"smoker","covid19_test_results"))
# remove missing values
categorical_new <- na.omit(categorical)  # 6420
dim(categorical_new)  # 11169

categoricalpos <- subset(categorical_new, categorical_new$covid19_test_results == "Positive")
dim(categoricalpos)  # 315
categoricalneg <- subset(categorical_new, categorical_new$covid19_test_results == "Negative")
dim(categoricalneg)  # 10854


prop.test(x = c(10, 235), n = c(315, 10854),alternative = "greater")
# 11169
table(categorical_new$diabetes)   # 10924 false 245 true
table(categoricalpos$diabetes)   # 305 false 10 true
table(categoricalneg$diabetes)   # 10619 false 235 true

table(categorical_new$chd)      # 11006 false 163 true
table(categoricalpos$chd)   # 313 false 2 true
table(categoricalneg$chd)   # 10693 false 161 true
prop.test(x = c(2, 161), n = c(315, 10854),alternative = "less")


table(categorical_new$htn)       # 10195 false 974 true
table(categoricalpos$htn)   # 289 false 26 true
table(categoricalneg$htn)   # 9906 false 948 true
prop.test(x = c(26, 948), n = c(315, 10854),alternative = "less")


table(categorical_new$cancer)     # 11079 false 90 true
table(categoricalpos$cancer)   # 313 false 2 true
table(categoricalneg$cancer)   # 10766 false 88 true
prop.test(x = c(2, 88), n = c(315, 10854),alternative = "less")


table(categorical_new$asthma)     # 10513 false 656 true
table(categoricalpos$asthma)   # 306 false 9 true
table(categoricalneg$asthma)   # 10207 false 647 true
prop.test(x = c(9, 647), n = c(315, 10854),alternative = "less")

table(categorical_new$copd)    # 11139 false 30 true
table(categoricalpos$copd)   # 315 false
table(categoricalneg$copd)   # 10824 false 30 true
prop.test(x = c(0, 30), n = c(315, 10854),alternative = "less")


table(categorical_new$autoimmune_dis)    # 11092 false 77 true
table(categoricalpos$autoimmune_dis)   # 314 false 1 true
table(categoricalneg$autoimmune_dis)   # 10778 false 76 true
prop.test(x = c(1, 76), n = c(315, 10854),alternative = "less")


table(categorical_new$smoker)    # 10392 false 777 true
table(categoricalpos$smoker)   # 306 false 9 true
table(categoricalneg$smoker)   # 10086 false 768 true

prop.test(x = c(9, 768), n = c(315, 10854),alternative = "less")



















