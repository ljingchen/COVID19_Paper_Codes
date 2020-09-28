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
library(VIM)

hospital <- read_excel("Desktop/Covid19/brazil_hospital.xlsx")
dim(hospital)   # 5644 obs & 111 cols

table(hospital$`SARS-Cov-2 exam result`)   # 5086 negative and 558 positive cases

sort(colMeans(is.na(hospital)))
# all missing are above 75%, which is hard for us to extract more info

#################
### Data cleaning
#################
# First, delete variables if 95% are missing   5644*0.95 = 5361.8
hospital <- hospital %>% rename(Regular=4,Semi=5,Intensive=6)
hospital1 <- hospital[, colSums(is.na(hospital)) < 5362]

dim(hospital1)  ## 46 cols left
#str(hospital1) 

# rename the Age variable
hospital1$Age <- hospital1$`Patient age quantile`
hospital1$`Patient age quantile`<- NULL

table(hospital1$`SARS-Cov-2 exam result`)   # 5086 negative & 558 positive
# rename the Covid-19 testing varaible, 1 = positive, 0 = negative
hospital1$Covid19.Results <- ifelse(hospital1$`SARS-Cov-2 exam result` == "positive", 1, 0)
hospital1$`SARS-Cov-2 exam result` <- NULL
#table(hospital1$Covid19.Results)   # 5086 negative & 558 positive

# subset the dataset that only has positive cases
hospital.pos <- subset(hospital1, hospital1$Covid19.Results == 1)
#dim(hospital.pos)
# subset the dataset that only has negative cases
hospital.neg <- subset(hospital1, hospital1$Covid19.Results == 0)
#dim(hospital.neg)

# rename all numeric variables. We handle these numeric variables later
hospital1$Mean.Platelet.Volume <- hospital1$`Mean platelet volume`
hospital1$`Mean platelet volume` <- NULL
hospital1$Red.Blood.Cells <- hospital1$`Red blood Cells`
hospital1$`Red blood Cells` <- NULL
hospital1$Mean.Corpuscular.Hemoglobin.Concentration <- hospital1$`Mean corpuscular hemoglobin concentration (MCHC)`
hospital1$`Mean corpuscular hemoglobin concentration (MCHC)` <- NULL
hospital1$Mean.Corpuscular.Hemoglobin <- hospital1$`Mean corpuscular hemoglobin (MCH)`
hospital1$`Mean corpuscular hemoglobin (MCH)` <- NULL
hospital1$Mean.Corpuscular.Volume <- hospital1$`Mean corpuscular volume (MCV)`
hospital1$`Mean corpuscular volume (MCV)` <- NULL
hospital1$Red.Blood.Cell.Distribution.Width <- hospital1$`Red blood cell distribution width (RDW)`
hospital1$`Red blood cell distribution width (RDW)` <- NULL
hospital1$Proteina.C <- hospital1$`Proteina C reativa mg/dL`
hospital1$`Proteina C reativa mg/dL` <- NULL


#### alternative codes (not necessary)  #######
#data <- hospital1[,-c(1:4)]
#dim(data)
#colnames(data)

#data$`Respiratory Syncytial Virus` <- ifelse(data$`Respiratory Syncytial Virus` == "detected",0 ,1)
#data$`Influenza A` <- ifelse(data$`Influenza A` == "detected",0 ,1)
#data$`Influenza B` <- ifelse(data$`Influenza B` == "detected",0 ,1)
#data$`Parainfluenza 1` <- ifelse(data$`Parainfluenza 1` == "detected",0 ,1)
#data$`CoronavirusNL63` <- ifelse(data$`CoronavirusNL63` == "detected",0 ,1)
#data$`Rhinovirus/Enterovirus` <- ifelse(data$`Rhinovirus/Enterovirus` == "detected",0 ,1)
#data$`Coronavirus HKU1` <- ifelse(data$`Coronavirus HKU1` == "detected",0 ,1)
#data$`Parainfluenza 3` <- ifelse(data$`Parainfluenza 3` == "detected",0 ,1)
#data$`Chlamydophila pneumoniae` <- ifelse(data$`Chlamydophila pneumoniae` == "detected",0 ,1)
#data$`Adenovirus` <- ifelse(data$`Adenovirus` == "detected",0 ,1)
#data$`Parainfluenza 4` <- ifelse(data$`Parainfluenza 4` == "detected",0 ,1)
#data$`Coronavirus229E` <- ifelse(data$`Coronavirus229E` == "detected",0 ,1)
#data$`CoronavirusOC43` <- ifelse(data$`CoronavirusOC43` == "detected",0 ,1)
#data$`Inf A H1N1 2009` <- ifelse(data$`Inf A H1N1 2009` == "detected",0 ,1)
#data$`Bordetella pertussis` <- ifelse(data$`Bordetella pertussis` == "detected",0 ,1)
#data$`Metapneumovirus` <- ifelse(data$`Metapneumovirus` == "detected",0 ,1)
#data$`Parainfluenza 2` <- ifelse(data$`Parainfluenza 2` == "detected",0 ,1)
#data$`Influenza B, rapid test` <- ifelse(data$`Influenza B, rapid test` == "negative",0 ,1)  # positive/negative
#data$`Influenza A, rapid test` <- ifelse(data$`Influenza A, rapid test` == "negative",0 ,1)  # positive/negative
#data$`Strepto A` <- ifelse(data$`Strepto A` == "positive",1 ,0)    # negative 297, positive 34, 1 not_done


### categorical varaibles ###
### below three variables are useless
hospital1$`Patient addmited to regular ward (1=yes, 0=no)` <- NULL
hospital1$`Patient addmited to semi-intensive unit (1=yes, 0=no)` <- NULL
hospital1$`Patient addmited to intensive care unit (1=yes, 0=no)` <- NULL
hospital1$`Patient ID` <- NULL

# Approach 1
### below these have same missing percent 0.7604536 
table(hospital1$`Respiratory Syncytial Virus`)   #  52 detected, 1302 not_detected 
table(hospital.pos$`Respiratory Syncytial Virus`) # all not_detected, drop this variable
table(hospital.neg$`Respiratory Syncytial Virus`) # all not_detected, drop this variable
hospital1$`Respiratory Syncytial Virus` <- NULL

table(hospital1$`Influenza A`)   #  18 detected, 1336 not_detected 
table(hospital.pos$`Influenza A`) # all not_detected, drop this variable
table(hospital.neg$`Influenza A`) # all not_detected, drop this variable
hospital1$`Influenza A` <- NULL

table(hospital1$`Influenza A, rapid test`)   #  768 negative, 52 positive 
table(hospital.pos$`Influenza A, rapid test`) # all negative, drop this variable
table(hospital.neg$`Influenza A, rapid test`) # all not_detected, drop this variable
hospital1$`Influenza A, rapid test` <- NULL

table(hospital1$`Parainfluenza 1`)   #  3 detected, 1349 not_detected 
table(hospital.pos$`Parainfluenza 1`) 
table(hospital.neg$`Parainfluenza 1`) 
hospital1$`Parainfluenza 1` <- NULL

table(hospital1$`Parainfluenza 2`)   #  1352 not_detected, drop this variable
table(hospital.pos$`Parainfluenza 2`) 
table(hospital.neg$`Parainfluenza 2`) 
hospital1$`Parainfluenza 2` <- NULL

table(hospital1$`Parainfluenza 3`)   #  10 detected, 1342 not_detected 
table(hospital.pos$`Parainfluenza 3`) # all not_detected, drop this variable
table(hospital.neg$`Parainfluenza 3`) 
hospital1$`Parainfluenza 3` <- NULL

table(hospital1$`Parainfluenza 4`)   #  19 detected, 1333 not_detected 
table(hospital.pos$`Parainfluenza 4`) # all not_detected, drop this variable
table(hospital.neg$`Parainfluenza 4`)
hospital1$`Parainfluenza 4` <- NULL

table(hospital1$CoronavirusNL63)   #  45 detected, 1307 not_detected 
table(hospital.pos$CoronavirusNL63) # 3 detected, 109 not detected
table(hospital.neg$CoronavirusNL63) # 42 detected, 1198 not detected

table(hospital1$`Coronavirus HKU1`)   #  20 detected, 1332 not_detected 
table(hospital.pos$`Coronavirus HKU1`) # all not_detected, drop this variable
table(hospital.neg$`Coronavirus HKU1`)
hospital1$`Coronavirus HKU1` <- NULL

table(hospital1$`Chlamydophila pneumoniae`)   #  9 detected, 1343 not_detected 
table(hospital.pos$`Chlamydophila pneumoniae`) # all not_detected, drop this variable
table(hospital.neg$`Chlamydophila pneumoniae`) 
hospital1$`Chlamydophila pneumoniae` <- NULL

table(hospital1$Adenovirus)   #  13 detected, 1339 not_detected 
table(hospital.pos$Adenovirus) # all not_detected, drop this variable
table(hospital.neg$Adenovirus) 
hospital1$Adenovirus <- NULL

table(hospital1$CoronavirusOC43)   #  8 detected, 1344 not_detected 
table(hospital.pos$CoronavirusOC43) # all not_detected, drop this variable
table(hospital.neg$CoronavirusOC43) 
hospital1$CoronavirusOC43 <- NULL

table(hospital1$`Inf A H1N1 2009`)   #  98 detected, 1254 not_detected 
table(hospital.pos$`Inf A H1N1 2009`) # all not_detected, drop this variable
table(hospital.neg$`Inf A H1N1 2009`) 
hospital1$`Inf A H1N1 2009` <- NULL

table(hospital1$`Bordetella pertussis`)   #  2 detected, 1350 not_detected 
table(hospital.pos$`Bordetella pertussis`) # all not_detected, drop this variable
table(hospital.neg$`Bordetella pertussis`) 
hospital1$`Bordetella pertussis` <- NULL

table(hospital1$Metapneumovirus)   #  14 detected, 1338 not_detected 
table(hospital.pos$Metapneumovirus) # all not_detected, drop this variable
table(hospital.neg$Metapneumovirus) 
hospital1$Metapneumovirus <- NULL

# check the following four varaibles, also delete since they are not the main factors for the Covid-19 testing results
table(hospital1$Coronavirus229E)   #  9 detected, 1343 not_detected 
table(hospital.pos$Coronavirus229E) # 1 detected, 111 not_detected
table(hospital.neg$Coronavirus229E) 

table(hospital1$`Rhinovirus/Enterovirus`)   #  379 detected, 973 not_detected 
table(hospital.pos$`Rhinovirus/Enterovirus`) # 6 detected, 106 not detected
table(hospital.neg$`Rhinovirus/Enterovirus`) # 373 detected, 867 not detected

table(hospital1$`Influenza B`)   #  77 detected, 1277 not_detected 
table(hospital.pos$`Influenza B`) # 3 detected, 109 not_detected
table(hospital.neg$`Influenza B`) 

table(hospital1$`Influenza B, rapid test`)   #  49 positive, 771 negative 
table(hospital.pos$`Influenza B, rapid test`) # 2 positive, 60 negative
table(hospital.neg$`Influenza B, rapid test`)


### Strepto A
table(hospital1$`Strepto A`)   #  34 positive, 287 negative, 1 not_done
table(hospital.pos$`Strepto A`) # all negative, drop this variable
table(hospital.neg$`Strepto A`)
hospital1$`Strepto A` <- NULL  # then, no other factor predictors

####
# Approach 2: create a new variable “other_disease_check” to describe whether a patient has either one of these 5 diseases. 
hospital1$other_disease_check <- ifelse(hospital1$CoronavirusNL63 == "detected" | hospital1$Coronavirus229E == "detected" | hospital1$`Rhinovirus/Enterovirus` == "detected"
                       | hospital1$`Influenza B` == "detected" | hospital1$`Influenza B, rapid test` == "detected", 1, 0)

# then delete these 5 variables
hospital1$CoronavirusNL63 <- NULL
hospital1$Coronavirus229E <- NULL
hospital1$`Rhinovirus/Enterovirus` <- NULL
hospital1$`Influenza B` <- NULL
hospital1$`Influenza B, rapid test`<- NULL

hospital1$Regular <- NULL
hospital1$Semi <- NULL
hospital1$Intensive <- NULL

###
## Handling Numeric variables
###
# Correlation
hosCorr <- cor(hospital1,use="pairwise.complete.obs")

##Visualize correlations
corrplot(hosCorr, order = "alphabet",tl.cex = 0.6, method="pie", type = "upper")
corrplot(hosCorr, order = "alphabet",tl.cex = 0.6, method="number", type = "upper",number.cex=0.35)

# correlation tests
cor.test(hospital1$Monocytes,hospital1$Covid19.Results, 
                method = "pearson")
cor.test(hospital1$Platelets,hospital1$Covid19.Results, 
         method = "pearson")
cor.test(hospital1$Leukocytes,hospital1$Covid19.Results, 
         method = "pearson")
cor.test(hospital1$Eosinophils,hospital1$Covid19.Results, 
         method = "pearson")
cor.test(hospital1$other_disease_check,hospital1$Covid19.Results, 
         method = "pearson")


##Identify and remove highly correlated predictors
highCorr <- findCorrelation(hosCorr, 0.5)
hospital2 <- hospital1[,-highCorr]

######################################
## Directly move to second attempt !!
## First attempt: Previous coded - we don't use it NOW!
######################################
impute_arg <- aregImpute(~Platelets+Lymphocytes+Leukocytes+Basophils+Eosinophils+Monocytes+Urea+Potassium+Sodium+Mean.Platelet.Volume+
                           Red.Blood.Cells+Mean.Corpuscular.Hemoglobin.Concentration+Mean.Corpuscular.Volume+Red.Blood.Cell.Distribution.Width+
                           Proteina.C,data = hospital2,n.impute = 5)

imputed <-impute.transcan(impute_arg, data=hospital2, imputation=2, list.out=TRUE, pr=FALSE, check=FALSE)
imputed.data <- as.data.frame(do.call(cbind,imputed))

Age <- hospital2$Age
Covid19.Results <- as.factor(hospital2$Covid19.Results)

hospital3 <- cbind(Age,Covid19.Results,imputed.data)  # final clean data
dim(hospital3)  # 5644 obs and 17 cols

sort(colMeans(is.na(hospital3)))

## other imputation method 1: still use Hmisc, but just impute data using median
set.seed(900)
hos.mis <- prodNA(hospital2[,-c(14,15,23)], noNA = 0.05)
summary(hos.mis)
hos.mis$Hematocrit <- with(hos.mis, impute(Hematocrit, median))
hos.mis$Hemoglobin <- with(hos.mis, impute(Hemoglobin, median))
hos.mis$Platelets <- with(hos.mis, impute(Platelets, median))
hos.mis$Lymphocytes <- with(hos.mis, impute(Lymphocytes, median))
hos.mis$Leukocytes <- with(hos.mis, impute(Leukocytes, median))
hos.mis$Basophils <- with(hos.mis, impute(Basophils, median))
hos.mis$Eosinophils <- with(hos.mis, impute(Eosinophils, median))
hos.mis$Monocytes <- with(hos.mis, impute(Monocytes, median))
hos.mis$Neutrophils <- with(hos.mis, impute(Neutrophils, median))
hos.mis$Urea <- with(hos.mis, impute(Urea, median))
hos.mis$Creatinine <- with(hos.mis, impute(Creatinine, median))
hos.mis$Potassium <- with(hos.mis, impute(Potassium, median))
hos.mis$Sodium <- with(hos.mis, impute(Sodium, median))
hos.mis$Mean.Platelet.Volume <-with(hos.mis, impute(Mean.Platelet.Volume, median))
hos.mis$Red.Blood.Cells <- with(hos.mis, impute(Red.Blood.Cells, median))
hos.mis$Mean.Corpuscular.Hemoglobin.Concentration <- with(hos.mis, impute(Mean.Corpuscular.Hemoglobin.Concentration, median))
hos.mis$Mean.Corpuscular.Hemoglobin <- with(hos.mis, impute(Mean.Corpuscular.Hemoglobin, median))
hos.mis$Mean.Corpuscular.Volume <- with(hos.mis, impute(Mean.Corpuscular.Volume, median))
hos.mis$Red.Blood.Cell.Distribution.Width <- with(hos.mis, impute(Red.Blood.Cell.Distribution.Width, median))
hos.mis$Proteina.C <- with(hos.mis, impute(Proteina.C, median))

# other imputation method 2: use hotdeck() in VIM package to impute missing values. the final dataset with no missing values
#hospital3 <- hotdeck(hospital2)[,(1:23)]  
#dim(hospital3)  # 650 obs and 23 cols

#################
##### SMOTE #####
#################
set.seed(456)
smote_hospital <- SMOTE(Covid19.Results ~ ., data  = hospital3)    
dim(smote_hospital)  # 3906 obs and 17 cols
table(smote_hospital$Covid19.Results)   # 2232 negative  1674 positive

#################
### Data spliting
##################
# 3100 rows for training (79.4%), 806 rows for testing
set.seed(456)
train <- sample(1:nrow(smote_hospital), 3100)
hospital.train <- smote_hospital[train,]
hospital.test <- smote_hospital[-train,]

hospital.train_X <- hospital.train[,c(1,3:17)]
hospital.train_Y <- hospital.train[,2]
hospital.train_Y <- factor(ifelse(hospital.train_Y == 1, "Positive", "Negative"))
hospital.test_X <- hospital.test[,c(1,3:17)]
hospital.test_Y <- hospital.test[,2]
hospital.test_Y <- factor(ifelse(hospital.test_Y == 1, "Positive", "Negative"))

#########################
## Logistic Regression ##
#########################
glm.fits <- glm(Covid19.Results ~ ., data=hospital.train,
                family=binomial)
summary(glm.fits)

glm.probs <- predict(glm.fits, hospital.test, type="response")
glm.pred <- rep("Negative", 806)
glm.pred[glm.probs > 0.5] <- "Positive"

table(glm.pred, hospital.test_Y)
mean(glm.pred==hospital.test_Y)   # The test accuracy is 0.5794

##Re-specify model using the strongest predictors
glm.fits1 <- glm(Covid19.Results ~ Age + Leukocytes + Eosinophils + Monocytes + Platelets + Red.Blood.Cells, data=hospital.train, family=binomial)
glm.probs1 <- predict(glm.fits1, hospital.test, type="response")
summary(glm.fits1)

glm.pred1 <- rep("Negative", 806)
glm.pred1[glm.probs1 > 0.5] <- "Positive"

table(glm.pred1, hospital.test_Y)
mean(glm.pred1==hospital.test_Y)  #  0.5694789, not good as well

###################
## Random Forest ##
###################

# Method 1
set.seed(800)
rf.cov<-randomForest(hospital.train$Covid19.Results~., hospital.train[,c(-2)])
pred.rf<-predict(rf.cov, newdata=hospital.test)
pred.rf1 <- as.factor(ifelse(pred.rf==0, "Negative","Positive"))

table(pred.rf1,hospital.test_Y)
mean(pred.rf1==hospital.test_Y)   # 0.8759305
importance(rf.cov)
varImpPlot(rf.cov)

######
# Method 2 using caret package

ctrl <- trainControl(method = "repeatedcv",
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- c(2, 4, 6, 8, 10)

set.seed(400)
rfFit <- train(x = hospital.train_X, 
               y = hospital.train_Y,
               method = "rf",
               ntree = 500,
               tuneGrid = data.frame(mtry = mtryValues),
               importance = TRUE,
               metric = "Kappa",
               trControl = ctrl)
rfFit

##Fitted values
pred.rf <- predict(rfFit)
##Check fitted accuracy
rf.train <- confusionMatrix(data=pred.rf, reference=hospital.train_Y)

##Check test accuracy
pred.rf.test <- predict(rfFit, hospital.test_X)
rf.test <- confusionMatrix(data=pred.rf.test, 
                           reference=hospital.test_Y)

rf.test
importance <- varImp(rfFit,scale=FALSE)
plot(importance, top = 10)

rfRoc <- roc(response = rfFit$pred$obs, predictor = rfFit$pred$Negative)

plot(rfRoc, type = "s", print.thres = c(.5),
     print.thres.pch = 3,
     print.thres.pattern = "",
     print.thres.cex = 1.2,
     col = "blue", legacy.axes = TRUE,
     print.thres.col = "blue")


##########
## SVM ###
##########
##
library(kernlab)
cov.svm <- ksvm(Covid19.Results ~. , data = hospital.train)
cov.svm.pred.prob <- predict(cov.svm, hospital.test, type = "decision")
cov.svm.pred <- predict(cov.svm, hospital.test, type = "response")

table.svm <- table(cov.svm.pred, hospital.test$Covid19.Results)
table.svm

accuracy.svm <- (table.svm[1,1]+table.svm[2,2])/sum(table.svm)
accuracy.svm   # 0.7034739

## Conclusion, the above analysis seems that random forest is really good, which has a test accuracy 0.876. However, this result is 
# somewhat that we really don't want since Platelets and Leucocytes are not significant from feature importance.
##
# check current correlations
hospital.train$Covid19.Results <- as.numeric(hospital.train$Covid19.Results)
cor(hospital.train)
hospital.train$Covid19.Results <- as.factor(hospital.train$Covid19.Results)
# 

######################################
####### Second Attempt ###############
#########   New Codes ################
######################################
######################################
## we don't need all rows
hospital4 <- hospital2
hospital4$other_disease_check <- NULL # first delete other_disease_check variable

## Remove rows with more than 50% NA
hospital5 <- hospital4[which(rowMeans(!is.na(hospital4)) > 0.5), ]
dim(hospital5)  # 602 obs and 17 cols

sort(colMeans(is.na(hospital5)))
# we find that Platelets, Leukocytes, Eosinophils don't have missing values, which is excellent!

## impute the remaining missing values, use median
# check how many missing left
NAcol1 <- which(colSums(is.na(hospital5)) > 0)
missing1 <- sort(colSums(sapply(hospital5[NAcol1], is.na)), decreasing = TRUE)

# we impute these six variables based on their medians respectively.
set.seed(900)
hos.mis <- prodNA(hospital5[,c(6,7,8,9,12,17)], noNA = 0.05)
summary(hos.mis)
hos.mis$Sodium <- with(hos.mis, impute(Sodium, median))
hos.mis$Potassium <- with(hos.mis, impute(Potassium, median))
hos.mis$Urea <- with(hos.mis, impute(Urea, median))
hos.mis$Proteina.C <- with(hos.mis, impute(Proteina.C, median))
hos.mis$Mean.Platelet.Volume <-with(hos.mis, impute(Mean.Platelet.Volume, median))
hos.mis$Monocytes  <- with(hos.mis, impute(Monocytes, median))

hospital5.new <- cbind(hospital5[,-c(6,7,8,9,12,17)], hos.mis)

# check outliers 
summary(hospital5.new)

# remove outliers
outliers <- c(which(hospital5.new$platelets > 9),
               which(hospital5.new$basophils > 4),
               which(hospital5.new$eosinophils > 5),
               which(hospital5.new$Urea > 4),
               which(hospital5.new$red_blood_cell_distribution_width_rdw > 6),
               which(hospital5.new$Proteina.C > 4))

hospital6 <- hospital5.new%>% slice(-outliers)

dim(hospital6)   # our final new clean data: 593 rows, 17 columns
# check: sort(colMeans(is.na(hospital6)))

### then, we redo the previous data analysis
# Coorelation
hosCorr1 <- cor(hospital6,use="pairwise.complete.obs")

##Visualize correlations
corrplot(hosCorr1, order = "alphabet",tl.cex = 0.35, method="pie", type = "upper")


#################
##### SMOTE #####
#################
hospital7 <- hospital6

hospital7$Covid19.Results <- as.factor(hospital7$Covid19.Results) # change to factor

set.seed(456)
smote_hospital1 <- SMOTE(Covid19.Results ~ ., data  = hospital7)    
dim(smote_hospital1)  # 581 obs and 17 cols
table(smote_hospital1$Covid19.Results)   # 332 negative  249 positive

#################
### Data spliting
##################
# 464 rows for training (79.9%), 117 rows for testing
set.seed(456)
train <- sample(1:nrow(smote_hospital1), 464)
hospital.train <- smote_hospital1[train,]
hospital.test <- smote_hospital1[-train,]

hospital.train_X <- hospital.train[,c(1:6,8:17)]
hospital.train_Y <- hospital.train[,7]
hospital.train_Y <- factor(ifelse(hospital.train_Y == 1, "Positive", "Negative"))
hospital.test_X <- hospital.test[,c(1:6,8:17)]
hospital.test_Y <- hospital.test[,7]
hospital.test_Y <- factor(ifelse(hospital.test_Y == 1, "Positive", "Negative"))

#########################
## Logistic Regression ##
#########################
glm.fits <- glm(Covid19.Results ~ ., data=hospital.train,
                family=binomial)
summary(glm.fits)

glm.probs <- predict(glm.fits, hospital.test, type="response")
glm.pred <- rep("Negative", 117)
glm.pred[glm.probs > 0.5] <- "Positive"

table(glm.pred, hospital.test_Y)
mean(glm.pred==hospital.test_Y)   # The test accuracy is 0.8376068

##Re-specify model using the strongest predictors
glm.fits1 <- glm(Covid19.Results ~ Age + Leukocytes + Eosinophils + Monocytes + Platelets + Red.Blood.Cells, data=hospital.train, family=binomial)
glm.probs1 <- predict(glm.fits1, hospital.test, type="response")
summary(glm.fits1)

glm.pred1 <- rep("Negative", 117)
glm.pred1[glm.probs1 > 0.5] <- "Positive"

table(glm.pred1, hospital.test_Y)
mean(glm.pred1==hospital.test_Y)  #  0.8547009


###################
## Random Forest ##
###################

# Method 1
set.seed(800)
rf.cov <- randomForest(hospital.train$Covid19.Results~., hospital.train[,c(-7)])
pred.rf <- predict(rf.cov, newdata=hospital.test)
pred.rf1 <- as.factor(ifelse(pred.rf==1, "Positive","Negative"))

table(pred.rf1,hospital.test_Y)
mean(pred.rf1==hospital.test_Y)   # 0.9487179
importance(rf.cov)
varImpPlot(rf.cov)


######
# Method 2 using caret package

ctrl <- trainControl(method = "repeatedcv",
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- c(2, 4, 6, 8, 10)

set.seed(400)
rfFit <- train(x = hospital.train_X, 
               y = hospital.train_Y,
               method = "rf",
               ntree = 500,
               tuneGrid = data.frame(mtry = mtryValues),
               importance = TRUE,
               metric = "Kappa",
               trControl = ctrl)
rfFit

##Fitted values
pred.rf <- predict(rfFit)
##Check fitted accuracy
rf.train <- confusionMatrix(data=pred.rf, reference=hospital.train_Y)

##Check test accuracy
pred.rf.test <- predict(rfFit, hospital.test_X)
rf.test <- confusionMatrix(data=pred.rf.test, 
                           reference=hospital.test_Y)

rf.test  # 0.9487
importance <- varImp(rfFit,scale=FALSE)
plot(importance, top = 10, main="Variable Importance - Top 10")

rfRoc <- roc(response = rfFit$pred$obs, predictor = rfFit$pred$Negative)

plot(rfRoc, type = "s", print.thres = c(.5),
     print.thres.pch = 3,
     print.thres.pattern = "",
     print.thres.cex = 1.2,
     col = "blue", legacy.axes = TRUE,
     print.thres.col = "blue")


##########
## SVM ###
##########
##
library(kernlab)
cov.svm <- ksvm(Covid19.Results ~. , data = hospital.train)
cov.svm.pred.prob <- predict(cov.svm, hospital.test, type = "decision")
cov.svm.pred <- predict(cov.svm, hospital.test, type = "response")

table.svm <- table(cov.svm.pred, hospital.test$Covid19.Results)
table.svm

accuracy.svm <- (table.svm[1,1]+table.svm[2,2])/sum(table.svm)
accuracy.svm   # 0.9230769


# check current correlations
smote_hospital1$Covid19.Results <- as.numeric(smote_hospital1$Covid19.Results)
new_corr <- cor(smote_hospital1)
smote_hospital1$Covid19.Results <- as.factor(smote_hospital1$Covid19.Results)

corrplot(new_corr, order = "alphabet",tl.cex = 0.35, tl.srt=45,method="pie", type = "upper")
corrplot(new_corr, order = "alphabet",tl.cex = 0.2, tl.srt=45, method="number", type = "upper")


##################
## Xgb Boosters
##################
# tree booster
trnCtrl2 <- trainControl(method="cv", number=5)
##Fit the xgboost model - use only 3 tuning parameter values each
##This will take 5-10 minutes

set.seed(1234)
my.train.xgb <- train(x=hospital.train_X, y=hospital.train_Y, 
                      method="xgbTree", trControl=trnCtrl2, tuneLength=3)


## Tree Boosters: Results
my.train.xgb$results
my.train.xgb$bestTune
treeboost.pred <- predict(my.train.xgb, newdata=hospital.test_X)

# accuracy
treeboost <- table(treeboost.pred, hospital.test_Y)
treeboost

treeboost.acc <- (treeboost[1,1]+treeboost[2,2])/sum(treeboost)
treeboost.acc   # 0.9059829

# variables importance
treeboost.imp.caret <- varImp(my.train.xgb, scale=FALSE)
plot(treeboost.imp.caret)

################################################################
### SHAP values
library(randomForestExplainer)
library("SHAPforxgboost")

#min_depth_frame <- min_depth_distribution(rf.cov)
#importance_frame <- measure_importance(rf.cov)
#head(min_depth_frame, n = 10)
#plot_min_depth_distribution(min_depth_frame)
#plot_importance_ggpairs(importance_frame)

shap <- hospital.train
shap$Monocytes <- as.numeric(shap$Monocytes)
shap$Urea <- as.numeric(shap$Urea)
shap$Potassium <- as.numeric(shap$Potassium)
shap$Sodium <- as.numeric(shap$Sodium)
shap$Mean.Platelet.Volume <- as.numeric(shap$Mean.Platelet.Volume)
shap$Proteina.C <- as.numeric(shap$Proteina.C)

X1 = as.matrix(shap[,-7])

## shap in xgboost
mod1 = xgboost::xgboost(
  data = X1, label = shap$Covid19.Results, gamma = 0, eta = 1,
  lambda = 0, nrounds = 1, verbose = FALSE)

shap_values <- shap.values(xgb_model = mod1, X_train = X1)
shap_values$mean_shap_score
shap_values_covid <- shap_values$shap_score

shap.plot.summary.wrap1(mod1, X1, top_n = 10)
shap.plot.summary.wrap2(shap_score = shap_values$shap_score, X1, top_n = 10)

shap_long_covid <- shap.prep(shap_contrib = shap_values_covid, X_train = X1)
shap.plot.summary(shap_long_covid)

