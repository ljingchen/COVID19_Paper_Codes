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

mexico <- read.csv("Desktop/Covid19/Mexico_Data.csv")
#mexico <- read.csv("Mexico_Data.csv")

dim(mexico) ## 1565452 obs and 35 cols
str(mexico)
colnames(mexico)

# first delete 8 variables that are useless. These variables describes the 
mexico1 <- mexico[,-c(1:5,7,8,9)]
dim(mexico1)

## Spanish data, transfer to English
mexico1$Sex <- mexico1$SEXO   # factor
mexico1$Care_type <- mexico1$TIPO_PACIENTE    # factor
mexico1$Care.date <- mexico1$FECHA_INGRESO  # date
mexico1$Start.date <- mexico1$FECHA_SINTOMAS  # date
mexico1$Death_date <- mexico1$FECHA_DEF    # date
mexico1$Intubation <- mexico1$INTUBADO   # factor
mexico1$Pneumonia <- mexico1$NEUMONIA   # factor
mexico1$Age <- mexico1$EDAD   # numeric
mexico1$Mex.fore <- mexico1$NACIONALIDAD  #  factor
mexico1$Pregnancy <- mexico1$EMBARAZO  # factor
mexico1$Hindi <- mexico1$HABLA_LENGUA_INDIG    # factor
mexico1$Diabetes <- mexico1$DIABETES   # factor
mexico1$COPD <- mexico1$EPOC   # factor
mexico1$Asthma <- mexico1$ASMA   # factor
mexico1$Immunosuppression <- mexico1$INMUSUPR  # factor
mexico1$Hypertension <- mexico1$HIPERTENSION  # factor
mexico1$Other_disease <- mexico1$OTRA_COM   # factor
mexico1$Cardiovascular <- mexico1$CARDIOVASCULAR   # factor
mexico1$Obesity <- mexico1$OBESIDAD  # factor
mexico1$CKD <- mexico1$RENAL_CRONICA  # factor
mexico1$Smoke <- mexico1$TABAQUISMO   # factor
mexico1$Other.cov <- mexico1$OTRO_CASO  # factor
mexico1$Results <- mexico1$RESULTADO  # factor
mexico1$Immigration <- mexico1$MIGRANTE  # factor
mexico1$Nation <- mexico1$PAIS_NACIONALIDAD  # factor
mexico1$Nation.left  <- mexico1$PAIS_ORIGEN  # factor
mexico1$ICU <- mexico1$UCI  # factor

mexico2 <- mexico1[,c(28:54)]

colnames(mexico2)
View(mexico2)

sort(colMeans(is.na(mexico2)))   
# seem to have no missing values, but some values mean "Not Applicable or Unknown"
#sapply(mexico2, function(x) sum(is.na(x)))

# rename all 27 variables
# 1. 
table(mexico2$Sex)
mexico2$Sex <- ifelse(mexico2$Sex == "HOMBRE", "Male", "Female")
table(mexico2$Sex)

# 2. 
table(mexico2$Care_type)
mexico2$Care_type <- ifelse(mexico2$Care_type == "AMBULATORIO", "Outpatient", "Inpatient")
table(mexico2$Care_type)

# 3,4, 5  date var, don't need change
mexico1$Care.date
mexico1$Start.date
mexico1$Death_date

# 6.
table(mexico2$Intubation)
mexico2$Intubation <- ifelse(mexico2$Intubation == "NO APLICA" | mexico2$Intubation == "NO ESPECIFICADO" , "Unknow", 
                             ifelse(mexico2$Intubation == "SI", "Yes", "No"))
table(mexico2$Intubation)

# 7.
table(mexico2$Pneumonia)
mexico2$Pneumonia <- ifelse(mexico2$Pneumonia == "NO ESPECIFICADO", "Unknow",
                            ifelse(mexico2$Pneumonia == "SI","Yes", "No"))
table(mexico2$Pneumonia)

# 8. Age: numeric var, don't need change

# 9.  
table(mexico2$Mex.fore)
mexico2$Mex.fore <- ifelse(mexico2$Mex.fore == "EXTRANJERA", "Foreign", "Mexican")

table(mexico2$Mex.fore)

# 10.
table(mexico2$Pregnancy)
mexico2$Pregnancy <- ifelse(mexico2$Pregnancy == "NO APLICA" | mexico2$Pregnancy == "SE IGNORA" , "Unknow", 
                            ifelse(mexico2$Pregnancy == "SI", "Yes", "No"))
table(mexico2$Pregnancy)

# 11.
table(mexico2$Hindi)
mexico2$Hindi <- ifelse(mexico2$Hindi == "NO ESPECIFICADO", "Unknow",
                            ifelse(mexico2$Hindi == "SI","Yes", "No"))
table(mexico2$Hindi)

# 12.
table(mexico2$Diabetes)
mexico2$Diabetes <- ifelse(mexico2$Diabetes == "NO", "No", ifelse(mexico2$Diabetes == "SI", "Yes", "Unknown"))
table(mexico2$Diabetes)

# 13.
table(mexico2$COPD)
mexico2$COPD <- ifelse(mexico2$COPD == "SE IGNORA", "Unknow",
                        ifelse(mexico2$COPD == "SI","Yes", "No"))
table(mexico2$COPD)

# 14.
table(mexico2$Asthma)
mexico2$Asthma <- ifelse(mexico2$Asthma == "SE IGNORA", "Unknow",
                       ifelse(mexico2$Asthma == "SI","Yes", "No"))
table(mexico2$Asthma)

# 15.
table(mexico2$Immunosuppression)
mexico2$Immunosuppression <- ifelse(mexico2$Immunosuppression == "SE IGNORA", "Unknow",
                         ifelse(mexico2$Immunosuppression == "SI","Yes", "No"))
table(mexico2$Immunosuppression)

# 16.
table(mexico2$Hypertension)
mexico2$Hypertension <- ifelse(mexico2$Hypertension == "SE IGNORA", "Unknow",
                                    ifelse(mexico2$Hypertension == "SI","Yes", "No"))
table(mexico2$Hypertension)

# 17.
table(mexico2$Other_disease)
mexico2$Other_disease <- ifelse(mexico2$Other_disease == "SE IGNORA", "Unknow",
                               ifelse(mexico2$Other_disease == "SI","Yes", "No"))
table(mexico2$Other_disease)

# 18.
table(mexico2$Cardiovascular)
mexico2$Cardiovascular <- ifelse(mexico2$Cardiovascular == "SE IGNORA", "Unknow",
                                ifelse(mexico2$Cardiovascular == "SI","Yes", "No"))
table(mexico2$Cardiovascular)

# 19.
table(mexico2$Obesity)
mexico2$Obesity <- ifelse(mexico2$Obesity == "SE IGNORA", "Unknow",
                                 ifelse(mexico2$Obesity == "SI","Yes", "No"))
table(mexico2$Obesity)

# 20.
table(mexico2$CKD)
mexico2$CKD <- ifelse(mexico2$CKD == "SE IGNORA", "Unknow",
                          ifelse(mexico2$CKD == "SI","Yes", "No"))
table(mexico2$CKD)

# 21.
table(mexico2$Smoke)
mexico2$Smoke <- ifelse(mexico2$Smoke == "SE IGNORA", "Unknow",
                      ifelse(mexico2$Smoke == "SI","Yes", "No"))
table(mexico2$Smoke)

# 22.
table(mexico2$Other.cov)
mexico2$Other.cov <- ifelse(mexico2$Other.cov == "NO ESPECIFICADO", "Unknow",
                        ifelse(mexico2$Other.cov == "SI","Yes", "No"))
table(mexico2$Other.cov)

# 23.
table(mexico2$Results)
mexico2$Results <- ifelse(mexico2$Results == "No positivo SARS-CoV-2", "Negative",
                        ifelse(mexico2$Results == "Resultado pendiente","Undecided", "Positive"))
table(mexico2$Results)

# 24.
table(mexico2$Immigration)
mexico2$Immigration <- ifelse(mexico2$Immigration == "NO ESPECIFICADO", "Unknow",
                        ifelse(mexico2$Immigration == "SI","Yes", "No"))
table(mexico2$Immigration)

# 25, 26 country names, no need to change 
table(mexico2$Nation)
table(mexico2$Nation.left)


# 27.
table(mexico2$ICU)
mexico2$ICU <- mexico2$ICU <- ifelse(mexico2$ICU == "NO APLICA" | mexico2$ICU == "NO ESPECIFICADO" , "Unknow", 
                                            ifelse(mexico2$ICU == "SI", "Yes", "No"))
table(mexico2$ICU)


# View(mexico2)
# copy the data
mexico3 <- mexico2
# dim(mexico3)   # 1565452 obs

# only contain negative/positve for test results
mexico.pos <- subset(mexico3, mexico3$Results == "Positive")   # 688954 obs and 27 cols
mexico.neg <- subset(mexico3, mexico3$Results == "Negative")   # 797447 obs and 27 cols
#  79051 cases undecided

## Age
summary(mexico.pos$Age)
summary(mexico.neg$Age)

## diabetes
table(mexico3$Diabetes)  # 189939 yes and 1370798 no
table(mexico.pos$Diabetes) # 107063 true
table(mexico.neg$Diabetes)  # 74336 true
prop.test(x = c(107063, 647), n = c(688954, 797447),alternative = "greater")

## Obsesity
table(mexico3$Obesity)  # 241208 yes and 1320066 no
table(mexico.pos$Obesity) # 125269 true
table(mexico.neg$Obesity)  # 106236 true

Obesity_subset <- subset(mexico3, mexico3$Obesity == "Yes")  # 241208 
No_Obesity_subset <- subset(mexico3, mexico3$Obesity == "No")  # 1320066 
table(Obesity_subset$Diabetes) # 50356 true   50356/241208 = 0.2087659
table(No_Obesity_subset$Diabetes) # 139395 true   139395/1320066 = 0.105597




## chd
table(mexico.pos$Cardiovascular)  # 13676 true 
table(mexico.neg$Cardiovascular)  # 14999 true
prop.test(x = c(13676, 14999), n = c(688954, 797447),alternative = "greater")

## htn
table(mexico.pos$Hypertension)  # 133939 true 
table(mexico.neg$Hypertension)  # 105155 true 
prop.test(x = c(133939, 105155), n = c(688954, 797447),alternative = "greater")
 
## smoke
table(mexico.pos$Smoke)   # 49878 true 
table(mexico.neg$Smoke)   # 72220 true 
prop.test(x = c(49878, 72220), n = c(688954, 797447),alternative = "less")


## Asthma
table(mexico.pos$Asthma)   # 17979 true 
table(mexico.neg$Asthma)   # 24962 true 
prop.test(x = c(17979, 24962), n = c(688954, 797447),alternative = "less")


## copd
table(mexico.pos$COPD)   # 10153 true 
table(mexico.neg$COPD)   # 9485 true 
prop.test(x = c(10153, 9485), n = c(688954, 797447),alternative = "greater")



## Immunosuppression
table(mexico3$Immunosuppression)    # 19361 true
table(mexico.pos$Immunosuppression)   # 7536 true   
table(mexico.neg$Immunosuppression)   # 10904 true 
prop.test(x = c(7536, 10904), n = c(688954, 797447),alternative = "less")


## Immunosuppression and COVID19
tbl1 <-table(mexico3$Immunosuppression, mexico3$Results)
tbl1

chisq.test(tbl1) 


mexico4 <- subset(mexico3, mexico3$Results != "Undecided")
tbl2 <-table(mexico4$Immunosuppression, mexico4$Results)
tbl2

chisq.test(tbl2) 


## Immunosuppression and diabetes
Imm <- subset(mexico3, mexico3$Immunosuppression == "Yes")  # 19361 
No_Imm <- subset(mexico3, mexico3$Immunosuppression == "No")  # 1541610

table(Imm$Diabetes)  # 4996 yes
table(No_Imm$Diabetes)  # 184732 yes
prop.test(x = c(4996, 184732), n = c(19361, 1541610),alternative = "greater")


table(Imm$Asthma)  # 1103 yes
table(No_Imm$Asthma)  # 43535 yes
prop.test(x = c(1103, 43535), n = c(19361, 1541610),alternative = "greater")



# The following codes were written in June, probably which is not useful now
# at that time,, I am focusing on analyzing on diabetes
######## Graphs #########
############################
### 1. Age
# show the distribution of age
m1 <- ggplot(mexico3, aes(x=Age)) + 
  geom_histogram(color="black", fill="lightblue") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6))

m1

# Add mean line
m1 + geom_vline(aes(xintercept=mean(Age)),
              color="red", linetype="dashed", size=1) + labs(title="Histogram of Age with Mean Line",x="Age")

# Histogram with density plot
m1.1 <- ggplot(mexico3, aes(x=Age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Density Plot of Age", x="Age")

m1.1

summary(mexico3$Age)

### 2. Results
m2 <- ggplot(data = mexico2, aes(x = Results)) +
  geom_bar(fill="lightblue") +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Barplot of COVID-19 Testing", x="COVID-19 Test Results")

m2

### 3. Diabetes
m3 <- ggplot(data = mexico2, aes(x = Diabetes)) +
  geom_bar(fill="lightblue") +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Barplot of Diabetes", x="Diabetes")

m3

table(mexico3$Diabetes)
prop.table(table(mexico3$Diabetes))  # 0.129891611 

pos <- subset(mexico3, mexico3$Results == "Positive")
dim(pos)
table(pos$Diabetes)
prop.table(table(pos$Diabetes))  # 0.177563026 

prop.table(table(mexico3$Obesity))  # 0.165606188
prop.table(table(pos$Obesity))  # 0.205747023


# relate Diabetes with Results
m3.1 <- ggplot(data = mexico2, aes(x = Results, fill=Diabetes)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Diabetes and COVID-19 Testing", x="COVID-19 Test Results")
m3.1

# relate Diabetes with Age
m3.2 <- ggplot(data = mexico2, aes(x = Diabetes, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Age and Diabetes", x="Diabetes", y = "Age")

m3.2

# get the summary table to compare the median of Age
tapply(mexico2$Age, mexico2$Diabetes, summary)


### 4. Obesity
m4 <- ggplot(data = mexico2, aes(x = Obesity)) +
  geom_bar(fill="lightblue") +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Barplot of Obesity", x="Obesity")

m4

# relate Obesity with Diabetes
m4.1 <- ggplot(data = mexico2, aes(x = Obesity, fill=Diabetes)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Diabetes and Obesity", x="Obesity")
m4.1

# get the summary table to compare the median of Age
tapply(mexico2$Age, mexico2$Diabetes, summary)

### 5. Asthma
m5 <- ggplot(data = mexico2, aes(x = Asthma)) +
  geom_bar(fill="lightblue") +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Barplot of Asthma", x="Asthma")

m5

# relate Diabetes with Results
m5.1 <- ggplot(data = mexico2, aes(x = Results, fill=Asthma)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Asthma and COVID-19 Testing", x="COVID-19 Test Results")
m5.1

### 6. Immunosuppression
m6 <- ggplot(data = mexico2, aes(x = Immunosuppression)) +
  geom_bar(fill="lightblue") +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Barplot of Immunosuppression", x="Immunosuppression")

m6

# relate Immunosuppression with Results
m6.1 <- ggplot(data = mexico2, aes(x = Results, fill=Immunosuppression)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=0.4, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Immunosuppression and COVID-19 Testing", x="COVID-19 Test Results")
m6.1

# relate Immunosuppression with Diabetes
m6.2 <- ggplot(data = mexico2, aes(x = Diabetes, fill=Immunosuppression)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),size=3.5,position = position_stack(vjust = 0.5)) +
  scale_colour_brewer(palette="Paired") + 
  theme(plot.background = element_blank(),
        panel.background = element_blank(), axis.line = element_line(size=1, colour = "black"),
        axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
        axis.text.y=element_text(colour="black", size = 6)) + labs(title="Diabetes and Immunosuppression", x="Obesity")
m6.2





