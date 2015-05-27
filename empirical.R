#####Assignment C

###Question 1: Set data ~ Done (need to include the functions below in the report)
#set data in R

tomLocExcel <- "C:/Users/Tom/Documents/GitHub/Empirical-Research-Methods/set5.xlsx"
edLocExcel <- ""
maikLocExcel <- ""
tomLocJava <- 'C:\\Program Files\\Java\\jre1.8.0_45'
edLocJava <- ''
maikLocJava <- ''

Sys.setenv(JAVA_HOME=tomLocJava)
library(xlsx)

mydata <- read.xlsx(tomLocExcel, 1)

#create factors for the data
mydata$Gender<-factor(mydata$Gender, levels =
              c(0:1), labels =
              c("Male","Female"))

mydata$Game_experience<-factor(mydata$Game_experience, levels =
                        c(0:4), labels =
                        c("Never","Less than once a month","Once or twice a month","Once or twice a week","Every day"))

mydata$Platform<-factor(mydata$Platform, levels =
                        c(0:3), labels =
                        c("Platform A","Platform B","Platform C","Platform D"))

mydata$Speech_regn<-factor(mydata$Speech_regn, levels =
                          c(0:1), labels =
                          c("English","English and Dutch"))

mydata$English_skill<-factor(mydata$English_skill, levels =
                             c(0:1), labels =
                             c("low","high"))

mydata
##Question 2: Reliability analysis
library(psych)

#take subset of data
relData = mydata[,c("Q1_use_avatar","Q2_use_avatar","Q3_use_avatar","Q4_use_avatar","Q5_use_avatar","Q6_use_avatar")]

#2 vragen:
#1. "Consider whether all items should be included in the scale", 
#   Betekent dit dat sommige waardes van de likert scale niet mee hoeven te tellen?
#2. "Based on analysis establish an aggregated a single measure to represent a person's attitude towards the use of avatars."
#   Moeten we hier kijken naar de scores voor Q1-Q6 en een functie maken die dan adhv die scores en de verschillende vragen
#   1 waarde teruggeeft?
alpha(relData)
splitHalf(relData, raw=TRUE, brute=FALSE, n.sample=10000, covar=FALSE)

###Question 3: Data exploration

###Question 4: Chi-square

###Question 5: Error -bars

###Question 6: One-sample t-test

###Question 7: Independent-sample t-test

###Question 8: Paired sample t-test

###Question 9: Man-Whitney U test
mydata9 <- read.xlsx(tomLocExcel, 1)
mydata$Gender<-factor(mydata9$Gender, levels = c(0:1))
mydata$Game_experience<-factor(mydata9$Game_experience, levels = c(0:4))
relData = mydata9[, c("Gender", "Game_experience")]

wilcox.test(relData$Game_experience~relData$Gender)

###Question 10: One-way ANOVA

###Question 11: Two-way ANOVA
test = aov(task_time_sec~English_skill*Speech_regn, data=mydata)
summary(test)
print(model.tables(test,"means"),digits=3) 

###Question 12: ANOVA with repeated measures

###Question 13: MANOVA

###Question 14: Correlation

###Question 15: Regression Analysis

###Question 16: Sample Size