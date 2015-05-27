#####Assignment C

###Question 1: Set data ~ Done (need to include the functions below in the report)
#set data in R
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_31')
library(xlsx)
mydata <- read.xlsx("C:/Users/edward/Documents/University/Empirical/set5.xlsx", 1)

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
##Question 2: Reliability analysis
library(psych)

#take subset of data
relData = mydata[,c("Q1_use_avatar","Q2_use_avatar","Q3_use_avatar","Q4_use_avatar","Q5_use_avatar","Q6_use_avatar")]
alpha(relData)

###Question 3: Data exploration

###Question 4: Chi-square

###Question 5: Error -bars

###Question 6: One-sample t-test

###Question 7: Independent-sample t-test

###Question 8: Paired sample t-test

###Question 9: Man-Whitney U test

###Question 10: One-way ANOVA

###Question 11: Two-way ANOVA

###Question 12: ANOVA with repeated measures

###Question 13: MANOVA

###Question 14: Correlation

###Question 15: Regression Analysis

###Question 16: Sample Size