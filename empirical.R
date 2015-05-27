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

###Question 3: Data exploration ~ TODO: Give interpretation of the plots
#take select Avatar Data from the dataset
selectAvatarData = mydata[,c("Select_avatar_1","Select_avatar_2","Select_avatar_3","Select_avatar_4","Select_avatar_5","Select_avatar_6","Select_avatar_7","Select_avatar_8")]

#Stem-and-leaf plots
stem(selectAvatarData$Select_avatar_1,scale=0.5)
stem(selectAvatarData$Select_avatar_2,scale=0.5)
stem(selectAvatarData$Select_avatar_3,scale=0.5)
stem(selectAvatarData$Select_avatar_4,scale=0.5)
stem(selectAvatarData$Select_avatar_5,scale=0.5)
stem(selectAvatarData$Select_avatar_6,scale=0.5)

#Box plots
boxplot(selectAvatarData, ylab = "Agreement (1 = disagree, 7 = agree)", xlab ="Avatar Selection", xaxt="n")
axis(1, at=seq(1,8,by=1),labels=c("1","2","3","4","5", "6","7","8"), las = 1)

###Question 4: Chi-square (Went for multinomial test)
install.packages("EMT")
library(EMT)
#There are 8 numbers
p <- c(1/8,1/8,1/8,1/8, 1/8,1/8,1/8,1/8)
#Get the observed data
num1 <- length(which(mydata$Most_pref_Avatar == 1))
num2 <- length(which(mydata$Most_pref_Avatar == 2))
num3 <- length(which(mydata$Most_pref_Avatar == 3))
num4 <- length(which(mydata$Most_pref_Avatar == 4))
num5 <- length(which(mydata$Most_pref_Avatar == 5))
num6 <- length(which(mydata$Most_pref_Avatar == 6))
num7 <- length(which(mydata$Most_pref_Avatar == 7))
num8 <- length(which(mydata$Most_pref_Avatar == 8))

observed<-c(num1,num2,num3,num4,num5,num6,num7,num8)
out<-multinomial.test(observed,p)

#using chi-square
install.packages("MASS")
library(MASS)
tbl = table(mydata$Gender,mydata$Most_pref_Avatar)
chisq.test(tbl) 


###Question 5: Error -bars
install.packages("plotrix")
library(plotrix)
set.seed(0815)
require(plotrix)
#####Vraag: Niets met fitted value?

##Plot of the mean
#take select Avatar Data from the dataset
selectAvatarData = mydata[,c("Select_avatar_1","Select_avatar_2","Select_avatar_3","Select_avatar_4","Select_avatar_5","Select_avatar_6","Select_avatar_7","Select_avatar_8")]
meanAvatarData = colMeans(selectAvatarData)
plot(meanAvatarData, ylab= "Mean of Score Avatar Preference", xlab ="Avatar")

##Plot of the 95% confidence interval
#A 95% confidence interval is a range of values that you can be 95% certain contains the true mean of the population. 
#This is not the same as a range that contains 95% of the values. 
lowerBound = c()
higherBound = c()
meanBound = c()
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_1)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_1)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_1)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_2)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_2)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_2)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_3)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_3)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_3)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_4)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_4)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_4)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_5)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_5)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_5)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_6)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_6)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_6)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_7)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_7)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_7)$estimate)
lowerBound = c(lowerBound,t.test(selectAvatarData$Select_avatar_8)$conf.int[1:2][1])
higherBound = c(higherBound,t.test(selectAvatarData$Select_avatar_8)$conf.int[1:2][2])
meanBound = c(meanBound,t.test(selectAvatarData$Select_avatar_8)$estimate)
#error.bars(selectAvatarData)
x <- 1:8
plotCI(x, meanBound, ui=higherBound, li=lowerBound, ylab="Avatar Score",xlab="Avatar Selection")

###Question 6: One-sample t-test

###Question 7: Independent-sample t-test

###Question 8: Paired sample t-test

###Question 9: Man-Whitney U test

###Question 10: One-way ANOVA ~ ? No idea ? Vragen aan docent
#conceptual model
#independent variable : Platform
#devependent variable : Use of avatar
#model: Platform (A,B,C,D) --> use of avatar

oneQ1 <- oneway.test(mydata$Platform ~ mydata$Q1_use_avatar, var.equal=TRUE)
oneQ2 <- oneway.test(mydata$Platform ~ mydata$Q2_use_avatar, var.equal=TRUE)
oneQ3 <- oneway.test(mydata$Platform ~ mydata$Q3_use_avatar, var.equal=TRUE)
oneQ4 <- oneway.test(mydata$Platform ~ mydata$Q4_use_avatar, var.equal=TRUE)
oneQ5 <- oneway.test(mydata$Platform ~ mydata$Q5_use_avatar, var.equal=TRUE)
oneQ6 <- oneway.test(mydata$Platform ~ mydata$Q6_use_avatar, var.equal=TRUE)

#Sidak correction ?


###Question 11: Two-way ANOVA



###Question 12: ANOVA with repeated measures

###Question 13: MANOVA

###Question 14: Correlation

###Question 15: Regression Analysis

###Question 16: Sample Size