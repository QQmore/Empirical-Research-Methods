#####Assignment C

###Question 1: Set data ~ Done (need to include the functions below in the report)
#set data in R
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_45')
library(xlsx)
mydata <- read.xlsx("C:/Users/Maikel/Documents/GitHub/Empirical-Research-Methods/set5.xlsx", 1)

#create factors for the data
mydata$Gender<-factor(mydata$Gender, levels =
              c(0:1), labels =
              c("Male","Female"))

mydata$Most_pref_Avatar<-factor(mydata$Most_pref_Avatar, levels =
                                 c(1:8), labels =
                                 c("Male_European","Male_East Asian","Male_African","Male_South Asian","Female_European","Female_East Asian","Female_African","Female_South Asian"))


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
t.test(mydata$Select_avatar_1, mu=4)
t.test(mydata$Select_avatar_2, mu=4)
t.test(mydata$Select_avatar_3, mu=4)
t.test(mydata$Select_avatar_4, mu=4)
t.test(mydata$Select_avatar_5, mu=4)
t.test(mydata$Select_avatar_6, mu=4)
t.test(mydata$Select_avatar_7, mu=4)
t.test(mydata$Select_avatar_8, mu=4)

## Give an interpretation of the results and report the statistical results 
## in a small paragraph that could be included in conference paper1.

###Question 7: Independent-sample t-test
male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_1[i])
  } else  {
    female = c(female, mydata$Select_avatar_1[i])
  }
}

t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_2[i])
  } else  {
    female = c(female, mydata$Select_avatar_2[i])
  }
}

t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_3[i])
  } else  {
    female = c(female, mydata$Select_avatar_3[i])
  }
}
t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_4[i])
  } else  {
    female = c(female, mydata$Select_avatar_4[i])
  }
}
t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_5[i])
  } else  {
    female = c(female, mydata$Select_avatar_5[i])
  }
}
t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_6[i])
  } else  {
    female = c(female, mydata$Select_avatar_6[i])
  }
}
t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_7[i])
  } else  {
    female = c(female, mydata$Select_avatar_7[i])
  }
}
t.test(male, female)

male = c()
female = c()

for (i in 1:NROW(mydata)) {
  if(mydata$Gender[i] == "Male") {
    male = c(male, mydata$Select_avatar_8[i])
  } else  {
    female = c(female, mydata$Select_avatar_8[i])
  }
}
t.test(male, female)

###Question 8: Paired sample t-test

###Question 9: Man-Whitney U test

###Question 10: One-way ANOVA

###Question 11: Two-way ANOVA

###Question 12: ANOVA with repeated measures

###Question 13: MANOVA

###Question 14: Correlation

###Question 15: Regression Analysis

###Question 16: Sample Size