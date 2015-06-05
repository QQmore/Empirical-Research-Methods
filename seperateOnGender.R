seperateOnGender <- function (mydata) {
  male = c()
  female = c()
  
  for (i in 1:NROW(mydata)) {
    if(mydata$Gender[i] == "Male") {
      male = c(male, mydata$Select_avatar_1[i])
    } else  {
      female = c(female, mydata$Select_avatar_1[i])
    }
  }
}