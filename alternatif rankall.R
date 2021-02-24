rankall <- function(outcome, num = "best"){
  library(dplyr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- arrange(data, State)
  state <- unique(data$State)
  
  if (num == "best"){
    num <- 1
  }
  
  
  hospital <- c()
  for (i in state){
    if (outcome == "heart attack"){
      data <- arrange(data, data[,11], data[,2])
    
    }
  }
}
    