rankall <- function(outcome, num = 1){
  library(dplyr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- arrange(data, State)
  state <- unique(data$State)
  
  if (num != "worst"){
    hospital <- c()
    for (i in state){
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      data <- filter(data, State == i)
      
      if (outcome == "heart attack"){
        data[,11] <- as.numeric(data[,11])
        data <- arrange(data, data[,11], data[,2])
      }
      
      if (outcome == "heart failure"){
        data[,17] <- as.numeric(data[,17])
        data <- arrange(data, data[,17], data[,2])
      }
      
      if (outcome == "pneumonia"){
        data[,23] <- as.numeric(data[,23])
        data <- arrange(data, data[,23], data[,2])
      }
      hospital <- c(hospital, data[num,2])
    }
    return(as.data.frame(cbind(hospital, state)))
  }
  
  if (num == "worst"){
    hospital <- c()
    for (i in state){
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      data <- filter(data, State == i)
      
      if (outcome == "heart attack"){
        worst_rate <- max(as.numeric(data[,11]), na.rm = TRUE)
        location <- which(as.numeric(data[,11]) == worst_rate)
      }
      
      if (outcome == "heart failure"){
        worst_rate <- max(as.numeric(data[,17]), na.rm = TRUE)
        location <- which(as.numeric(data[,17]) == worst_rate)
      }
      
      if (outcome == "pneumonia"){
        worst_rate <- max(as.numeric(data[,23]), na.rm = TRUE)
        location <- which(as.numeric(data[,23]) == worst_rate)
      }
      
      names <- data[,2][location]
      hospital <- c(hospital, max(names))
  }
  return(as.data.frame(cbind(hospital, state)))
  }
}