rankhospital <- function(state, outcome, num){ #num is integer
  library(dplyr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- filter(data, State == state)
  
  if (outcome == "heart attack"){
    data[,11] <- as.numeric(data[,11])
    data <- data[!is.na(data[,11]),]
    data <- arrange(data, data[,11], data[,2])
    
    if (num == "best"){
      return(data[1,2])
    }
    if (num == "worst"){
      return(data[nrow(data),2])
    }
    else {
      return(data[num,2])
    }
  }

  if (outcome == "heart failure"){
    data[,17] <- as.numeric(data[,17])
    data <- data[!is.na(data[,17]),]
    data <- arrange(data, data[,17], data[,2])
    if (num == "best"){
      return(data[1,2])
    }
    
    if (num == "worst"){
      return(data[nrow(data),2])
    }
    
    else {
      return(data[num,2])
    }
  }

  if (outcome == "pneumonia"){
    data[,23] <- as.numeric(data[,23])
    data <- data[!is.na(data[,23]),]
    data <- arrange(data, data[,23], data[,2])
    if (num == "best"){
      return(data[1,2])
    }
    
    if (num == "worst"){
      return(data[nrow(data),2])
    }
    
    else {
      return(data[num,2])
    }
  }
}