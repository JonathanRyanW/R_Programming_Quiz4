best <- function(state, outcome){ #both are character strings
  library(dplyr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- filter(data, State == state)
  
  if (outcome == "heart attack"){
    best_rate <- min(as.numeric(data[,11]), na.rm = TRUE) #finding best rate
    location <- which(as.numeric(data[,11]) == best_rate) #finding locations
    hospital <- data$Hospital.Name[location] #finding all the hospital names
    hospital <- sort(hospital) #sorting the hospital names alphabetically
    return(hospital[1])
  }
  
  if (outcome == "heart failure"){
    best_rate <- min(as.numeric(data[,17]), na.rm = TRUE)
    location <- which(as.numeric(data[,17]) == best_rate)
    hospital <- data$Hospital.Name[location]
    hospital <- sort(hospital)
    return(hospital[1])
  }
  
  if (outcome == "pneumonia"){
    best_rate <- min(as.numeric(data[,23]), na.rm = TRUE)
    location <- which(as.numeric(data[,23]) == best_rate)
    hospital <- data$Hospital.Name[location]
    hospital <- sort(hospital)
    return(hospital[1])
  }
}