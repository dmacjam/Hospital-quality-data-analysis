rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  library(dplyr)
  
  dataset <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  hospital_data <- subset(dataset,State == state)
  
  if(nrow(hospital_data)==0){
    stop("invalid state")
  }
  
  if(outcome=="heart attack"){
    hospital_data$target <- as.numeric(hospital_data[,11])
  }
  else if(outcome=="heart failure"){
    hospital_data$target <- as.numeric(hospital_data[,17])
  }
  else if(outcome=="pneumonia"){
    hospital_data$target <- as.numeric(hospital_data[,23])
  }else{
    stop("invalid outcome")
  }
  
  #drop NA
  hospital_data <- subset(hospital_data,!is.na(hospital_data$target)) 
  # select only working columns
  hospital_data <- subset(hospital_data,select=c("Hospital.Name","target"))
  
  # sort
  ordered <- arrange(hospital_data,target,Hospital.Name)
  
  if(num=="best"){
    result <- ordered[1,]$Hospital.Name
  }else if(num=="worst"){
    result <- ordered[nrow(ordered),]$Hospital.Name
  }else{
    result <- ordered[num,]$Hospital.Name
  }
  
  result
}