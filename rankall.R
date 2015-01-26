rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  library(dplyr)
  
  dataset <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  hospital_data <- dataset
  
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
  hospital_data <- subset(hospital_data,select=c("Hospital.Name","State","target"))
  
  hospital_data$State <- factor(hospital_data$State)
  hospital_data <- hospital_data %>% group_by(State) %>% arrange(target,Hospital.Name)
  
  # split by groups
  splited <- split(hospital_data,hospital_data$State)
  
  #lapply
  if(num=="best"){
    result <- lapply(splited,function(x) x[1,])
  }else if(num=="worst"){
    result <- lapply(splited,function(x) x[nrow(x),])
  }else{
    result <- lapply(splited,function(x) x[num,])
  }
  
  result <- as.data.frame(do.call(rbind,result))
  result$state <- row.names(result)
  result <- subset(result,select=c("Hospital.Name","state"))
  result <- rename(result,hospital=Hospital.Name)
  result
}