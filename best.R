best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  dataset <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  hospital_data <- subset(dataset,State == state)
  
  if(nrow(hospital_data)==0){
    stop("invalid state")
  }
  
  
  if(outcome=="heart attack"){
    hospital_data <- hospital_data[complete.cases(hospital_data[,11]),]
    hospital_data$target <- as.numeric(hospital_data[,11])
  }
  else if(outcome=="heart failure"){
    hospital_data <- hospital_data[complete.cases(hospital_data[,17]),]
    hospital_data$target <- as.numeric(hospital_data[,17])
  }
  else if(outcome=="pneumonia"){
    hospital_data <- hospital_data[complete.cases(hospital_data[,23]),]
    hospital_data$target <- as.numeric(hospital_data[,23])
  }else{
    stop("invalid outcome")
  }
  
  hospital_data <- subset(hospital_data,hospital_data$target == min(hospital_data$target,na.rm=TRUE),c("Hospital.Name","target"))
  sort(hospital_data$Hospital.Name)[1]
}