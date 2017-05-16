best <- function(state, outcome) {
  require(dplyr)
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_data = filter(outcome_data,State==state)
  if(dim(state_data)[1] == 0) {
    stop("invalid state")
  }
  
  prefix = "Hospital.30.Day.Death..Mortality..Rates.from."
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  #Construct the outcome name as it appears in the dataset
  #Capitalize the outcome, split it on spaces, reassemble it with "." and add the
  #prefix
  outcome_name = paste(strsplit(simpleCap(outcome), " ")[[1]],collapse=".")
  column_name = paste(prefix,outcome_name,sep="",collapse = "")
  if(!any(colnames(outcome_data)==column_name)){
    stop("invalid outcome")
  }
  
  #get the hospital name and outcome data
  name_outcome <- select_(state_data,"Hospital.Name",column_name)
  #convert the character data to numeric
  name_outcome[,2] <- suppressWarnings(as.numeric(name_outcome[,2]))
  #sort by the outcome, lowest numbers first
  ordered <- arrange_(name_outcome,column_name)
  ordered[1,1]
  
}