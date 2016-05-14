#' Title
#'
#' @param state 
#' @param outcome 
#' @param num 
#'
#' @return Hospital.Name
#' @export
#'
#' @examples
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcomeColumn <- switch (
    outcome,
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23,
    stop("invalid outcomes")
  )
  setwd("D:/Learning/DS/R/Assigment")
  outcomeData <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!(state %in% outcomeData$State))
    stop("invalid states")
  else
  {
    outcomeData[, outcomeColumn] <-
      as.numeric(outcomeData[, outcomeColumn])
    
    outcomeData<-na.omit(outcomeData[outcomeData$State==state,])
    outcomeData<-outcomeData[order(outcomeData[,outcomeColumn],outcomeData$Hospital.Name),]
    if(is.numeric(num)){
      if(num > nrow(outcomeData))
        print(NA)
      else
      {
        outcomeData$Hospital.Name[num]
      }
    }
    else{
      num<-as.character(num)
      if(num=="best")
      {
        outcomeData$Hospital.Name[1]
      }
      else if(num=="worst")
      {
        outcomeData$Hospital.Name[nrow(outcomeData)]
      }
      else{
        stop("invalid num")
      }
    }
    
  }
}