#' Title
#'
#' @param state 
#' @param outcome 
#'
#' @return
#' @export
#'
#' @examples
best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcomeColumn <- switch (
    outcome,
    "heart attack" = 13,
    "heart failure" = 19,
    "pneumonia" = 25,
    stop("invalid outcomes")
  )
  setwd("D:/Learning/DS/R/Assigment")
  outcomeData <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!(state %in% outcomeData$State))
    stop("invalid states")
  else{
    outcomeData[, outcomeColumn] <-
      as.numeric(outcomeData[, outcomeColumn])
    outcomeData<-na.omit(outcomeData[outcomeData$State==state,])
    outcomeData<-outcomeData[order(outcomeData$State),]
    bestHospitals <-
      outcomeData$Hospital.Name[which.min(outcomeData[, outcomeColumn])]
  }
  bestHospitals
}
