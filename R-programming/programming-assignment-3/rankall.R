rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
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
  
  outcomeData[, outcomeColumn] <-
    as.numeric(outcomeData[, outcomeColumn])
  
  outcomeData<-na.omit(outcomeData)
  
  s<-split(outcomeData,outcomeData$State)
  
  if(is.character(num))
  {
    if(num=="best")
    {
      lst<-lapply(s, function(x) x[order(x[,outcomeColumn]),]$Hospital.Name[1])
      data.frame(cbind(state=names(lst),hospital=unlist(lst,use.names = F)))
    }
    else if(as.character(num)=="worst")
    {
      lst<-lapply(s, function(x) x[order(x[,outcomeColumn]),]$Hospital.Name[nrow(x)])
      data.frame(cbind(state=names(lst),hospital=unlist(lst,use.names = F)))
    }
    else{
      stop("num as invalid character value")
    }
  }
  else if(is.numeric(num))
  {
    lst<-lapply(s, function(x) x[order(x[,outcomeColumn]),]$Hospital.Name[num])
    data.frame(cbind(state=names(lst),hospital=unlist(lst,use.names = F)))
  }
  else
  {
    stop("num has invalid values")
  }
}
