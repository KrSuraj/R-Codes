rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  data<- data[,c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  
  statedata<-NULL
  outcomedata<-NULL
  statedata<-(data["state"] == state)
  statenumber<-sum(statedata)
  if (statenumber < 1)
    stop ("invalid state")
  outcomenumber<-(outcome == "heart attack"|| outcome == "pneumonia"|| outcome == "heart failure")
  if(!outcomenumber)
    stop("invalid outcome")
  
  
  ## Return hospital name in that state with the given rank
  
  data<-data[data["state"]==state,]
  outcomebool<-data[outcome] == "Not Available"
  data<-data[!outcomebool,]
  data<-arrange(data,name)
  vals<-sort(as.numeric(data[,outcome]),index.return = TRUE)
  colname<-paste("i",outcome,sep="")
  bools<-vals$ix
  #data[outcome]
  bools
  #data[1:5,1:5]
  data<-data[bools,]
  if(num == "worst")
    num = dim(data)[1]
  if (num == "best")
    num = 1
  library(plyr)
  
  data[num,]$name
  ## 30-day death rate
}