rankall <- function(outcome, num = "best") {
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
  outcomenumber<-(outcome == "heart attack"|| outcome == "pneumonia"|| outcome == "heart failure")
  if(!outcomenumber)
    stop("invalid outcome")
  
  
  
  outcomebool<-data[outcome] == "Not Available"
  data<-data[!outcomebool,]
  
  
  data<-split(data,data$state)
   
    if(num == "best")
      num <- 1
  
     
    
    library(plyr)
    data2<-lapply(data,function(d){
      d<-arrange(d,d[["name"]])
      arrange(d,as.numeric(d[[outcome]]))
    }
  )
  
  
  
  fn2<-lapply(data2,function(x){y<-x[["state"]]
                                y[1]
  })
  
  fn1<-lapply(data2,function(x){y<-x[["name"]]
                                if (num == "worst"){
                                  y[length(y)]
                                
                                }
                                else y[num]
  })
  
  
  fn3<-cbind(fn1,fn2)
  
  retval<-data.frame(fn3)
  names(retval)[1]<-"hospital"
  names(retval)[2]<-"state"
  
  retval
}