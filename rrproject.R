calc<- function() {
  data<-read.csv("activity.csv")
  data<-transform(data,as.Date(date))
  
  data2<-split(data, data$date)
  daystravel<-sapply(data2,function(x)return (sum(x$steps, na.rm = TRUE)))
  meanvalue<-sapply(data2, function(x)return(mean(x$steps,na.rm = TRUE)))
  medianvalue<-sapply(data2, function(x)return(median(x$steps,na.rm = TRUE)))
  mean<-mean(daystravel,na.rm = TRUE)
  median<-median(daystravel)
  print(paste("Mean and median of the total number of steps taken per day are",mean,"and",median,"respectively", sep = " "))
  res<-cbind(meanvalue,medianvalue,date = levels(data$date))
  res<-data.frame(res)
  res$daystravel<-daystravel
  hist(res$daystravel,xlab = "Number of Steps")
  plot(as.Date(res$date),res$daystravel,type="l",xlab="Date",ylab="Number of Steps")
  
  data3<-split(data,data$interval)
  data4<-sapply(data3,function(x)return (mean(x$steps, na.rm = TRUE)))
  intervals<-levels(as.factor(data$interval))
  plot(intervals,data4,type = "l",xlab = "5-minute interval",ylab= "Average number of steps")
  tmaxsteps<-as.vector(intervals[which.max(data4)])
  print(paste("Interval with maximum average number of steps is",tmaxsteps, "-", as.numeric(tmaxsteps)+5))
    
  num<-sum(is.na(data[,1]))
  print(paste("There are",num,"missing values in data",sep = " "))
  newdata<-data
  for(i in 1:nrow(newdata))
  {
    if(is.na(newdata[i,]$steps)){
      newdata[i,]$steps <- mean(data4)
    }
  }
  newdata2<-split(newdata, newdata$date)
  newdaystravel<-sapply(newdata2,function(x)return (sum(x$steps, na.rm = TRUE)))
  newmean<-mean(newdata$step)
  newmedian<-median(newdata$steps)
  hist(newdaystravel)
  print(paste("Mean and median of the total number of steps taken per day for tha data obtained by replacing missing values are",newmean,"and",newmedian,"respectively",sep = " "))
  
  
  newdata$daytype<-rep("data unavailable",length(newdata[,1]))
    day<-weekdays(as.Date(newdata$date),abbreviate = TRUE)
  for (j in 1:length(newdata[,1])){
  
    if(day[j]=="Mon"|day[j]=="Tue"|day[j]=="Wed"|day[j]=="Thu"|day[j]=="Fri")
         newdata[j,]$daytype <- "weekday"
    else newdata[j,]$daytype<-"weekend"
  }
  library(lattice)
  xyplot(steps~interval|daytype,data = newdata,type = "l",layout = c(1,2))

  
}
