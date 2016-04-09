plot1<-function()
  
  {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  data<-split(NEI,as.factor(NEI$year))
  rm(NEI,SCC)
  p1data<-sapply(data,function(x) return (sum(x$Emissions)))#getting total emission
  plot(names(p1data),p1data,
       xlab = "Year",ylab="Pollutant Emission",pch = 8,lwd= 2)
  regline<-lm(p1data~as.numeric(names(p1data))) #fitting regression model
  abline(regline,lwd = 2,col="grey") #plotting linear regression line
  dev.copy(png,file ="plot1.png")
  dev.off()
  

}