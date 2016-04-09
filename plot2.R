plot2<-function(){

  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  data<- NEI[NEI$fips=="24510",]
  rm(NEI,SCC)
  yearlydata<-split(data,as.factor(data$year))
  yearlysum<-sapply(yearlydata,function(y)return(sum(y$Emission)))
  plot(names(yearlysum),yearlysum,type = "l",col = "grey",xlab="Year",ylab="Pollutant Emission")
  points(names(yearlysum),yearlysum,pch = 8,col = "black")
  dev.copy(png,file = "plot2.png")
  dev.off()
}