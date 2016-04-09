plot4<-function(){
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  index<-grep("coal", SCC$Short.Name,ignore.case = TRUE,fixed = FALSE)
  data<-SCC[index,1:4]
  rm(SCC)
  combdata<-data[grep("comb",data$Short.Name,ignore.case = TRUE,fixed = FALSE),]
  boolindex<-NEI$SCC%in%combdata$SCC
  coaldata<-NEI[boolindex,]
  yearwisesum <-c(sum(coaldata[coaldata$year==1999,"Emissions"]),
                  sum(coaldata[coaldata$year==2002,"Emissions"]),
                  sum(coaldata[coaldata$year==2005,"Emissions"]),
                  sum(coaldata[coaldata$year==2008,"Emissions"]))
  year<-c(1999,2002,2005,2008)
  plot(year,yearwisesum,type = "l",lwd = 2, pch = 8,col = "grey",
       ylab = "Emission from Coal-related combustion")
  points(year,yearwisesum,pch = 19)
  dev.copy(png,"plot4.png")
  dev.off()
}