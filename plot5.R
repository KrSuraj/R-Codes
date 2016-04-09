plot5<-function(){
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  baltimore<-NEI[NEI$fips == "24510",]
  index<-grep("Mobile - On-Road",SCC$EI.Sector,ignore.case=T,fixed = F)
  motorSCC<-SCC[index,1:4]
  boolindex<-baltimore$SCC%in%motorSCC$SCC
  motordata<-baltimore[boolindex,]
  yearwisesum <-c(sum(motordata[motordata$year==1999,"Emissions"]),
                  sum(motordata[motordata$year==2002,"Emissions"]),
                  sum(motordata[motordata$year==2005,"Emissions"]),
                  sum(motordata[motordata$year==2008,"Emissions"]))
  year<-c(1999,2002,2005,2008)
  plot(year,yearwisesum,type = "l",lwd = 2, pch = 8,col = "grey",
       ylab = "Emission from Motor Vehicles")
  points(year,yearwisesum,pch = 19)
  dev.copy(png,"plot5.png")
  dev.off()
}