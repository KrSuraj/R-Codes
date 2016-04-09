plot6<-function(){
  library(ggplot2)
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  baltimore<-NEI[NEI$fips == "24510",]
  LosAngeles<-NEI[NEI$fips == "06037",]
  index<-grep("Mobile - On-Road",SCC$EI.Sector,ignore.case=T,fixed = F)
  motorSCC<-SCC[index,1:4]
  rm(NEI,SCC)
  boolindex<-baltimore$SCC%in%motorSCC$SCC
  baltimore_data<-baltimore[boolindex,]
  baltimore_sum <-c(sum(baltimore_data[baltimore_data$year==1999,"Emissions"]),
                  sum(baltimore_data[baltimore_data$year==2002,"Emissions"]),
                  sum(baltimore_data[baltimore_data$year==2005,"Emissions"]),
                  sum(baltimore_data[baltimore_data$year==2008,"Emissions"]))
  LAindex<-LosAngeles$SCC%in%motorSCC$SCC
  LosAngeles_data<-LosAngeles[LAindex,]
  LosAngeles_sum <-c(sum(LosAngeles_data[LosAngeles_data$year==1999,"Emissions"]),
                    sum(LosAngeles_data[LosAngeles_data$year==2002,"Emissions"]),
                    sum(LosAngeles_data[LosAngeles_data$year==2005,"Emissions"]),
                    sum(LosAngeles_data[LosAngeles_data$year==2008,"Emissions"]))
  year<-c(1999,2002,2005,2008)
  plotdata<-data.frame(Emission<-c(LosAngeles_sum,baltimore_sum),year<-rep(year,2),
            Area<-rep(c("Los Angeles","Baltimore"),each = 4))
  names(plotdata)<-c("Emission","Year","Area")
  qplot(Year,Emission,data = plotdata,color = Area,geom = c("point","smooth"))
            
  dev.copy(png,file ="plot6.png")
  dev.off()
}