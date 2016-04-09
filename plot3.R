plot3<-function(){
    library(ggplot2)
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    data<-split(NEI,as.factor(NEI$type))
    rm(NEI,SCC)
    d<-sapply(data,function(x)return(tapply(x$Emissions,x$year,sum)))
    df<-data.frame(Emission=as.vector(d),Year = rep(c(1999,2002,2005,2008),
              length(d[,1])),Type = rep(names(as.data.frame(d)),each=length(d[,1])))
    qplot(Year,Emission,data = df,color = Type,geom = c("point","smooth"))
    dev.copy(png, file = "plot3.png")
    dev.off()
    
  
}