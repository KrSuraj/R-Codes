library(datasets)
library(ggplot2)
load(ToothGrowth)
tg<-ToothGrowth
ggplot(data = tg, aes(x = as.factor(dose),y = len,fill = supp))
   +geom_bar(stat = "identical")
   +facet_grid(.~supp)
   +xlab("Dose")+ylab("Tooth Length")
   +guides(fill = guide_legend(title = "Supplement type"))
