str(mydata)
library(plyr)
summydata <- ddply(mydata, c("Threshold", "Network", "Time"), summarise, 
                    meanCC = mean(ClusteringCoefficient),
                    sdCC = sd(ClusteringCoefficient),
                    seCC = sdCC/sqrt(N),
                    meanDG = mean(Degree),
                    sdDG = sd(Degree),
                    seDG = sdDG/sqrt(N),
                    meanLE = mean(LocalEfficiency),
                    sdLE = sd(LocalEfficiency),
                    seLE = sdLE/sqrt(N),
                    meanST = mean(Strength),
                    sdST = sd(Strength),
                    seST = sdST/sqrt(N)
                    )
sumlongdata <- ddply(longdata, c("Threshold", "Network", "Time", "Metric"), summarise,
                     meanvalue = mean(value),
                     sdvalue = sd(value),
                     sevalue = sdvalue/sqrt(17)
                     )
mydata
test<-ddply(mydata, c("Threshold","Network","Time"), summarize, meanCC=mean(ClusteringCoefficient))
library(ggplot2)

sumCC <- subset(frontsumCC, Network=="Global")
a<-ggplot(sumCC, aes(x=Threshold, y=meanCC, color=Time))
a+geom_point()
a+geom_errorbar(aes(ymin=meanCC-seCC, ymax=meanCC+seCC))

b<-ggplot(sumlongdata, aes(x=Threshold, y=meanvalue, color=Time))
b+ geom_point() + facet_grid(Metric~Network,)

subSLD <- subset(sumlongdata, Metric%in%c("Degree", "Strength"))
b<-ggplot(subSLD, aes(x=Threshold, y=meanvalue, color=Time))
c<- b+ geom_point() + facet_grid(Metric~Network,)
c+ geom_errorbar(aes(ymin=meanvalue-sevalue, ymax=meanvalue+sevalue))

subfro <- subset(subSLD, Network%in%c("Frontal", "Global"))
d<-ggplot(subfro, aes(x=Threshold, y=meanvalue, color=Time))
e<- d + geom_point() + facet_grid(Metric~Network)
e+ geom_errorbar(width=.0125,aes(ymin=meanvalue-sevalue, ymax=meanvalue+sevalue))

