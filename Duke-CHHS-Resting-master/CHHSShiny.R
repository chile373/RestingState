library(shiny)
library(ggplot2)
library(lattice)
library(plyr)
library(rsconnect)
library(shinyapps)

#In order to upload longdata, you need to run this code
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

dir <- "/Users/cwh27/Desktop/Data"
setwd(dir)
CC <- read.csv("meancluscoeff.csv", header=TRUE)
LE <- read.csv("meanlocaleff.csv", header=TRUE)
DG <- read.csv("meandeg.csv", header=TRUE)
ST <- read.csv("meanstrength.csv", header=TRUE)

IDs <- as.character(c(12481,	12534,	12536,	12537,	12538,	12539,	12540,	12541,	12546,	12547,	12548,	12550,	12553,	12559,	12808,	12809,	12812, 12481,	12534,	12536,	12537,	12538,	12539,	12540,	12541,	12546,	12547,	12548,	12550,	12553,	12559,	12808,	12809,	12812))
Time <- as.character(c(rep(1,17), rep(2,17)))

dflist <- list(CC, LE, DG, ST)
dfs <- lapply(dflist, function(d){
  d$ID <- IDs
  d$Time<-Time
  d
})

library(reshape)
dfs_2<-lapply(dfs, function(d){
  melt(d, id=c("ID","Time"))
})

network <- c(rep("Global", 340), rep("Frontal",340), rep("Parietal",340), rep("Temporal",340), rep("Mediotemporal",340), rep("Occipital", 340), rep("Subcortical", 340), rep("Cerebellar", 340))
threshold <- c(rep(c(1:10),8, each=34))
dfs_3 <- lapply(dfs_2, function(d){
  d$Network <- network
  d$Threshold <- ((threshold-1)/10)
  d$variable <- NULL
  d
})

head(dfs_3[[1]], n=20)

dfs_3[[1]]$ClusteringCoefficient <- dfs_3[[1]]$value
dfs_3[[2]]$LocalEfficiency <- dfs_3[[2]]$value
dfs_3[[3]]$Degree <- dfs_3[[3]]$value
dfs_3[[4]]$Strength <- dfs_3[[4]]$value
dfs_3[[1]]$value <- NULL
dfs_3[[2]]$value <- NULL
dfs_3[[3]]$value <- NULL
dfs_3[[4]]$value <- NULL

mydata<-Reduce(merge, dfs_3)
longdata <- melt(mydata, id=c("ID", "Time", "Network", "Threshold"))
longdata$Metric <- longdata$variable
longdata$variable <- NULL

frontsumCC <- ddply(mydata, c("threshold", "network", "time"), summarise, 
                    N = length(ClusteringCoefficient),
                    meanCC = mean(ClusteringCoefficient),
                    sdCC = sd(ClusteringCoefficient),
                    seCC = sd/sqrt(N),
                    meanDG = mean(Degree),
                    sdDG = sd(Degree),
                    seDG = sd/sqrt(N),
                    meanLE = mean(LocalEfficiency),
                    sdLE = sd(LocalEfficiency),
                    seLE = sd/sqrt(N),
                    meanST = mean(Strength),
                    sdST = sd(Strength),
                    seST = sd/sqrt(N)
)

#Run this code in order to make the ShinyApp
str(longdata)
longdata$Network <- as.factor(longdata$Network)
longdata$Threshold <- as.numeric(longdata$Threshold)
longdata$Threshold <- longdata$Threshold/10
metriclist<-unique(as.character(longdata$Metric))
subnetlist <- unique(as.character(longdata$Network))
sort(subnetlist)
sort(metriclist)

ui <- fluidPage(
  sidebarPanel(
    selectInput("Metrics", "Choose a graph metric:", choices=metriclist, selected ="Degree"),
    selectInput("Networks", "Choose a SubNetwork:", choices=subnetlist, selected ="Global"),
    selectInput("Compare", "Choose a Comparison:", choices=as.character(longdata$Network)),
  ),
  mainPanel(
    plotOutput("testPlot")
  ))
server <- function(input, output){
  Metric <- reactive(input$Metrics)
  Networks <- reactive(input$Networks)
  Compare <- reactive(input$Compare)
  output$testPlot <- renderPlot({
    pdata <- subset(longdata, Network==as.character(Networks()))
    pdata <- subset(pdata, Metric==as.character(Metric()))
    psum <- ddply(pdata, c("Threshold", "Value"), summarize,
                  N = length(value),
                  mean = mean(value),
                  sd = sd(value),
                  se = sd / sqrt(N)
    )
    ggplot(psum, aes(x=Threshold, y=mean, colour=Value, group=Value))+
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=0.01, size=0.75)+
      geom_line(size=1.25)+geom_point(size=3)+theme_classic(base_size = 14)+xlab("r Threshold") +
      ylab(metric()) +
      guides(colour=guide_legend(title="Pre-Season and Post-Season for CHHS Players"))+
      scale_color_manual(values=c("#96C0E6", "#D5BA31"))+
      theme(legend.justification = c(1, 1), legend.position = c(1, 1))+
      theme(axis.line.x=element_line(color="black", size=0.5), axis.line.y=element_line(color="black", size=0.5))
  })
}


shinyApp(ui = ui, server = server)

