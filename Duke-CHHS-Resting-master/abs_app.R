library(shiny)
library(ggplot2)
library(lattice)
library(plyr)
library(rsconnect)
library(shinyapps)

dir <- "/Users/cwh27/Desktop/RestingState/Abs_CHHS/App/"
setwd(dir)
longdata <- read.csv("longdata_chhs_abs.csv", header= T)

#Run this code in order to make the ShinyApp
longdata$Time<-as.factor(longdata$Time)
metriclist<-levels(longdata$Metric)
subnetlist<-levels(longdata$Network)
sort(subnetlist)
sort(metriclist)

ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput("Metrics", "Choose a graph metric:", choices=metriclist, selected ="Degree"),
    checkboxGroupInput("Networks", "Choose a SubNetwork:", choices=subnetlist, selected ="Global")
  ),
  mainPanel(
    plotOutput("testPlot")
  ))
server <- function(input, output){
  Metric <- reactive(input$Metrics)
  Networks <- reactive(input$Networks)
  output$testPlot <- renderPlot({
    pdata <- subset(longdata, Network%in%(Networks()))
    pdata <- subset(pdata, Metric%in%Metric())
    psum <- ddply(pdata, c("Threshold", "Time", "Metric", "Network"), summarize,
                  N = length(value),
                  mean = mean(value),
                  sd = sd(value),
                  se = sd / sqrt(N)
    )
    ggplot(psum, aes(x=Threshold, y=mean, colour=Time))+
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=0.01, size=0.75)+
      geom_line(size=1.25)+geom_point(size=3)+theme_classic(base_size = 14)+xlab("r Threshold") +
      facet_grid(Metric~Network, scales="free_y") + 
      guides(color=guide_legend(title="Time"))+
      scale_color_manual(values=c("#96C0E6", "#D5BA31"))+
      #theme(legend.justification = c(1, 1), legend.position = c(1, 1))+
      theme(axis.line.x=element_line(color="black", size=0.5), axis.line.y=element_line(color="black", size=0.5))
  })
}


shinyApp(ui = ui, server = server)

