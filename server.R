#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above (assuming you are running R Studio).
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#library(latex2exp)
library(MASS)
library(shiny)
library(data.table)
library(ggplot2)
library(visreg)
library(rgl)
library(tidyverse)
library(gcookbook)

slider_width <- 220
graph_height <- "60%"
graph_ht <- 260

####################################
#
# Define server logic required to draw a histogram
#
server <- function(input, output) {
  rnd <- 4 # number of digits to round numbers to in table
  # modify print function to always print exactly 'rnd' digits to make the chart look nicer
  print.numeric<-function(x, digits = rnd ) formatC(x, digits = digits, format = "f")
  
  trial_color <-"mediumorchid2"
  N_pop = 1000
  min_cuttoff <- 0
  max_cuttoff <- 15
  lognorm_cuttoff <- max_cuttoff
  x1_mu <- 7.5
  x1_sigma <- 1.7
  x2_mu <- 0.4
  x2_sigma <- 0.8
  x3_min <- 0
  x3_max <- 15
  
  x1_samp <- reactive({rnorm(N_pop*100,x1_mu,x1_sigma)})
  x1_b <- reactive({x1_samp()[x1_samp() < max_cuttoff]})
  x1 <- reactive({x1_b()[x1_samp() > min_cuttoff]})
  x2_samp <- reactive({rlnorm(N_pop*100, x2_mu, x2_sigma)})
  x2 <- reactive({x2_samp()[x2_samp() < max_cuttoff]})
  x3 <- reactive({runif(N_pop*10000,x3_min, x3_max)})
  
  x1_range_min <- reactive({0})
  x1_range_max <- reactive({15})
  
  x2_range_min <- reactive({0})
  x2_range_max <- reactive({lognorm_cuttoff})
  
  x3_range_min <- reactive({0})
  x3_range_max <- reactive({15})
  
  
  # set the bin size
  bin_step <- reactive({.1})
  ########
  #
  # Generate all the plots
  #
  #  The first three are histograms for the input variables, all normally distributed with slider-set
  #   values for number_samples, mean, and sd.
  #
  
  lsize <- 4.5
  dot_color <- "#888888"
  
  x_val <- seq(0,1,0.01)
  y1 <- dnorm(x_val,mean=x1_mu,sd=x1_sigma)
  dd <- data.frame(x_val,y1)
  max_density_ht <- 0.55
  
  
  output$x1Plot <- renderPlot({ 
    hist(x1(), breaks = seq(x1_range_min()-1,x1_range_max()+1, by=bin_step()), ylim=c(0,max_density_ht),
         border='lightblue', yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="Normal", col='lightblue')
    curve(dnorm(x, mean=x1_mu, sd=x1_sigma), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")}, height=graph_ht)
  
  output$x2Plot <- renderPlot({
    hist(x2(), breaks = seq(x2_range_min()-1,x2_range_max()+1, by=bin_step()), ylim=c(0,max_density_ht),
         border='lightgreen', yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="Lognormal", col='lightgreen')
    curve(dlnorm(x, x2_mu, x2_sigma), 
          col="darkgreen", lwd=2, add=TRUE, yaxt="n")}, height=graph_ht)
  
  output$x3Plot <- renderPlot({
    hist(x3(), breaks = seq(x3_range_min()-1,x3_range_max()+1, by=bin_step()), ylim=c(0,max_density_ht),
         border='orange', yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="Uniform", col='orange')
    curve(dunif(x, x3_min, x3_max), 
          col="darkred", cex=2, lwd=2, add=TRUE, yaxt="n")}, height=graph_ht)
  
  sample_ht <- 0.35
  mean_ht <- 0.45
  txt_offset <- .05
  pts <- reactive({data.frame(x=sample(x1(),input$n_samples),y=c(rep(sample_ht,input$n_samples)))})
  output$x4Plot <- renderPlot({ 
    hist(x1(), breaks = seq(x1_range_min()-1,x1_range_max()+1, by=bin_step()), ylim=c(0,max_density_ht),
         border='white', yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="", col='white')
    curve(dnorm(x, mean=x1_mu, sd=x1_sigma), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    points(pts(),col="darkblue")
    segments(pts()$x,0,pts()$x,sample_ht, col="darkblue")
    points(mean(pts()$x),mean_ht,col=trial_color,pch=19)
    segments(mean(pts()$x),0,mean(pts()$x),mean_ht, col=trial_color)
    text(mean(pts()$x),mean_ht + txt_offset,labels=c("mean"),col="black")
  }, height=graph_ht)
  
  x2_pts <- reactive({data.frame(x=sample(x2(),input$n_samples),y=c(rep(sample_ht,input$n_samples)))})
  output$x5Plot <- renderPlot({ 
    hist(x2(), breaks = seq(x2_range_min()-1,x2_range_max()+1, by=bin_step()), ylim=c(0,max_density_ht),
         border='white', yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="", col='white')
    curve(dlnorm(x, mean=x2_mu, sd=x2_sigma), 
          col="darkgreen", lwd=2, add=TRUE, yaxt="n")
    points(x2_pts(),col="darkgreen")
    segments(x2_pts()$x,0,x2_pts()$x,sample_ht, col="darkgreen")
    points(mean(x2_pts()$x),mean_ht,col=trial_color,pch=19)
    segments(mean(x2_pts()$x),0,mean(x2_pts()$x),mean_ht, col=trial_color)
    text(mean(x2_pts()$x),mean_ht + txt_offset,labels=c("mean"),col="black")
  }, height=graph_ht)
  
  
  x3_pts <- reactive({data.frame(x=sample(x3(),input$n_samples),y=c(rep(sample_ht,input$n_samples)))})
  output$x6Plot <- renderPlot({ 
    hist(x3(), breaks = seq(x3_range_min()-1,x3_range_max()+1, by=bin_step()), ylim=c(0,max_density_ht),
         border='white', yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="", col='white')
    curve(dunif(x, min=x3_min, max=x3_max), 
          col="darkred", lwd=2, add=TRUE, yaxt="n")
    points(x3_pts(),col="darkred")
    segments(x3_pts()$x,0,x3_pts()$x,sample_ht, col="orange")
    points(mean(x3_pts()$x),mean_ht,col=trial_color,pch=19)
    segments(mean(x3_pts()$x),0,mean(x3_pts()$x),mean_ht, col=trial_color)
    text(mean(x3_pts()$x),mean_ht + txt_offset,labels=c("mean"),col="black")
  }, height=graph_ht)
  
  
  sample_curve_height<-.87
  
  
  x4_pts <- reactive({
    replicate(input$k_trials,mean(sample(x1(),input$n_samples)))
  })
  output$x7Plot <- renderPlot({ 
    hist(x4_pts(), breaks = seq(x1_range_min()-1,x1_range_max()+1, by=bin_step()*1.4), ylim=c(0,sample_curve_height),
         border=trial_color, yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="", col=trial_color)
    curve(dnorm(x, mean=x1_mu, sd=x1_sigma/sqrt(input$n_samples)), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n",lty="dotted")
    curve(dnorm(x, mean=x1_mu, sd=x1_sigma), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
  }, height=graph_ht)
  
  mean_lnorm <- exp(x2_mu + (x2_sigma^2)/2)
  sigma_lnorm <- sqrt((exp(x2_sigma^2)-1)*(exp(2*x2_mu+x2_sigma^2)))
  
  x5_pts <- reactive({
    replicate(input$k_trials,mean(sample(x2(),input$n_samples)))
  })
  output$x8Plot <- renderPlot({ 
    hist(x5_pts(), breaks = seq(x2_range_min()-1,x2_range_max()+1, by=bin_step()*1.4), ylim=c(0,sample_curve_height),
         border=trial_color, yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="", col=trial_color)
    curve(dnorm(x, mean=mean_lnorm, sd=sigma_lnorm/sqrt(input$n_samples)), 
          col="darkgreen", lwd=2, add=TRUE, yaxt="n",lty="dotted")
    curve(dlnorm(x, mean=x2_mu, sd=x2_sigma), 
          col="darkgreen", lwd=2, add=TRUE, yaxt="n")
  }, height=graph_ht)
  
  mean_unif <- (x3_max+x3_min)/2
  sigma_unif <- sqrt(((x3_max-x3_min)^2)/12)
  
  x6_pts <- reactive({
    replicate(input$k_trials,mean(sample(x3(),input$n_samples)))
  })
  output$x9Plot <- renderPlot({ 
    hist(x6_pts(), breaks = seq(x3_range_min()-1,x3_range_max()+1, by=bin_step()*2.5), ylim=c(0,sample_curve_height),
         border=trial_color, yaxt='n', xlab = NULL, ylab=NULL, probability=TRUE, main="", col=trial_color)
    curve(dnorm(x, mean=mean_unif, sd=sigma_unif/sqrt(input$n_samples)), 
          col="darkgreen", lwd=2, add=TRUE, yaxt="n",lty="dotted")
    curve(dunif(x, min=x3_min, max=x3_max), 
          col="darkred", lwd=2, add=TRUE, yaxt="n")
  }, height=graph_ht)
  
}