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
library(tidyverse)
library(gcookbook)

slider_width <- 220
graph_height <- "60%"
graph_ht <- 260



####################################
#
# Define UI for application that draws the control page
#
ui <- fluidPage(
    tags$head(HTML(
      "<script>
      (function(i,s,o,g,r,a,m){
        i['GoogleAnalyticsObject']=r;i[r]=i[r]||
        function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
          a=s.createElement(o), m=s.getElementsByTagName(o)[0];
          a.async=1;
          a.src=g;m.parentNode.insertBefore(a,m)
        })
      (window, document, 'script',
        '//www.google-analytics.com/analytics.js','ga');
      
        ga('create', 'UA-151972923-1', 'auto');
        ga('send', 'pageview');
      
      </script>"
    )),
  tags$style(type = "text/css", "
             .irs-bar {width: 50px; height: 16px; background: lightGreen; border-top: 1px solid black; border-bottom: 1px solid black;} /* foreground box */
             .irs-bar-edge {background: black; border-left: 1px solid black; border-bottom: 1px solid black; border-top: 1px solid black; height: 16px; border-radius: 1px; width: 20px;}
             .irs-line {border: 1px solid black; height: 16px; border-radius: 1px; background: black} /* background box */
             .irs-grid-text {font-family: 'arial'; color: white; bottom: 22px; z-index: 1;} /* gridtext */
             .irs-grid-pol {display: none;}                 /* eliminate gridlines */
             .irs-min {visibility: hidden !important;}
             .irs-max {visibility: hidden !important;}
             .irs-from {visibility: hidden;}
             .irs-to {visibility: hidden; }
             .irs-single {color:black; top: 25px; z-index:2;} /* color and background of the selected number */
             .irs-slider {width: 17px; height: 20px; top: 23px;}
             "),
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, 
                  .js-irs-0 .irs-slider {background: #dddddd}
                  .js-irs-0 .irs-min:after {content: 'Samples (n)' !important;} 
                  .irs-min:after {visibility: visible !important; font-size: 13px;")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, 
                  .js-irs-1 .irs-slider {background: #dddddd}
                  .js-irs-1 .irs-min:after {content: 'Trials' !important;} 
                  .irs-min:after {visibility: visible !important; font-size: 13px;")),
  
  
  column(2,         
         fluidRow(
           div(style = "height:100px; text-align: center"),
           HTML("<html><H4><b>Demonstration of the <br>Central Limit Theorem</b></H4>
                </html>")
           ),
         fluidRow(
           div(style = "height:180px"),
           sliderInput("n_samples",  label=NULL, min=1, max=15, value=3, step = 2, width=slider_width)
         ),
         fluidRow(
           div(style = "height:180px"),
           sliderInput("k_trials",  label=NULL, min=10, max=100, value=10, step = 10, width=slider_width)
         )
         ),
  column(10,
         fluidRow(
           htmlOutput("model_info")
         ),
         fluidRow(
           column(1,
                  HTML("<style>
                       div#rotate-text {
                       -webkit-transform: rotate(-90deg);
                       -moz-transform: rotate(-90deg);
                       -o-transform: rotate(-90deg);
                       -ms-transform: rotate(-90deg);
                       transform: rotate(-90deg);
                  }
                       </style><html>
                       <table>
                       <tr style='height:260px'><td><div id='rotate-text'><b>Population</b></div></td></tr> 
                       <tr style='height:260px'><td> <div id='rotate-text'><b>Samples<br>& Mean</b></td> </tr> 
                       <tr style='height:260px'><td> <div id='rotate-text'><b><center>Sampling Distribution<br>of Mean</center></b></td>  </tr> 
                       </table></html>")
                  ),
           column(3,
                  plotOutput("x1Plot", height=graph_height),
                  plotOutput("x4Plot", height=graph_height),
                  plotOutput("x7Plot", height=graph_height)
           ),
           column(3,
                  plotOutput("x2Plot", height=graph_height),
                  plotOutput("x5Plot", height=graph_height),
                  plotOutput("x8Plot", height=graph_height)
           ),
           column(3,
                  plotOutput("x3Plot", height=graph_height),
                  plotOutput("x6Plot", height=graph_height),
                  plotOutput("x9Plot", height=graph_height)
           ))
           ))
