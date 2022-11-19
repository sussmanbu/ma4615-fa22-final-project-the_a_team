#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Final project
### Author: Italo Duran,.....,.....,.....,.....
### Email: duran01@bu.edu

#if (!requireNamespace("BiocManager", quietly= TRUE))
#install.packages("BiocManager")
library(BiocManager)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(colourpicker)# you might need to install this.
#library(DT)# you might need to install this.
#BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)# you might need to install this.
#BiocManager::install("DESeq2")
#library(igraph)# you might need to install this.
#library(bslib)


###############################################################
#####Visual interface - Front End Part#########################
ui = fluidPage(theme = shinythemes::shinytheme("journal"),
               titlePanel("Final Project"),
               h3("By: Italo Duran"),
               h4(HTML("<b>Visualization analysis of:</b><br> mRNA-Seq Expression profiling of human post-mortem BA9 brain tissue for<br>Huntington's Disease and neurologically normal individuals data set...")),
               tags$a(href="https://github.com/imd9/Final-Project-BF591-R",target="_blank",rel="noopener noreferrer","Click here to see the projects 'Github' files!"), br(),br(),
               #This part is where we build the page layout and how it's going to look
               # here is the panel for the left side
               sidebarPanel(sliderInput(inputId = 'Year Range', label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1),
                            selectInput(inputId = "Aggregation", label = "Aggregation",
                              choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            selectInput(inputId = "Aggregation_Options", label = "Aggregation Options",
                                        choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            selectInput(inputId = "Country", label = "Country",
                                        choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            selectInput(inputId = "Basket_item", label = "Basket Item",
                                        choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            selectInput(inputId = "Commodity", label = "Commodity (CPC 2.0)",
                                        choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            selectInput(inputId = "Value_Chain_Stage", label = "Value Chain Stage(s)",
                                        choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            selectInput(inputId = "method_data_collect", label = "Method Of Data Collection",
                                        choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            checkboxInput(inputId = "top_SGD_basket", label = "Select to keep only the top 10 SGD basket items",)),
               
               ## here are the tabs for the right side
               tabsetPanel(
                 tabPanel("Plot of loss percentage", fluid = TRUE),
                 tabPanel("Heatmap of Available Data", fluid = TRUE),
                 tabPanel("Boxplot by Stage", fluid = TRUE),
                 tabPanel("Data", fluid = TRUE)))
                
##################################server##############################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
