#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Final project
### Author: The A Team
### Email: duran01@bu.edu

#if (!requireNamespace("BiocManager", quietly= TRUE))
#install.packages("BiocManager")
library(BiocManager)
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(tidyverse)
library(countrycode)
library(colourpicker)# you might need to install this.
#library(DT)# you might need to install this.
#BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)# you might need to install this.
#BiocManager::install("DESeq2")
#library(igraph)# you might need to install this.
#library(bslib)

##########################################################
# Load data
Food_waste <- read_csv(here::here("dataset", "FoodLossandWasteAll.csv"))
Food_production <- read_csv(here::here("dataset-ignore", "Production_All(Normalized).csv"))
GDP_dat <- read_csv(here::here("dataset", "GDP_data_clean.csv"))
GDP_dat_macro <- read_csv(here::here("dataset", "GDP_data_clean_macro.csv"))
#population_dat <- read_csv(here::here("dataset", "Population_data_clean_macro.csv"))
#AgriGDP_data_clean_macro <- read_csv(here::here("dataset", "AgriGDP_data_clean_macro.csv"))




###############################################################
#####  UI - Visual interface - Front End Part#########################
ui = fluidPage(theme = shinythemes::shinytheme("darkly"),
               titlePanel("Final Project"),
               h3("By: Italo Duran"),
               h4(HTML("<b>Visualization analysis of:</b><br> mRNA-Seq Expression profiling of human post-mortem BA9 brain tissue for<br>Huntington's Disease and neurologically normal individuals data set...")),
               tags$a(href="https://github.com/sussmanbu/ma4615-fa22-final-project-the_a_team",target="_blank",rel="noopener noreferrer","Click here to see the projects 'Github' files!"), br(),br(),
               #This part is where we build the page layout and how it's going to look
               # here is the panel for the left side
               sidebarPanel(sliderInput(inputId = 'Year Range', label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1),
                            selectInput(inputId = "type_eda", label = "E.D.A. Categories ",choices = unique(Food_waste$type_eda),selected = "Country")),
                            # selectInput(inputId = "Aggregation_Options", label = "Aggregation Options",
                            #             choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            # selectInput(inputId = "Country", label = "Country",
                            #             choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            # selectInput(inputId = "Basket_item", label = "Basket Item",
                            #             choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            # selectInput(inputId = "Commodity", label = "Commodity (CPC 2.0)",
                            #             choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            # selectInput(inputId = "Value_Chain_Stage", label = "Value Chain Stage(s)",
                            #             choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            # selectInput(inputId = "method_data_collect", label = "Method Of Data Collection",
                            #             choices = c("All" = "malaria_tot", "0-4 yrs" = "malaria_rdt_0-4","5-14 yrs" = "malaria_rdt_5-14","15+ yrs" = "malaria_rdt_15")),
                            # checkboxInput(inputId = "top_SGD_basket", label = "Select to keep only the top 10 SGD basket items",)),
               
               ## here are the tabs for the right side
               tabsetPanel(
                 tabPanel("Plot of loss percentage", fluid = TRUE),
                 tabPanel("Heatmap of Available Data", fluid = TRUE),
                 tabPanel("Data", fluid = TRUE)))
                


#################################

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

#################################
#####' load_Data ##########
# load_data <- reactive({
#   req(input$file1) #this function makes the program not run until a file is given 
#   f=input$file1
#   if (is.null(f)){return(NULL)} #if there's no file then return nothing
#   else{datafile=read.csv(f$datapath, header= TRUE, sep=",")}%>% 
#     rename(Gene = X)%>% #once the line above is executed, this renames the X values as Gene 
#     return() }) #datafile
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# 

#######################################################

  ########' Volcano plot ################
  volcano_plot <- function(dataf, x_name, y_name, slider, color1, color2) {
    p<-ggplot(data = dataf,aes(x = !!sym(x_name), y=-log10(!!sym(y_name)))) + 
      geom_point(aes(color = padj< 1*10^(slider)))+
      labs( color = str_glue('{y_name} 1 x 10^ {slider}'))+ #for labels to follow the selected button,like f' in python.
      scale_color_manual(values = c(color1, color2)) + theme_classic() +
      theme(panel.background = element_rect(fill = "grey23"),
            panel.border = element_blank(),
            panel.grid.major = element_line(color = "grey35"),
            panel.grid.minor = element_line(color = "grey25"),
            plot.background = element_rect(fill = "grey75",colour="grey75"),
            plot.margin = margin(0.5, 0.5, 0.3, 0.5, "cm"),
            legend.key = element_rect(fill = "grey81", colour="grey81"),
            legend.background = element_rect(fill = "grey75"),
            legend.position = "bottom" ) # colour = "black",size = 25, size = 1,theme_classic(), plot.margin = margin(2, 2, 2, 2, "cm")
    # plot.background = element_rect(fill = "grey30")
    return(p) }
  #############' Draw and filter table ##############
  draw_table <- function(dataf, slider) {
    dataf %>%
      arrange(pvalue) %>% #took dataframe and arranged the pvalues
      filter(padj < 10^slider) %>% #filtered for the p-adjusted with the slider
      mutate(pvalue = formatC(.$pvalue, digits = 3, format = "g" ), # Used mutate to format both pvals & p_adjusted. digits(for numbr of decimal spaces)
             padj = formatC(.$padj, digits = 3, format = "g")) %>% # format can also use: d(integers), f,e,E,G,fg(real numbers), s(strings) 
      return() }
  #' These outputs aren't really functions, so they don't get a full skeleton, 
  #' but use the renderPlot() and renderTabel() functions to return() a plot 
  #' or table object, and those will be displayed in your application.
  #' here we load the fields from the server and UI to connect it between them so it gives us an ouput on the app.
  output$volcano <- renderPlot(volcano_plot(dataf = load_data(), slider = input$slider_p, x_name=input$button1, y_name=input$button2, color1=input$col1, color2=input$col2)) # replace this NULL
  # Same here, just return the table as you want to see it in the web page
  output$table <- renderTable(draw_table(dataf = load_data(), slider = input$slider_p)) # replace this NULL
}
# Run the application
shinyApp(ui = ui, server = server)

