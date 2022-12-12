#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
#if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
#if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
#if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
#if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
#if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
#if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
#if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
#if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
#if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

load_pkg <- rlang::quos(shiny,shinythemes,tidyverse,plotly,lubridate,magrittr,rvest,readxl,dplyr,maps,reshape2,ggiraph,RColorBrewer,leaflet,geojsonio,janitor,shinythemes,shinydashboard,shinyWidgets)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE))

##########################################################
# Load data
Food_waste <- read_csv(here::here("dataset", "FoodLossandWasteAll.csv"))
Food_production <- read_csv(here::here("dataset-ignore", "Production_All(Normalized).csv"))
GDP_dat <- read_csv(here::here("dataset", "GDP_data_clean.csv"))
GDP_dat_macro <- read_csv(here::here("dataset", "GDP_data_clean_macro.csv"))
#population_dat <- read_csv(here::here("dataset", "Population_data_clean_macro.csv"))
#AgriGDP_data_clean_macro <- read_csv(here::here("dataset", "AgriGDP_data_clean_macro.csv"))


#ggiraph,maps,geojsonio,readxl,janitor,leaflet,RColorBrewer,shinythemes,shinydashboard,shinyWidgets
### SHINY UI ###
ui <- bootstrapPage(
  #tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinythemes::shinytheme("superhero"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Food Waste Tracker</a>'), id="nav",
             windowTitle = "Food Waste Tracker",
             
             
             sidebarPanel(sliderInput(inputId = 'Year Range', label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1),
                          selectInput(inputId = "type_eda", label = "E.D.A. Categories ",choices = unique(Food_waste$type_eda),selected = "Country")),
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
             
             
             #tabPanel("Food Waste mapper", div(class="outer",tags$head(includeCSS("styles.css")),
                          #leafletOutput("mymap", width="100%", height="100%") #,
                          
                          # absolutePanel(id = "controls", class = "panel panel-default",
                          #               top = 75, left = 55, width = 250, fixed=TRUE,
                          #               draggable = TRUE, height = "auto",
                          #               
                          #               span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                          #               h3(textOutput("reactive_case_count"), align = "right"),
                          #               h4(textOutput("reactive_death_count"), align = "right"),
                          #               h6(textOutput("clean_date_reactive"), align = "right"),
                          #               h6(textOutput("reactive_country_count"), align = "right"),
                          #               plotOutput("epi_curve", height="130px", width="100%"),
                          #               plotOutput("cumulative_plot", height="130px", width="100%") #,
                                        
                                        # sliderTextInput("plot_date",
                                        #                 label = h5("Select mapping date"),
                                        #                 choices = format(unique(cv_cases$date), "%d %b %y"),
                                        #                 selected = format(current_date, "%d %b %y"),
                                        #                 grid = FALSE,
                                        #                 animate=animationOptions(interval = 3000, loop = FALSE))) 
                                        #)),
             
               tabPanel("Plot of loss percentage", fluid = TRUE),
               tabPanel("Heatmap of Available Data", fluid = TRUE),
               tabPanel("Data", fluid = TRUE)))

####################################################################################


# tabPanel("COVID-19 mapper",
#          div(class="outer",
#              tags$head(includeCSS("styles.css")),
#              leafletOutput("mymap", width="100%", height="100%"),
#              
#              absolutePanel(id = "controls", class = "panel panel-default",
#                            top = 75, left = 55, width = 250, fixed=TRUE,
#                            draggable = TRUE, height = "auto",
#                            
#                            span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
#                            h3(textOutput("reactive_case_count"), align = "right"),
#                            h4(textOutput("reactive_death_count"), align = "right"),
#                            h6(textOutput("clean_date_reactive"), align = "right"),
#                            h6(textOutput("reactive_country_count"), align = "right"),
#                            plotOutput("epi_curve", height="130px", width="100%"),
#                            plotOutput("cumulative_plot", height="130px", width="100%"),
#                            
#                            sliderTextInput("plot_date",
#                                            label = h5("Select mapping date"),
#                                            choices = format(unique(cv_cases$date), "%d %b %y"),
#                                            selected = format(current_date, "%d %b %y"),
#                                            grid = FALSE,
#                                            animate=animationOptions(interval = 3000, loop = FALSE))),
#              
#              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
#                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
#              
#              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
#                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
#                                         onclick = sprintf("window.open('%s')", 
#                                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
#              
#              
#          )
# )


####################################################################################


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  draw_table <- function(dataf, slider) {
    dataf %>%
      arrange(pvalue) %>% #took dataframe and arranged the pvalues
      filter(padj < 10^slider) %>% #filtered for the p-adjusted with the slider
      mutate(pvalue = formatC(.$pvalue, digits = 3, format = "g" ), # Used mutate to format both pvals & p_adjusted. digits(for numbr of decimal spaces)
             padj = formatC(.$padj, digits = 3, format = "g")) %>% # format can also use: d(integers), f,e,E,G,fg(real numbers), s(strings) 
      return() }
  
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

#' These outputs aren't really functions, so they don't get a full skeleton, 
#' but use the renderPlot() and renderTabel() functions to return() a plot 
#' or table object, and those will be displayed in your application.
#' here we load the fields from the server and UI to connect it between them so it gives us an ouput on the app.

#output$table <- renderTable(draw_table(dataf = load_data() )) # replace this NULL




###### back up copy of og server code

# server <- function(input, output) {
#   
#   output$distPlot <- renderPlot({
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white',
#          xlab = 'Waiting time to next eruption (in mins)',
#          main = 'Histogram of waiting times')
#   })
# }

#######

# Run the application 
shinyApp(ui = ui, server = server)

