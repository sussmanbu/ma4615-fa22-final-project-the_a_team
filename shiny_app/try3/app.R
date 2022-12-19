#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

load_pkg <- rlang::quos(shiny,shinythemes,tidyverse,plotly,lubridate,magrittr,rvest,readxl,dplyr,maps,reshape2,ggiraph,RColorBrewer,leaflet,geojsonio,janitor,shinythemes,shinydashboard,shinyWidgets)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE))

##########################################################
# Load data
Food_waste <- read_csv(here::here("dataset", "FoodLossandWasteAllClean.csv"))
Food_production <- read_csv(here::here("dataset-ignore", "Production_All(Normalized).csv"))
GDP_dat <- read_csv(here::here("dataset", "GDP_data_clean.csv"))
GDP_dat_macro <- read_csv(here::here("dataset", "GDP_data_clean_macro.csv"))
#population_dat <- read_csv(here::here("dataset", "Population_data_clean_macro.csv"))
#AgriGDP_data_clean_macro <- read_csv(here::here("dataset", "AgriGDP_data_clean_macro.csv"))

#ggiraph,maps,geojsonio,readxl,janitor,leaflet,RColorBrewer,shinythemes,shinydashboard,shinyWidgets
###################################################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
  navbarPage(theme = shinythemes::shinytheme("superhero"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Food Waste Tracker</a>'), id="nav",
             windowTitle = "Food Waste Tracker",
             # in this tab, it needs its own plotOutput part
             tabPanel("Plot of loss percentage", fluid = TRUE,
                      sidebarPanel(helpText("Select a year or a range of years to observe change"),
                                   sliderInput(inputId = "year_range", label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1), #
                                   helpText("Select any of the filters below to observe changes around the world"),
                                   selectInput(inputId = "country", label = "country",choices = c("world",unique(Food_waste$country)),selected = "country"),
                                   selectInput(inputId = "commodity", label = "commodity",choices = c("world",unique(Food_waste$commodity)),selected = "commodity")),
                      mainPanel(#plotOutput(outputId ="dotPlot"), 
                                #br(),
                                plotlyOutput(outputId ="dotPlot"),
                                br(),
                                dataTableOutput(outputId ="table")
                        ))) )
###################################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
  # This section is to run the graph above but with plotly:
  # we had to make filters for all possible selectors for the tab window but to also show the world data:
  output$dotPlot <- renderPlotly({
    if (input$country == "world" & input$commodity == "world"){data_yr <- Food_waste %>% filter(year >= input$year_range[1],
                                                                                                year <= input$year_range[2])}
    if (input$country == "world" & input$commodity != "world"){data_yr <- Food_waste %>% filter( commodity == input$commodity,
                                                                                                 year >= input$year_range[1],
                                                                                                 year <= input$year_range[2])}
    if (input$country != "world" & input$commodity == "world"){data_yr <- Food_waste %>% filter(country == input$country,
                                                                                                year >= input$year_range[1],
                                                                                                year <= input$year_range[2])}
    if (input$country != "world" & input$commodity != "world"){data_yr <- Food_waste %>% filter(country == input$country,
                                                                                                commodity == input$commodity,
                                                                                                year >= input$year_range[1],
                                                                                                year <= input$year_range[2])}

    plot1 <- ggplot(data_yr,aes(x = year, y = mean_loss_percentage, color = country)) + 
      geom_point(alpha = 0.3) + geom_smooth(size=0.5, se = FALSE) + ggtitle("Proportion of food waste since 1970") + 
      xlab("Year") + ylab("Food waste by percentage")
    
    p1 <- plot1 + guides(color = FALSE) + scale_x_continuous(, 10)
    
    p1
    
   })
  #   
# This section is to run the graph above but w/o plotly:
  # output$dotPlot <- renderPlot({
  #   
  #   if (input$country == "world" & input$commodity == "world"){data_yr <- Food_waste %>% filter(year >= input$year_range[1],
  #                                                                                                 year <= input$year_range[2])}
  #   
  #   if (input$country == "world" & input$commodity != "world"){data_yr <- Food_waste %>% filter( commodity == input$commodity,
  #                                                                                                 year >= input$year_range[1],
  #                                                                                                 year <= input$year_range[2])}
  #   
  #   if (input$country != "world" & input$commodity == "world"){data_yr <- Food_waste %>% filter(country == input$country,
  #                                                                                                 year >= input$year_range[1],
  #                                                                                                 year <= input$year_range[2])}
  #   
  #   if (input$country != "world" & input$commodity != "world"){data_yr <- Food_waste %>% filter(country == input$country,
  #                                                                                                 commodity == input$commodity,
  #                                                                                                 year >= input$year_range[1],
  #                                                                                                 year <= input$year_range[2])}
  # 
  #   plot1 <- ggplot(data_yr,aes(x = year, y = mean_loss_percentage, color = country)) +
  #     geom_point(alpha = 0.3) + geom_smooth(size=0.5, se = FALSE) + ggtitle("Proportion of food waste since 1970") + 
  #     xlab("Year") + ylab("Food waste by percentage")
  # 
  #   p1 <- plot1 + guides(color = FALSE) + scale_x_continuous(, 10)
  # 
  #   p1
  # 
  # })

# This section is for the interactive table:
  # we had to make filters for all possible selectors for the tab window but to also show the world data:
   output$table <- renderDataTable({
     
     if (input$country == "world" & input$commodity == "world"){data_yr <- Food_waste %>% filter(year >= input$year_range[1], 
                                                                                                 year <= input$year_range[2])}
     if (input$country == "world" & input$commodity != "world"){data_yr <- Food_waste %>% filter( commodity == input$commodity, 
                                                                                                  year >= input$year_range[1], 
                                                                                                  year <= input$year_range[2])}
     if (input$country != "world" & input$commodity == "world"){data_yr <- Food_waste %>% filter(country == input$country, 
                                                                                                 year >= input$year_range[1], 
                                                                                                 year <= input$year_range[2])}
     if (input$country != "world" & input$commodity != "world"){data_yr <- Food_waste %>% filter(country == input$country, 
                                                                                                 commodity == input$commodity, 
                                                                                                 year >= input$year_range[1], 
                                                                                                 year <= input$year_range[2])}
     data_yr

  }, options = list(pageLength = 10))
  
    
}
# Create Shiny app ----
# Run the application 
shinyApp(ui = ui, server = server)
