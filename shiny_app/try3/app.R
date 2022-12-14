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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  navbarPage(theme = shinythemes::shinytheme("superhero"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Food Waste Tracker</a>'), id="nav",
             windowTitle = "Food Waste Tracker",
             # in this tab, it needs its own plotOutput part
             tabPanel("Plot of loss percentage", fluid = TRUE,
                      sidebarPanel(helpText("Select a year or a range of years to observe change"),
                                   sliderInput(inputId = "year", label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1),
                                   helpText("Select any of the filters below to observe changes around the world"),
                                   selectInput(inputId = "country", label = "country",choices = unique(Food_waste$country),selected = "country"),
                                   selectInput(inputId = "commodity", label = "commodity",choices = unique(Food_waste$commodity),selected = "commodity")),
                      mainPanel( #plotOutput("distPlot"),
                        plotOutput("dotPlot")
                        )),
             # in this tab, it needs its own plotOutput part
             tabPanel("Heatmap of Available Data", fluid = TRUE,
                      sidebarPanel(helpText("Select a year or a range of years to observe change"),
                                   sliderInput(inputId = "year", label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1),
                                   helpText("Select any of the filters below to observe changes around the world"),
                                   selectInput(inputId = "country", label = "country",choices = unique(Food_waste$country),selected = "country"),
                                   selectInput(inputId = "commodity", label = "commodity",choices = unique(Food_waste$commodity),selected = "commodity")),),
             # in this tab, it needs its own plotOutput part
             tabPanel("Data", fluid = TRUE,
                      mainPanel( #plotOutput("distPlot"),
                        dataTableOutput("table")))
             ),
  
  # sidebarPanel(helpText("Select a year or a range of years to observe change"),
  #              sliderInput(inputId = "year", label = 'Year Range',min=1961,max=2021,value = c(1961,2021),step=1),
  #              helpText("Select any of the filters below to observe changes around the world"),
  #              selectInput(inputId = "country", label = "country",choices = unique(Food_waste$country),selected = "country"),
  #              selectInput(inputId = "commodity", label = "commodity",choices = unique(Food_waste$commodity),selected = "commodity")),
               #sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30)),
    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
        #sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
        #),

        # Show a plot of the generated distribution
        # mainPanel( #plotOutput("distPlot"),
        #           plotOutput("dotPlot"),
        #           dataTableOutput("table")
        # ) 
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
  
  # data_yr <- reactive({input$year_range})  
  # data_country <- reactive({input$country}) 
  # data_commodity <- reactive({input$commodity}) 
    
    output$dotPlot <- renderPlot ({
      
      data_yr <- input$year_range
      data_country <- input$country
      data_commodity <- input$commodity
      
      plot1 <- 
        ggplot(Food_waste, aes(x = year, y = mean_loss_percentage, color = country)) +
        geom_point(alpha = 0.3)+ geom_smooth(size=0.5, se = FALSE) + #se = false, is to remove the confidence bands
        ggtitle("Proportion of food waste since 1970") +
        xlab("Year") + ylab("Food waste by percentage")  #+ labs(color = "commodity")
      
      p1 <- plot1 + guides(color = FALSE) #+ scale_x_continuous(, 10)
      #plot_ly(Food_waste,  x = ~year, y = ~mean_loss_percentage, color = ~country ) %>% add_markers()
      
      #fig1 <- plotly::ggplotly(p1)
      #plotly::ggplotly(p1
      
      p1
    })
    
    br()
    
    # this one works, but it only shows one country at a time...
    output$table <- renderDataTable({
      country_filter <- subset(Food_waste, Food_waste$country == input$country)
      #country_filter <- subset(Food_waste, Food_waste$country == data_country())
      #year_filter <- subset(Food_waste, Food_waste$year == input$year)
      #year_filter <- subset(Food_waste, Food_waste$year == data_yr()) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#3pp qst 1
