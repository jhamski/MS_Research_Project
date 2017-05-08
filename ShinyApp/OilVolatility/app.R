#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(changepoint)

load("wti_project.Rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Changepoint Parameterization of Oil Price Regimes"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateRangeInput('dateRange',
                       label = 'Date range input: yyyy-mm-dd',
                       start = "1986-01-02", end = "2016-12-30"),
        
         sliderInput("minseg",
                     "Minimum Number of Days in Regime:",
                     min = 30,
                     max = 750,
                     value = 250),
         
         sliderInput("penalty",
                     "PELT Penalty:",
                     min = 0,
                     max = 20000,
                     value = 10000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

   
   output$Plot <- renderPlot({
     start = as.POSIXct(input$dateRange[1])
     end = as.POSIXct(input$dateRange[2])
     
     wti.ts.range <- wti.xts[paste(start, end, sep="::")] %>% as.ts()
     
     cpt <- cpt.mean(wti.ts.range, 
                     method="PELT", 
                     penalty = "Manual", 
                     pen.value = input$penalty, 
                     minseglen = input$minseg)
     
     plot(cpt, cpt.width = 5)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

