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
         sliderInput("minseg",
                     "Minimum Number of Bins:",
                     min = 30,
                     max = 1000,
                     value = 250),
         
         sliderInput("penalty",
                     "PELT Penalty:",
                     min = 0,
                     max = 10000,
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
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$Plot <- renderPlot({
     cpt <- cpt.mean(wti.ts, 
                     method="PELT", 
                     penalty = "Manual", 
                     pen.value = input$penalty, 
                     minseglen = input$minseg)
     
     plot(cpt, , cpt.width = 5)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

