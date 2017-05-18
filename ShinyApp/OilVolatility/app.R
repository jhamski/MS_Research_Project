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
         plotOutput("CPTplot"),
         plotOutput("Results")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

   
   output$CPTplot <- renderPlot({
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
   
   
   output$Results <- renderPlot({
     start = as.POSIXct(input$dateRange[1])
     end = as.POSIXct(input$dateRange[2])
     
     wti.ts.range <- wti.xts[paste(start, end, sep="::")] %>% as.ts()
     
     cpt <- cpt.mean(wti.ts.range, 
                     method="PELT", 
                     penalty = "Manual", 
                     pen.value = input$penalty, 
                     minseglen = input$minseg)
     
     regimes <- cpt@cpts
     regime.index <- NULL
     
     for (i in 1:length(regimes)){
       ifelse(i == 1, 
              regime.index.iter <- rep.int(i, times = length(1:regimes[i])), 
              regime.index.iter <- rep.int(i, times = length((regimes[i-1]+1):regimes[i])))
       regime.index <- c(regime.index, regime.index.iter)
     }
     
     wti.regimes <- cbind(wti.ts.range, regime.index) %>% as.data.frame()
     colnames(wti.regimes) <- c("close", "regime")
     wti.regimes$regime <- as.factor(wti.regimes$regime)
     
     price.regime.descriptive <- wti.regimes %>%
       group_by(regime) %>% 
       summarize(med = median(close), sd = sd(close))
     
     plot(x = price.regime.descriptive$med, y = price.regime.descriptive$sd, type = "p")
     abline(lm(sd ~ med, data = price.regime.descriptive), col = "red")
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

