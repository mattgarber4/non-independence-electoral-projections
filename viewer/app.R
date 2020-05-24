library(myUtils)
library(shiny)
library(Rcpp)
library(ggplot2)
library(fiftystater)
library(shinyWidgets)
library(pryr)
library(dplyr)

source("simulation_utils.R")
simMap <- readRDS("sims.rds")

ui <- fluidPage(
   
   # Application title
   titlePanel("Election Projection Bias"),
   
   sidebarPanel(noUiSliderInput(
       inputId = "bias", 
       label = "Bias Towards Dems",
       range = list(
           min = 100 * c(-1, .5),
           "10%" = 100 * c(-.5, .25),
           "15%" = 100 * c(-.25, .15),
           "20%" = 100 * c(-.1, .025),
           "30%" = 100 * c(-.05, .01),
           "70%" = 100 * c(.05, .025),
           "80%" = 100 * c(.1, .15),
           "85%" = 100 * c(.25, .25),
           "90%" = 100 * c(.5, .5),
           max = 100 * 1
       ),
       pips = list(
           mode = "values",
           values = 100 * c(-1, -.5, -.1, -.05, -.02, 0, .02, .05, .1, .5, 1),
           density = 5
           
       ),
       behaviour = "snap",
       format = wNumbFormat(decimals = 1, suffix = "%"),
       value = 0,
       direction = "ltr",
       connect = F,
       update_on = "end",
       tooltips = T,
       orientation = "horizontal",
       width = "300px"),
   
   noUiSliderInput(
       inputId = "coef", 
       label = "Coeficient of Dependence",
       range = list(
           min = 100 * c(0, .0001),
           "10%" = 100 * c(.001, .0005),
           "60%" = 100 * c(.01, .005),
           "70%" = 100 * c(.05, .01),
           "80%" = 100 * c(.1, .1),
           "90%" = 100 * c(1, 1),
           max = 100 * 4
       ),
       pips = list(
           mode = "values",
           values = 100 * c(0, .01, .05, .1, 1, 4),
           density = 5
           
       ),
       behaviour = "snap",
       update_on = "end",
       format = wNumbFormat(decimals = 2, suffix = "%"),
       value = 0,
       direction = "rtl",
       tooltips = T,
       orientation = "vertical",
       connect = F,
       height = "300px")),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
   output$distPlot <- renderPlot({
       sim <- simMap$get(input$coef / 100, input$bias / 100)
       sim$plot(paste0("Dems win ", round(100 * sim$winPct, 1), "% of elections",
                       " | Dems average ", round(sim$average, 1), " electoral votes"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

