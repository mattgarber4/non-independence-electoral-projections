library(myUtils) # to use, run devtools::install_github("mattgarber4/myUtils")
library(shiny)
library(Rcpp)
library(ggplot2)
library(fiftystater)
library(shinyWidgets)
library(aws.s3)
library(shape)

source("simulation_utils.R")
simMap <- if(Sys.info()[['nodename']] == "MG-XPS-15") { 
                 readRDS("simsLazy.rds")
          } else {
                 s3readRDS(object = "simsLazy.rds", 
                           bucket = "s3://mgelectsimlazy", 
                           region = 'us-east-2')
          }

ui <- fluidPage(
   
   # Application title
   titlePanel("Election Projection Bias"),
   
   fluidRow(
       column(
           width = 1, 
           noUiSliderInput(
               inputId = "coef", 
               label = "Dependence",
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
               height = "300px"
               )
           ),
       column(
           width = 10, 
           tabsetPanel(
              tabPanel(
                 "Electoral College",
                 strong(div(br(), 'Distribution of Electoral College Votes', align = 'center')),
                 plotOutput("distPlot")
              ),
              tabPanel(
                 "State Win Percents",
                 strong(div(br(), 'Share of Simulated Wins by State', align = 'center')),
                 plotOutput("winPct")
              ),
              tabPanel(
                 "Map",
                 strong(div(br(), "States Shaded by Share of Simulations Won", align = 'center')),
                 plotOutput("map")
              )
              ,
              tabPanel(
                 "Vote Share",
                 strong(div(br(), 'Average Electoral College Votes by Dependence Coefficient', align = 'center')),
                 plotOutput("voteShare")
              )
           ),
           fluidRow(
               div(align = "center",
               noUiSliderInput(
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
                   width = "500px"
                   ))
               )
           )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   output$distPlot <- renderPlot({
      sim <<- simMap$get(input$coef / 100, input$bias / 100)
      sim$plot(main = F, sub = paste0("Dems win ", round(100 * sim$winPct, 1), "% of elections",
                       " | Dems average ", round(sim$average, 1), " electoral votes"))
   })
   output$winPct <- renderPlot({
      sim <<- simMap$get(input$coef / 100, input$bias / 100)
      sim$plotMeans()
   })
   output$map <- renderPlot({
      sim <<- simMap$get(input$coef / 100, input$bias / 100)
      sim$map(colorMapper)
   })
   output$voteShare <- renderPlot({
      plotMeanLine(simMap, input$bias / 100)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

