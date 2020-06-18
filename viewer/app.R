library(myUtils) # to use, run devtools::install_github("mattgarber4/myUtils")
library(shiny)
library(Rcpp)
library(ggplot2)
library(fiftystater)
library(shinyWidgets)
library(aws.s3)
library(shape)
library(dplyr)

source("simulation_utils.R")
simMap <- if(Sys.info()[['nodename']] == "MG-XPS-15") { 
   readRDS("simsLazy.rds")
} else {
   s3readRDS(object = "simsLazy.rds", 
             bucket = "s3://mgelectsimlazy", 
             region = 'us-east-2')
}

ui <- fluidPage(
   includeCSS("st.css"),
   # Application title
   titlePanel("Dependence in Election Forecasts"),
   
   fluidRow(
      column(
         width = 1, 
         style = 'padding-top:5%',
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
         ),
         tags$div(
            align = "center",
            span("Toggle Party", style = "color:gray;font-style:italic"),
            switchInput("as.dem",
                        value = T,
                        onLabel = "Dem",
                        offLabel = "Rep")
         
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
               strong(div(br(), 'Electoral College Votes & Win Probability by Dependence Coefficient', align = 'center')),
               div('50 percent of modelled elections fall in shaded region', style = 'color:grey', align='center'),
               plotOutput("voteShare")
            )
         ),
         fluidRow(
            div(align = "center",
                noUiSliderInput(
                   inputId = "bias", 
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
                ),
                br(),
                strong(textOutput("biasTxt"))
            )
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   biasMult <- reactive({
      if (input$as.dem) {
         1
      } else {
         -1
      }
   })
   
   winPct <- function(as.dem, demPct) {
      if (as.dem) {
         demPct
      } else {
         1 - demPct
      }
   }
   
   ecAvg <- function(as.dem, demAvg) {
      if (as.dem) {
         demAvg
      } else {
         538 - demAvg
      }
   }
   
   output$distPlot <- renderPlot({
      sim <<- simMap$get(input$coef / 100, biasMult() * input$bias / 100)
      party <- c("Democrats", "Republicans")[biasMult()]
      sim$plot(main = F, as.dem = input$as.dem, 
               sub = paste0(party, " win ", round(100 * winPct(input$as.dem, sim$winPct), 1), "% of elections",
                                      " | ", party, " average ", round(ecAvg(input$as.dem, sim$average), 1), " electoral votes"))
   })
   output$winPct <- renderPlot({
      sim <<- simMap$get(input$coef / 100, biasMult() * input$bias / 100)
      sim$plotMeans(as.dem = input$as.dem)
   })
   output$map <- renderPlot({
      sim <<- simMap$get(input$coef / 100, biasMult() * input$bias / 100)
      sim$map(colorMapper)
   })
   output$voteShare <- renderPlot({
      plotMeanLine(simMap, biasMult() * input$bias / 100, as.dem = input$as.dem, highlighted = input$coef / 100)
   })
   output$biasTxt <- renderText({
      paste0("Bias Towards ", ifelse(input$as.dem, "Democrats", "Republicans"))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

