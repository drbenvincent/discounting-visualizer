#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

defaultDA <- 180
defaultRA <- 50
defaultDB <- 365
defaultRB <- 100
defaultK <- 0.003


exponentialDiscountFraction <- function(k, delays){
  return(exp(-k*delays))
}

ylimDemoPlot <- function(RA,RB){
  if ( RA>0 & RB>0) {
    ylim <- c(0,max(RA,RB))
  } else if ( RA<0 & RB<0) {
    ylim <- c(min(RA,RB), 0)
  } else if ( RA<0 & RB>0) {
    ylim <- c(RA,RB)
  } else if ( RA>0 & RB<0) {
    ylim <- c(RB,RA)
  }
  return(ylim)
}

max_delay <- 365+7


ui <- fixedPage(
  # TOP ROW WITH INPUTS
  fixedRow(
    column(width = 4,
           h2("Exponential Discounting"),
           HTML("<p>Welcome to the exponential demo thing. Written by Benjamin T. Vincent of <a href='http://www.inferenceLab.com'>inferenceLab</a>.</p>"),
           wellPanel(sliderInput(inputId = "k",
                       label = "discounting parameter, k",
                       min = 0,
                       max = 0.01,
                       value = defaultK,
                       step = 0.0001)
                     )
           ),
    column(width = 4,
           h4("Sooner reward"),
           wellPanel(numericInput(inputId = "RA",
                           label = "reward size", 
                           value = defaultRA),
                     sliderInput(inputId = "DA",
                          label = "delay (days)",
                          min = 0,
                          max = 365+7,
                          value = defaultDA)
                     )
           ),
    column(width = 4,
           h4("Later reward"),
           wellPanel(numericInput(inputId = "RB",
                 label = "reward size", 
                 value = defaultRB),
              sliderInput(inputId = "DB",
                label = "delay (days)",
                min = 0,
                max = 365+7,
                value = defaultDB)
              )
           ),
  # NEW ROW SHOWING PLOTS
  fixedRow(
    column(width = 6,
           h2("Discount function"),
           p("The discount function is a proposed cognitive model. Personally, I see discount functions as descriptive cognitive models, not as truely explanatory."),
           withMathJax(
             helpText('$$\\mathrm{discount~fraction}(k,delay)= \\exp(-k.delay)$$')
           ),
           plotOutput("discountFunctionPlot")
    ),
    column(width = 6,
           h2("Example decision"),
           p("This plot shows the internal subjective value (y-axis) of the rewards (circles) as a function of time from now."),
           withMathJax(
             helpText('$$\\mathrm{subjective~value}(reward,delay,k)= reward \\times \\mathrm{discount~fraction}(k,delay) $$')
           ),
           plotOutput("examplePlot")
    )
  )
)
)





server <- function(input, output){
  # DISCOUNT FUNCTION
  output$discountFunctionPlot <- renderPlot(
    curve( exponentialDiscountFraction(input$k, x),
           from = 0,
           to = max_delay,
           n = 1001,
           xlab = "Delay (days)",
           ylab = "Discount fraction",
           xlim = c(0,max_delay),
           ylim = c(0,1)))
  
  # EXAMPLE DECISION PLOT
  output$examplePlot <- renderPlot({
    # delayed reward
    curve( exponentialDiscountFraction(input$k, input$DB-x) * input$RB,
           from = 0,
           to = input$DB,
           n = 1001,
           xlab = "Time from now (days)",
           ylab = "subjective value",
           xlim = c(0,max_delay),
           ylim = ylimDemoPlot(input$RA,input$RB)
    )
    
    # immediate reward
    curve( exponentialDiscountFraction(input$k, input$DA-x) * input$RA,
           from = 0,
           to = input$DA,
           n = 1001,
           add = TRUE)
    lines(x=c(input$DA,input$DA),
         y=c(0,input$RA))
    
    points(x=c(input$DA,input$DB),
           y=c(input$RA,input$RB),
           pch = 19,
           cex = 1,
           lwd = 2,
           add = TRUE)
    lines(x=c(input$DB,input$DB),
          y=c(0,input$RB))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
