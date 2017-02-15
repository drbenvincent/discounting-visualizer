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
defaultM <- -0.2
defaultC <- -3

hyperbolicDiscountFraction <- function(m, c, delays, reward){
  k <- magnitudeEffect(m, c, reward)
  return(1/(1+k*delays))
}

magnitudeEffect <- function(m, c, reward){
  # Calculate k for a given (m, c, reward)
  return( exp(m*log(reward)+c) )
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
  column(width = 12,
         h2("Hyperbolic Discounting - with Magnitude Effect"),
         HTML("<p>Welcome to the discounting demo thing. Written by Benjamin T. Vincent of <a href='http://www.inferenceLab.com'>inferenceLab</a>. For more information see my paper, Vincent, B. T. (2016). <a href='http://link.springer.com/article/10.3758%2Fs13428-015-0672-2'>Hierarchical Bayesian estimation and hypothesis testing for delay discounting tasks</a>. Behavior Research Methods, 48(4), 1608–1620.</p>")),
  # TOP ROW WITH INPUTS
  column(width = 4,
         wellPanel(sliderInput(inputId = "m",
                               label = "m",
                               min = -3,
                               max = 1,
                               value = defaultM,
                               step = 0.01),
                   sliderInput(inputId = "c",
                               label = "c",
                               min = -10,
                               max = 10,
                               value = defaultC,
                               step = 0.5)
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
           h2("Discount surface"),
           p("The magnitude effect states that your discount function depends upon the reward magnitude. So you in fact have a discount surface. For clarity, I show discount functions for 3 example reward magnitudes"),
           withMathJax(
             helpText('$$\\mathrm{discount~fraction}(\\{m,c\\},delay, reward)= 1/(1+(\\exp(m \\log(reward) + c)).delay)$$')
           ),
           plotOutput("discountFunctionPlot")
    ),
    column(width = 6,
           h2("Example decision"),
           p("This plot shows the internal subjective value (y-axis) of the rewards (circles) as a function of time from now."),
           withMathJax(
             helpText('$$\\mathrm{subjective~value}(\\{m,c\\},delay, reward)= reward \\times \\mathrm{discount~fraction}(\\{m,c\\},delay, reward) $$')
           ),
           plotOutput("examplePlot")
    )
  )
)






server <- function(input, output){
  # DISCOUNT FUNCTION
  output$discountFunctionPlot <- renderPlot({
    curve( hyperbolicDiscountFraction(input$m, input$c, x, 10),
           from = 0,
           to = max_delay,
           n = 1001,
           lwd = 4,
           col = rgb(0,0,0),
           xlab = "Delay (days)",
           ylab = "Discount fraction",
           xlim = c(0,max_delay),
           ylim = c(0,1))
    curve( hyperbolicDiscountFraction(input$m, input$c, x, 100),
           from = 0,
           to = max_delay,
           n = 1001,
           lwd = 4,
           add = TRUE,
           col = rgb(0.3,0.3,0.3))
    legend(x = "topright",
           legend=c("1000", "100", "10" ),
           col=c(rgb(0.6,0.6,0.6), rgb(0.3,0.3,0.3), rgb(0,0,0)), 
           lwd=c(4,4,4), 
           box.lwd = 0,
           cex=0.8,
           title="reward magnitude")
    curve( hyperbolicDiscountFraction(input$m, input$c, x, 1000),
           from = 0,
           to = max_delay,
           n = 1001,
           lwd = 4,
           add = TRUE,
           col = rgb(0.6,0.6,0.6))
  }
  )
  
  # EXAMPLE DECISION PLOT
  output$examplePlot <- renderPlot({
    # delayed reward
    curve( hyperbolicDiscountFraction(input$m, input$c, input$DB-x, input$RB) * input$RB,
           from = 0,
           to = input$DB,
           n = 1001,
           xlab = "Time from now (days)",
           ylab = "subjective value",
           xlim = c(0,max_delay),
           ylim = ylimDemoPlot(input$RA,input$RB))
    
    # immediate reward
    curve( hyperbolicDiscountFraction(input$m, input$c, input$DA-x, input$RA) * input$RA,
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
