library(shiny)
library(UsingR)
library(ggplot2)

shinyServer(function(input, output) {
  y <- galton$child - mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  freqData <- as.data.frame(table(x,y))
  names(freqData) <- c("child", "parent", "freq")
  freqData$child <- as.numeric(as.character(freqData$child))
  freqData$parent <- as.numeric(as.character(freqData$parent))
# normalized values
  yn <- (galton$child - mean(galton$child))/sd(galton$child)
  xn <- (galton$parent - mean(galton$parent))/sd(galton$parent)
  corynxn <- cor(yn, xn)
# best fit https://github.com/bcaffo/regmodsbook/blob/master/manuscript/01_introduction.md  
  lm1 <-lm(formula = I(child - mean(child)) ~ I(parent - mean(parent)) -1, data = galton)
  cnfit1 <- confint(lm1)[1]
  
  myPlot <- reactive({
      slopeInput <- input$sliderSLOPE
      ggh <- ggplot(freqData[freqData$freq >0,], aes(x=parent, y=child))
    # ggh <- ggh + scale_size(range=c(2,20), guide='none')
      ggh <- ggh + geom_point(colour="grey50", aes(size=freq+20))
    # ggh <- ggh + geom_point(colour="grey50", aes(size=freq+20, show_guide=FALSE))
      ggh <- ggh + geom_point(aes(colour=freq, size=freq))
    # ggh <- ggh  + scale_colour_gradient(low="lightblue", high="white")
      if(input$showNorm1)
        ggh <- ggh + geom_abline(intercept=0, slope=cnfit1, size=2, colour='green')
      if(input$showBest1)
        ggh <- ggh + geom_abline(intercept=0, slope=corynxn, size=2, colour='red')
      ggh <- ggh + geom_abline(intercept=0, slope=slopeInput, size=2)
      mse <- mean((y - slopeInput * x) ^ 2)
      ggh <- ggh + ggtitle(paste("Slope/beta = ", slopeInput, "mse = ", round(mse, 3)))
      ggh
  })
  
    
  msecalc <- reactive({
    slopeInput <- input$sliderSLOPE
    mse <- mean((y - slopeInput * x) ^ 2)
  })

  output$plot1 <- renderPlot({
    legend(25, 250, c("Xi are the parents’ heights. Consider picking the slope β that minimizes ∑i=1n(Yi−Xiβ)2"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
    myPlot()
  })
  
  output$pred1 <- renderText({
    if(input$showNorm1)
       paste("MSE value :", as.character(round(msecalc(),3)), ",   Slope normalized values :", as.character(round(corynxn,3)))
      else
        paste("MSE value :", as.character(round(msecalc(),3)))
  })
})