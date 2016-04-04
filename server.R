



# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

revtotal <- read.table(file="revtotal")

shinyServer(function(input, output) {

  output$tsPlot <- renderPlot({

    # Convert revtotal to a time series of the requisite length
    #start=c(2012,1),end=c(2016,8)
    
    mts <- ts(revtotal$monthly_revenue, start=c(2012,1),end=c(2012 + (input$periods-1) %/% 12, (input$periods-1) %% 12 + 1),frequency=12)

    # Fit Seasonal Decomposition
    fit <- stl(mts, s.window="period", robust = TRUE)
    
    # Prepare the time series analysis plot
    op <- par(pin = c(10, 10),mar = c(0, 4, 0, 3), oma = c(5, 0, 4, 0), mfcol = c(4, 1))
    plot(fit, set.pars = NULL, labels  =  NULL,
         main = "Seasonal decompositon of Time series by Loess (STL)")
    
    # Add markers for outliers
    (iO <- which(fit$weights  < 1e-8))
    sts <- fit$time.series
    points(time(sts)[iO], 0.8* sts[,"remainder"][iO], pch = 4, col = "red")
    par(op)   # reset

  })

})
