
shinyServer(function(input, output, session) {

  output$dtm <- renderPrint({

    dtm <- tidyDTM(ITdata, input$sparsity)

    return(dtm)

  })

})
