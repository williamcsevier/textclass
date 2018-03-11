
shinyServer(function(input, output, session) {

  observeEvent(input$build,{
    dtm <- tidyDTM(ITdata, input$sparsity)
    rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
    dtm <- dtm[rowTotals> 0, ]           #remove all docs without words
    output$dtm <- renderPrint({
      return(dtm)
    })
  observeEvent(input$run,{
    lda <- LDA(dtm, input$k)
    plt <- plot_topics(lda)
    output$topic_models <- renderPlot({
      plt})
    })
  })
  output$tf <- renderPlot({
    tf_plot <- term_frequency(data = ITdata,n= as.double(input$n))
    tf_plot
  })
  })

