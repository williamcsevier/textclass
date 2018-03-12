
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
  observeEvent(input$choose,{
  output$tf <- renderPlot({
    tf_plot <- term_frequency(data = ITdata,n= as.double(input$n))
    tf_plot
  })})
  observeEvent(input$corr,{
     bigrams <- ITdata %>%
        unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% sw$word,
               !word2 %in% sw$word) %>%
        count(word1, word2, sort = TRUE)

     bigrams <- bigrams %>%
            filter(n > 500,
            !str_detect(word1, "\\d"),
            !str_detect(word2, "\\d"))

      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

      plt <- bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()

      output$bigrams <- renderPlot({
        plt
      })
})

  })

