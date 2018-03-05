

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(title = 'Topic Modeling of AFICA IT Contracts',
                        theme = shinythemes::shinytheme('flatly'),

  # Application title
  titlePanel("Topic Modeling of AFICA IT Contracts"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel( width = 3,
       sliderInput("sparsity",
                   label ="Remove Sparse Terms within:",
                   min = 0.8,
                   max = 1,
                   value = .98,
                   step = 0.01,
                   ticks = TRUE,
                   animate = TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel( width = 9,
      tabsetPanel(
        tabPanel(title = 'Document Term Matrix',
                 verbatimTextOutput(outputId = 'dtm'))

      )
    )
  )
))
