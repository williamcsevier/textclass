

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(title = 'Topic Modeling of AFICA IT Contracts',
                        theme = shinythemes::shinytheme('flatly'),

   tabPanel("DTM and Topic Modeling",
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
                   animate = FALSE),
       sliderInput("k",
                   label = "Number of Topics Modeled:",
                   min = 2,
                   max = 20,
                   value = 2,
                   step = 1,
                   ticks = TRUE,
                   animate = FALSE)),

    # Show a plot of the generated distribution
    mainPanel( width = 9,
      tabsetPanel(
        tabPanel(title = 'Document Term Matrix',
                 verbatimTextOutput(outputId = 'dtm'),
                 actionButton("build", "Build DTM")),

        tabPanel(title = 'Topic Models',
                 plotOutput(outputId = 'topic_models'),
                 actionButton("run", "Model K Topics")))))),
  tabPanel("Term and N-gram Frequency",
           sidebarLayout(
             sidebarPanel( width = 3,
                           radioButtons("n",
                                        label ="How many consecutive terms?",
                                        choices = c(1,2,3,4),
                                        selected = 1)),
           mainPanel( width = 9,
                      tabsetPanel(
                        tabPanel(title = 'Term Frequency',
                                 plotOutput(outputId = 'tf')))))
  )
))

