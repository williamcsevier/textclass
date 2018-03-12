ui <- shinyUI(navbarPage(title = 'Topic Modeling of AFICA IT Contracts',
                        theme = shinythemes::shinytheme('flatly'),

tabPanel("Term and N-gram Frequency",
         sidebarLayout(
           sidebarPanel( width = 3,
                         radioButtons("n",
                                      label ="How many consecutive terms?",
                                      choices = c(1,2,3,4),
                                      selected = 1),
                         actionButton("choose", "Show Term Frequency")),
           mainPanel( width = 9,
                      plotOutput(outputId = 'tf')))),

tabPanel("DTM and Topic Modeling",


verticalLayout(
  sidebarLayout(
    sidebarPanel( width = 3,
       sliderInput("sparsity",
                   label ="Remove Sparse Terms within:",
                   min = 0.8,
                   max = .99,
                   value = .98,
                   step = 0.01,
                   ticks = TRUE,
                   animate = FALSE),
       actionButton("build", "Build DTM")),
       mainPanel( width = 9,
                           verbatimTextOutput(outputId = 'dtm')))),

sidebarLayout(
    sidebarPanel(width = 3,
       sliderInput("k",
                   label = "Number of Topics Modeled:",
                   min = 2,
                   max = 20,
                   value = 2,
                   step = 1,
                   ticks = TRUE,
                   animate = FALSE),
       actionButton("run", "Model K Topics")),
       mainPanel(width = 9,
                plotOutput(outputId = 'topic_models')))),

tabPanel("Bigram Correlations",
         verticalLayout(
           actionButton("corr", "Construct Bigram Correlations Plot"),
           plotOutput(outputId = 'bigrams')))


         )

)
