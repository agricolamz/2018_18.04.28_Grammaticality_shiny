library(shiny)

ui <- fluidPage(
  titlePanel("Grammaticality estimation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("grammaticality",
                  "Grammaticality:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.7, 
                  step = 0.01),
      sliderInput("sample_size",
                  "Sample size:",
                  min = 1,
                  max = 100,
                  value = 40)),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Beta distribution", plotOutput("BetaPlot")), 
        tabPanel("Direchlet distribution", plotOutput("DirechletPlot"))
      )
    )
  )
)