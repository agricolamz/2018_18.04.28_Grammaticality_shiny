library(shiny)
library(dplyr)
library(ggplot2)
library(Compositional)

server <- function(input, output) {
  output$BetaPlot <- renderPlot({
    alpha = input$sample_size * input$grammaticality
    beta = input$sample_size * (1 - input$grammaticality)
    
    data_frame(
      id = seq(0, 1, length = 100),
      density = dbeta(id, shape1 = alpha, shape2 = beta)) %>% 
      ggplot(aes(id, density))+
      geom_line(alpha = 0.5)+
      theme_bw()+
      labs(x = "grammaticality",
           title = paste0("p(+) = ", alpha, ", p(-) = ", beta))
  })
  output$DirechletPlot <- renderPlot({
    if(input$grammaticality == 0.5){
      alpha_1 = 1/3 * input$sample_size
      alpha_2 = 1/3 * input$sample_size
      alpha_3 = 1/3 * input$sample_size
    } else if(input$grammaticality > 0.5){
      alpha_2 = input$sample_size * input$grammaticality
      alpha_1 = input$sample_size * ((1 - input$grammaticality)/2)
      alpha_3 = input$sample_size * ((1 - input$grammaticality)/2)
    } else {
      alpha_2 = input$sample_size * ((input$grammaticality)/2)
      alpha_3 = input$sample_size * ((input$grammaticality)/2)
      alpha_1 = input$sample_size * (1-input$grammaticality)}
    
    diri.contour(a = c(alpha_1, alpha_2, alpha_3))
    title(xlab="grammaticality",
          main=paste0("p(+) = ", alpha_2, 
                      ", p(-) = ", alpha_1, 
                      ", p(?) = ", alpha_3))
  })
}