library(shiny)
library(ggplot2)

# probability density functions
calculate_density <- function(t, x1, x2, x3, x4, x5) {
  density <- 0.019 * (t/74.44)^0.38 * exp(1.24*x1 + 1.57*x2 - 0.11*x3 + 0.04*x4 + 0.01*x5) *
    (1 + (1.45 * t^1.378 * exp(1.24*x1 + 1.57*x2 - 0.11*x3 + 0.04*x4 + 0.01*x5) / 382.91))^-1.691 
  return(density)
}

# UI 
ui <- fluidPage(
  titlePanel("Visualize probability density functions"),
  
  # Input Section
  fluidRow(
    column(3, radioButtons("gender", "Gender Selection:", choices = c("Male", "Female"), selected = "Male")),
    column(3, numericInput("age", "Age:", min = 0, max = 100, value = 50)),
    column(3, numericInput("ApoE4", "Number of ApoE4 genes:", min = 0, max = 2, value = 0)),
    column(3, numericInput("education", "Education (years):", min = 0, max = 100, value = 12))
  ),
  
  # Add some space
  br(),
  
  # Plot Output Section
  fluidRow(
    column(12, plotOutput("density_plot"))
  )
)

# Server 
server <- function(input, output) {
  output$density_plot <- renderPlot({
    t_values <- seq(0, 100, by = 1)
    
    density_values <- calculate_density(
      t_values,
      ifelse(input$ApoE4 == 0, 1, 0),         # x1
      ifelse(input$ApoE4 == 1, 1, 0),         # x2
      ifelse(input$gender == "Male", 1, 0),   # x3
      input$age,                              # x4
      ifelse(input$education >= 12, 1, 0)     # x5 
    )
    
    df <- data.frame(Time = t_values, Density = density_values)
    
    ggplot(df, aes(Time, Density)) +
      geom_line() +
      labs(x = "Time", y = "Probability Density") +
      ylim(0, max(density_values) * 1.2) +
      ggtitle(paste("Age:", input$age, ", Gender:", input$gender, ", Education:", input$education, " years"))
  })
}

shinyApp(ui, server)