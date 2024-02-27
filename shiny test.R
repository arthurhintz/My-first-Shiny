#==========/==========/==========/==========/==========/==========/==========/==========/
# Packages

library(shiny)
library(tidyverse)
library(plotly)

#==========/==========/==========/==========/==========/==========/==========/==========/
# Organize data

data <- mtcars %>%
  rownames_to_column(var = "Car") %>%
  mutate(
    vs = factor(vs),
    am = case_when(am == 0 ~ "Automatic",
                   am == 1 ~ "Manual")
  )

#==========/==========/==========/==========/==========/==========/==========/==========/

ui <- fluidPage(
  titlePanel("Choose the best car"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cyl", "Select the number of cylinders:",
                  choices = c("All", unique(data$cyl))),
      
      selectInput("gear", "Select the number of gears:",
                  choices = c("All", unique(data$gear))),
      
      selectInput("am", "Select the transmission type:",
                  choices = c("All", unique(data$am))),
      
      sliderInput("hp", label = "Select the horsepower limits",
                  min = min(data$hp), max = max(data$hp), value = c(min(data$hp), max(data$hp))),
      
      actionButton("reset", "Reset Filters"),
    ),
    
    mainPanel(
      tableOutput("top_mpg_table"),
      plotlyOutput("scatter_plot", height = 250),
      plotlyOutput("box_plot", height = 250)
    )
  )
)

#==========/==========/==========/==========/==========/==========/==========/==========/

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$cyl)
    req(input$gear)
    req(input$am)
    
    filtered <- data
    
    if (input$cyl != "All") {
      filtered <- filtered %>% filter(cyl == input$cyl)
    }
    
    if (input$gear != "All") {
      filtered <- filtered %>% filter(gear == input$gear)
    }
    
    if (input$am != "All") {
      filtered <- filtered %>% filter(am == input$am)
    }
    
    filtered <- filtered %>% filter(between(hp, input$hp[1], input$hp[2]))
    
    filtered
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~hp, y = ~mpg, color = ~factor(cyl), type = 'scatter', mode = 'markers', source = "scatter_plot") %>%
      layout(title = "Relationship between Horsepower and Miles/gallon",
             xaxis = list(title = "Horsepower"),
             yaxis = list(title = "Miles Per Gallon"),
             showlegend = TRUE)
  })
  
  # Box plot
  output$box_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~gear, y = ~mpg, type = 'box') %>%
      layout(title = "Distribution of Mileage by Number of Gears",
             xaxis = list(title = "Number of Gears"),
             yaxis = list(title = "Miles Per Gallon"))
  })
  
  # Select fuel economic car
  output$top_mpg_table <- renderTable({
    filtered_data() %>%
      arrange(desc(mpg)) %>%
      slice_head(n = 3)
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "cyl", selected = "All")
    updateSelectInput(session, "gear", selected = "All")
    updateSelectInput(session, "am", selected = "All")
    updateSliderInput(session, "hp", value = c(min(data$hp), max(data$hp)))
  })
}

# Execute the app
shinyApp(ui = ui, server = server)
