# Load required packages
library(shiny)
library(tidyverse)
library(gapminder)


gapminder <- gapminder[complete.cases(gapminder), ]
# Define UI
ui <- fluidPage(
  # Add a title
  titlePanel("Gapminder Dataset"),
  
  # Add a sidebar layout
  sidebarLayout(
    # Add a sidebar panel with two widgets
    sidebarPanel(
      # Add a widget to select a continent
      selectInput(inputId = "continent",
                  label = "Select a continent:",
                  choices = c("All", levels(gapminder$continent))),
      
      # Add a widget to select a year
      sliderInput(inputId = "year",
                  label = "Select a year:",
                  min = min(gapminder$year),
                  max = max(gapminder$year),
                  value = min(gapminder$year))
    ),
    
    # Add a main panel with tabs
    mainPanel(
      tabsetPanel(
        # Add a tab for general information
        tabPanel("General Info", 
                 HTML("<h2>About the Dataset</h2>
                      <p>The Gapminder dataset contains information on life expectancy, population, and GDP per capita for countries around the world.</p>
                      <p>The data is available from 1952 to 2007, and is categorized by continent.</p>")),
        
        # Add a tab for plots
        tabPanel("Plots", plotOutput("myplot")),
        
        # Add a tab for tables
        tabPanel("Tables", tableOutput("mytable"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Create a reactive dataset
  filtered_data <- reactive({
    data <- gapminder
    
    # Filter by continent
    if(input$continent != "All") {
      data <- data %>% filter(continent == input$continent)
    }
    
    # Filter by year
    data <- data %>% filter(year == input$year)
    
    return(data)
  })
  
  # Create a plot
  output$myplot <- renderPlot({
    ggplot(filtered_data(), aes(x = gdpPercap, y = lifeExp)) +
      geom_point(size = 3) +
      labs(x = "GDP per capita", y = "Life expectancy") +
      ggtitle(paste("Life Expectancy and GDP per Capita in", input$continent, "in", input$year))
  })
  
  # Create a table
  output$mytable <- renderTable({
    filtered_data() %>% select(country, lifeExp, pop, gdpPercap) %>% 
      rename("Country" = country, 
             "Life Expectancy" = lifeExp, 
             "Population" = pop, 
             "GDP per capita" = gdpPercap)
  })
}

# Run the app
shinyApp(ui, server)
