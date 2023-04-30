library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)

# Upload the data set and clean the names
trade_partners_raw <- read.csv('data/FAO_data_US_agriculture_trade_quantity_2001-2021.csv')
trade_partners_raw <- clean_names(trade_partners_raw)

# Select columns of interest
trade_partners <- trade_partners_raw %>% 
  select(reporter_countries,
         partner_countries,
         element,
         item,
         year,
         value)

# Aggregate values for import/export line chart for each country
trade_trend <- trade_partners %>% 
  select(element,
         partner_countries,
         item,
         year,
         value) %>% 
  group_by(partner_countries, element, item, year) %>% 
  summarise(value = round(sum(value)))

# Define UI
ui <- fluidPage(
  selectInput(inputId = "produce",
              label = "Choose a Produce:",
              choices = sort(unique(trade_partners$item))),
  fluidRow(
    column(width = 6, plotlyOutput("import_plot")),
    column(width = 6, plotlyOutput("export_plot"))
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create import plot
  output$import_plot <- renderPlotly({
    # Filter data based on produce selection and element type
    filtered_data <- trade_trend %>% 
      filter(item == input$produce & element == "Import Quantity")
    
    # Create line chart
    p <- ggplot(filtered_data, aes(x = year, y = value, color = partner_countries, group = partner_countries,
                                   text = paste("Year:", year, "<br>",
                                                "Country:", partner_countries, "<br>",
                                                "Import Quantity:", value))) + 
      geom_line() +
      scale_x_continuous(breaks = seq(2001, 2021, 1)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      labs(y = "Quantity of Trade (in Tonnes)", 
           x = "Year",
           title = paste("U.S.", input$produce, "Import Trade Partners Over Time"),
           color = "Country") +
      theme_minimal() + 
      theme(
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    # Convert ggplot object to plotly object
    ggplotly(p, tooltip = "text") %>%
      layout(title = NULL)
  })
  
  # Create export plot
  output$export_plot <- renderPlotly({
    # Filter data based on produce selection and element type
    filtered_data <- trade_trend %>% 
      filter(item == input$produce & element == "Export Quantity")
    
    # Create line chart
    p <- ggplot(filtered_data, aes(x = year, y = value, color = partner_countries, group = partner_countries,
                                   text = paste("Year:", year, "<br>",
                                                "Country:", partner_countries, "<br>",
                                                "Export Quantity:", value))) + 
      geom_line() +
      scale_x_continuous(breaks = seq(2001, 2021, 1)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      labs(y = "Quantity of Trade (in Tonnes)", 
           x = "Year",
           title = paste("U.S.", input$produce, "Export Trade Partners Over Time"),
           color = "Country") +
      theme_minimal() + 
      theme(
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    # Convert ggplot object to plotly object
    ggplotly(p, tooltip = "text") %>%
      layout(title = NULL)
    
  })
  
}

# View App
shinyApp(ui, server)