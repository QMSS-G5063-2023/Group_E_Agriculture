library(shiny)
library(dplyr)
library(network)
library(ggplot2)

# Load data
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

# Define UI
ui <- fluidPage(
  selectInput(inputId = "year",
              label = "Select year:",
              choices = unique(trade_partners_raw$year)),
  selectInput(inputId = "produce",
              label = "Choose a Produce:",
              choices = unique(trade_partners_raw$item)),
  plotlyOutput("network_plot")
)

# Define server logic
server <- function(input, output) {
  
  output$network_plot <- renderPlotly({
    
    # Filter data based on year and produce selection
    filtered_data <- trade_partners_raw %>% 
      filter(year == input$year & item == input$produce & element == "Import Quantity")
    
    # Create network plot
    trade_net <- network::network(filtered_data[, c("partner_countries", "reporter_countries")], directed = TRUE)
    set.edge.attribute(trade_net, "export_value", filtered_data$value)
    
    plot <- ggplot(trade_net, 
                   aes(x, y, xend = xend, yend = yend)) +
      geom_edges(aes(size = export_value, color = vertex.names)) +
      geom_nodes(aes(color = vertex.names)) +
      geom_nodelabel(aes(label = vertex.names, color = vertex.names)) +
      labs(title = paste(input$produce, "trade in", input$year)) +
      theme_void()
    
    ggplotly(plot)
  })
}

# Run the app
shinyApp(ui = ui, server = server)



