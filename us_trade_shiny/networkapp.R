library(shiny)
library(dplyr)
library(network)
library(ggplot2)
library(plotly)

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
              choices = sort(unique(trade_partners_raw$year))),
  selectInput(inputId = "produce",
              label = "Choose a Produce:",
              choices = sort(unique(trade_partners_raw$item))),
  fluidRow(
    column(width = 6, plotlyOutput("import_plot")),
    column(width = 6, plotlyOutput("export_plot"))
  )
)

# Define server logic
server <- function(input, output) {
  
  output$import_plot <- renderPlotly({
    
    # Filter data based on year and produce selection
    filtered_data <- trade_partners_raw %>% 
      filter(year == input$year & item == input$produce & element == "Import Quantity")
    
    # Create network plot
    trade_net <- network::network(filtered_data[, c("partner_countries", "reporter_countries")], directed = TRUE)
    set.edge.attribute(trade_net, "Import Value", filtered_data$value)
    
    plot <- ggplot(trade_net,
                   aes(x, y, xend = xend, yend = yend)) +
      geom_edges(aes(size = `Import Value`,
                     color = vertex.names, 
                     text = paste("Country:", vertex.names, "<br>",
                                  "Import Quantity:", `Import Value`))) +
      geom_nodes(aes(size = `Import Value`, 
                     color = vertex.names)) +
      geom_nodelabel(aes(label = vertex.names, color = vertex.names)) +
      labs(title = paste(input$produce, "Import Trade in", input$year),
           color = "Country",
           subtitle = "Quantity in Tonnes") +
      theme_void()
    
    ggplotly(plot, 
             tooltip = "text") %>% 
      layout(hovermode = "x",
             hoverdistance = 1)
    
  })
  
  output$export_plot <- renderPlotly({
    
    # Filter data based on year and produce selection
    filtered_data <- trade_partners_raw %>% 
      filter(year == input$year & item == input$produce & element == "Export Quantity")
    
    # Create network plot
    trade_net <- network::network(filtered_data[, c("partner_countries", "reporter_countries")], directed = TRUE)
    set.edge.attribute(trade_net, "Export Value", filtered_data$value)
    
    plot <- ggplot(trade_net,
                   aes(x, y, xend = xend, yend = yend)) +
      geom_edges(aes(size = `Export Value`,
                     color = vertex.names, 
                     text = paste("Country:", vertex.names, "<br>",
                                  "Export Quantity:", `Export Value`))) +
      geom_nodes(aes(size = `Export Value`, 
                     color = vertex.names)) +
      geom_nodelabel(aes(label = vertex.names, color = vertex.names)) +
      labs(title = paste(input$produce, "Export Trade in", input$year),
           color = "Country",
           subtitle = "Quantity in Tonnes") +
      theme_void()
    
    ggplotly(plot, 
             tooltip = "text") %>% 
      layout(hovermode = "x",
             hoverdistance = 1)
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)



