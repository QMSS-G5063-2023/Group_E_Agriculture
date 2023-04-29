library(ggplot2)
library(dplyr)
library(shiny)
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

# Aggregate values for import/export line chart 
produce_trade <- trade_partners %>% 
  select(element,
         item,
         year,
         value) %>% 
  group_by(element, item, year) %>% 
  mutate(item = case_when(item == "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)"  ~ "Chillies and peppers",
                          TRUE ~ item)) %>%  
  summarise(value = sum(value))

# Define UI
ui <- fluidPage(
  selectInput(inputId = "year",
              label = "Select year:",
              choices = unique(produce_trade$year)),
  plotlyOutput("import_bar"),
  plotlyOutput("export_bar")
)

# Define server logic
server <- function(input, output) {
  
  # Import quantity chart
  output$import_bar <- renderPlotly({
    # Filter data based on year selection and import quantity
    filtered_data <- produce_trade %>% 
      filter(year == input$year & element =="Import Quantity")
    
    # Get top 10 produce items based on value
    top10_items <- filtered_data %>% 
      group_by(item) %>% 
      summarize(total_value = sum(value)) %>% 
      arrange(desc(total_value)) %>% 
      top_n(10)
    
    # Create horizontal bar chart
    chart <- ggplot(top10_items, 
                    aes(x = fct_reorder(item, total_value), 
                        y = total_value,
                        text = paste("Import Qunatity:", total_value))) +
      geom_col(fill = "#f99976") +
      xlab("Produce") +
      ylab("Total Trade Quantity (in Tonnes)") +
      ggtitle(paste("Top 10 Produce Imports in", input$year)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12)) +
      coord_flip() +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
    # Convert ggplot to plotly
    ggplotly(chart, 
             tooltip="text") %>% config(displayModeBar = F)
  })
  
  # Export quantity chart
  output$export_bar <- renderPlotly({
    # Filter data based on year selection and export quantity
    filtered_data <- produce_trade %>% 
      filter(year == input$year & element =="Export Quantity")
    
    # Get top 10 produce items based on value
    top10_items <- filtered_data %>% 
      group_by(item) %>% 
      summarize(total_value = sum(value)) %>% 
      arrange(desc(total_value)) %>% 
      top_n(10)
    
    # Create horizontal bar chart
    chart <- ggplot(top10_items, 
                    aes(x = fct_reorder(item, total_value), 
                        y = total_value,
                        text = paste("Export Qunatity:", total_value))) +
      geom_col(fill = "#7ac9af") +
      xlab("Produce") +
      ylab("Total Trade Quantity (in Tonnes)") +
      ggtitle(paste("Top 10 Produce Exports in", input$year)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12)) +
      coord_flip() +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
    # Convert ggplot to plotly
    ggplotly(chart, 
             tooltip="text") %>% config(displayModeBar = F) 
  })
  
}

# View app
shinyApp(ui = ui, server = server)

