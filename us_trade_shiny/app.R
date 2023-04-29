library(shiny)
library(dplyr)
library(ggplot2)
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

# Aggregate values for import/export line chart 
produce_trade <- trade_partners %>% 
  select(element,
         item,
         year,
         value) %>% 
  group_by(element, item, year) %>% 
  summarise(value = sum(value))

# Write code for interactive import/export line chart
ui <- fluidPage(
  selectInput(inputId = "item", 
              label = "Choose a Produce", 
              choices = unique(produce_trade$item)), 
  plotOutput("line")
)

server <- function(input, output) {
  output$line <- renderPlot({
    ggplot(produce_trade %>% 
             filter(item == input$item), 
           aes(year, value, 
               color = element, group = element)) + 
      geom_line() +
      scale_x_continuous(breaks = seq(2001, 2021, 1)) +
      #scale_y_continuous(breaks = seq(0, 40, 5)) +
      scale_color_brewer(palette="Set2") +
      labs(y = "Quantity of Trade (in Tonnes)", 
           x = "Year",
           title = "U.S. Produce Trade Over Time",
           subtitle = "Data from trade between 2001 - 2021",
           caption = "Source: Food and Agriculture Organization of the United Nations",
           color = "Trade Type") +
      theme_minimal() + 
      theme(
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"))
  })
}

# View App
 shinyApp(ui, server)