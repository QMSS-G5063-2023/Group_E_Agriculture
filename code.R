knitr::opts_chunk$set(echo = TRUE)
install.packages("ggridges")
install.packages("ggplot2")
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(DT)

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)

library(shinythemes)

library(shiny)
library(leaflet)
library(scales)
library(dplyr)
library(ggplot2)
library(emoji)
library(dplyr)
library(ggimage)
library(emojifont)
library(shiny)
#library(shinydashboard)
library(ggplot2)
library(stringr)
library(forcats)
library(dplyr)
library(plotly)
library(highcharter)
library(DT)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
#library(leaflegend)
library(sf)
library(usmap)
library(ggplot2)
library(maps)
library(ggthemes)
library(readxl)
library(stringr)
library(urbnmapr)
library(leaflet.extras)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(plotly)
library(DT)
library(highcharter)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(plotly)


# Import data (put all your CSVs here)
calendar <- read.csv("data/calendar.csv")

df <- calendar %>% select(Program, Year, Period, Commodity, Data.Item, Value) 
trade_partners_raw <- read.csv('data/FAO_data_US_agriculture_trade_quantity_2001-2021.csv')
trade_partners_raw <- clean_names(trade_partners_raw)
trade_yield <- read.csv('data/FAO_Data/FAO_data_US_annual_yield_production_2001-2021.csv')
trade_yield <- clean_names(trade_yield)

# Data processing
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

trade_yield$image <-"data/emojis/Bud.png"

make_production_table <- function(fruit_veg, y_p){ 
  
  production_table <- trade_yield %>% 
    filter(item == fruit_veg, element == y_p) 
  return(production_table) }

make_production_plot <- function(fruit_veg, y_p,unit, icon_point="Bud"){ 
  
  production_table <- trade_yield %>% 
    filter(item == fruit_veg, element == y_p, unit == unit ) %>% 
    mutate(image = paste0("data/emojis/",icon_point,".png")) 
  
  ggplot(production_table) + 
    geom_line(aes(x=year, y = value, color="#fc8d62"),size=.5, show.legend = FALSE) +
    geom_point(aes(x=year, y = value), size=.5) +
    geom_image(aes(x=year, y = value,image= image), size=.055) +
    labs(x="Year",y=paste0(y_p," of ",fruit_veg, " (",unit,")")) + theme_minimal() + theme(axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20))) 
  
}

# Define UI
ui <- fluidPage(
  # App title ----
  #titlePanel("Title of your project"),
  
  navbarPage("Farm stuff", 
             theme = shinytheme("flatly"),
             tabPanel("U.S. Agricultural Trade", fluid = TRUE, 
                      selectInput(inputId = "produce",
                                  label = "Choose a Produce:",
                                  choices = sort(unique(trade_partners$item))),
                      fluidRow(
                        column(width = 6, plotlyOutput("import_plot")),
                        column(width = 6, plotlyOutput("export_plot"))
                      )
             ),
             tabPanel("U.S. Agricultural Production and Yield", fluid = TRUE, 
                      align="center",
                      plotOutput("fruit_plot",click = "plot_click", width = "600px", height="500px"),
                      p(),
                      fluidRow(
                        column(12,
                               selectInput(
                                 "fruit",
                                 "Select a fruit or vegetable:",
                                 choices = c(
                                   "Almonds, in shell",                                                       
                                   "Apples",                                                                  
                                   "Apricots",                                                                
                                   "Artichokes",                                                              
                                   "Asparagus",                                                               
                                   "Avocados",                                                                
                                   "Bananas",                                                                 
                                   "Barley",                                                                  
                                   "Beans, dry",                                                              
                                   "Blueberries",                                                             
                                   "Broad beans and horse beans, green",                                      
                                   "Buckwheat",                                                               
                                   "Cabbages",                                                                
                                   "Cantaloupes and other melons",                                            
                                   "Carrots and turnips",                                                     
                                   "Castor oil seeds",                                                        
                                   "Cauliflowers and broccoli",                                               
                                   #"Cereals n.e.c.",                                                          
                                   "Cherries",                                                                
                                   "Chick peas, dry",                                                         
                                   #"Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw",            
                                   #"Chillies and peppers, green (Capsicum spp. and Pimenta spp.)",            
                                   "Coffee, green",                                                           
                                   "Cow peas, dry",                                                           
                                   "Cranberries",                                                             
                                   "Cucumbers and gherkins",                                                  
                                   #"Currants",                                                                
                                   "Dates",                                                                   
                                   "Eggplants (aubergines)",                                                  
                                   "Figs",                                                                    
                                   "Ginger, raw",                                                             
                                   "Grapes",                                                                  
                                   "Green corn (maize)",                                                      
                                   "Green garlic",                                                            
                                   "Groundnuts, excluding shelled",                                           
                                   "Hazelnuts, in shell",                                                     
                                   "Hop cones",                                                               
                                   "Kiwi fruit",                                                              
                                   "Lemons and limes",                                                        
                                   "Lentils, dry",                                                            
                                   "Lettuce and chicory",                                                     
                                   "Linseed",                                                                 
                                   "Maize (corn)",                                                            
                                   "Mangoes, guavas and mangosteens",                                         
                                   "Millet",                                                                  
                                   "Mushrooms and truffles",                                                  
                                   "Mustard seed",                                                            
                                   "Oats",                                                                    
                                   "Okra",                                                                    
                                   "Olives",                                                                  
                                   "Onions and shallots, dry (excluding dehydrated)",                         
                                   "Oranges",                                                                 
                                   "Other beans, green",                                                      
                                   "Other berries and fruits of the genus vaccinium n.e.c.",                  
                                   "Other citrus fruit, n.e.c.",                                              
                                   "Other fruits, n.e.c.",                                                    
                                   "Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c.",
                                   "Other vegetables, fresh n.e.c.",                                          
                                   "Papayas",                                                                 
                                   "Peaches and nectarines",                                                  
                                   "Pears",                                                                   
                                   "Peas, dry",                                                               
                                   "Peas, green",                                                             
                                   "Pineapples",                                                              
                                   "Pistachios, in shell",                                                    
                                   "Plums and sloes",                                                         
                                   "Pomelos and grapefruits",                                                 
                                   "Potatoes",                                                                
                                   "Pumpkins, squash and gourds",                                                                                                 
                                   "Raspberries",                                                             
                                   "Rice",                                                                    
                                   "Rye",                                                                     
                                   "Safflower seed",                                                          
                                   "Seed cotton, unginned",                                                   
                                   "Sesame seed",                                                             
                                   "Sorghum",                                                                 
                                   "Sour cherries",                                                           
                                   "Soya beans",                                                              
                                   "Spinach",                                                                 
                                   "Strawberries",                                                            
                                   "String beans",                                                            
                                   "Sugar beet",                                                              
                                   "Sugar cane",                                                              
                                   "Sunflower seed",                                                          
                                   "Sweet potatoes",                                                          
                                   "Tangerines, mandarins, clementines",                                      
                                   "Taro",                                                                    
                                   "Tomatoes",                                                                
                                   "Tung nuts",                                                                                                             
                                   "Walnuts, in shell",                                                       
                                   "Watermelons",                                                             
                                   "Wheat"
                                 ),
                                 selected = "Apple"
                               ),
                               radioButtons(
                                 "value",
                                 "Select an output:",
                                 choices = c("Production","Yield"),
                                 selected = "Production")
                        )
                      ),
                      tableOutput("data")
             ),
             tabPanel("U.S. Agricultural Monthly Prices", fluid = TRUE, 
                      mainPanel(
                        plotOutput("plot",
                                   width = "1200px", 
                                   height="1000px")
                      )
             ),
  ),
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  output$plot <- renderPlot({
    
    p <- df %>%
      ggplot( aes(y=Commodity, 
                  x=Period,  
                  #height=stat(Value),
                  #size = 1, # outline
                  fill=Value)) +
      geom_density_ridges(alpha=0.6, # transparency
                          stat="binline", # bins
                          binwidth = 0.5,) + 
      theme_ridges() +
      theme(legend.position="none")
    
    p
  })
  
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
  
  output$fruit_plot <- renderPlot({
    make_production_plot(
      fruit_veg = input$fruit,
      y_p = input$value,
      if (input$value == "Yield") {
        unit = "hg/ha" 
      }  else {
        unit = "Tonnes"
      },
      if (input$fruit == "Apples" || input$fruit == "Avocados" || input$fruit == "Bananas" || input$fruit == "Avocados" || input$fruit == "Blueberries" || 
          input$fruit == "Cherries"|| input$fruit == "Grapes" || input$fruit == "Oranges" || input$fruit == "Olives" || input$fruit == "Sweet potatoes" ||
          input$fruit == "Pears" || input$fruit =="Pineapples" || input$fruit == "Potatoes" || input$fruit == "Tomatoes" || input$fruit == "Watermelons" || input$fruit == "Strawberries") {icon_point = input$fruit
      } 
      
      else if (input$fruit == "Barley" || input$fruit == "Buckwheat" || input$fruit == "Rice" || input$fruit =="Wheat") {icon_point = "Crop"}
      
      else if (input$fruit == "Beans, dry" || input$fruit == "Broad beans and horse beans, green" || input$fruit == "Chick peas, dry" ||
               input$fruit == "Lentils, dry" ) {icon_point = "Beans"}
      
      else if (input$fruit == "Carrots and turnips") {icon_point = "Carrots"}
      
      else if (input$fruit == "Almonds, in shell" || input$fruit ==  "Hazelnuts, in shell" || input$fruit == "Groundnuts, excluding shelled" || input$fruit == "Pistachios, in shell" ||
               input$fruit == "Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c." ||
               input$fruit == "Tung nuts" || input$fruit == "Walnuts, in shell") {icon_point = "Nuts"}
      
      else if (input$fruit == "Cabbages" || input$fruit == "Lettuce and chicory" || input$fruit == "Spinach") {icon_point = "Lettuce"}
      
      else if (input$fruit == "Cauliflowers and broccoli") {icon_point = "Broccoli"}
      else if (input$fruit == "Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw" || input$fruit == "Chillies and peppers, green (Capsicum spp. and Pimenta spp.") {icon_point = "Chili Peppers"}
      else if (input$fruit == "Coffee, green") {icon_point = "Coffee"} 
      else if (input$fruit == "Cucumbers and gherkins") {icon_point = "Cucumbers"} 
      else if (input$fruit == "Eggplants (aubergines)") {icon_point = "Egglplants"}
      else if (input$fruit == "Kiwi fruit") {icon_point = "Kiwis"}
      else if (input$fruit == "Lemons and limes" || input$fruit == "Other citrus fruit, n.e.c.") {icon_point = "Lemons"}
      else if (input$fruit == "Maize (corn)" || input$fruit ==  "Green corn (maize)") {icon_point = "Corn"}
      else if (input$fruit == "Mangoes, guavas and mangosteens") {icon_point = "Mangoes"}
      else if (input$fruit == "Onions and shallots, dry (excluding dehydrated)") {icon_point = "Onions"}
      else if (input$fruit == "Peaches and nectarines") {icon_point = "Peaches"}
      else if (input$fruit == "Sour cherries") {icon_point = "Cherries"}
      else if (input$fruit == "Tangerines, mandarins, clementines") {icon_point = "Oranges"}
      else if (input$fruit == "Green garlic") {icon_point = "Garlic"}
      else {icon_point = "Bud"}
    )
  })
  output$data <- renderTable({
    req(input$plot_click)
    production_plot <- make_production_table(input$fruit,input$value)
    nearPoints(production_plot[,c("item","year","value")], input$plot_click)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
