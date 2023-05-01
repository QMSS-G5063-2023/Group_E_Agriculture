#Format from: #https://shiny.rstudio.com/gallery/ncaa-swim-team-finder.html

# load packages
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
library(leaflegend)
library(sf)
library(usmap)
library(ggplot2)
library(maps)
library(ggthemes)
library(readxl)
library(stringr)
# library(urbnmapr)
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
library(shiny)
library(leaflet)
library(scales)
library(dplyr)
library(urbnmapr)
library(network)




# Import data (put all your CSVs here) #########################################
#Calendar
calendar <- read.csv("data/calendar.csv")
df <- calendar %>% select(Program, Year, Period, Commodity, Data.Item, Value) 

#Network data
trade_partners_raw <- read.csv('data/FAO_Data/FAO_data_US_agriculture_trade_quantity_2001-2021.csv')
trade_partners_raw <- clean_names(trade_partners_raw)

#Yield Plots
trade_yield <- read.csv('data/FAO_Data/FAO_data_US_annual_yield_production_2001-2021.csv')
trade_yield <- clean_names(trade_yield)

#State maps
state1 <- read.csv("data/USDA_Data/state_usda_2022_2011.csv")
state2 <- read.csv("data/USDA_Data/state_usda_2010_2002.csv")
usda_state <- rbind(state1, state2)


# Data processing ################################################################

##### Agriculture Trade data processing 
# Select columns of interest
trade_partners <- trade_partners_raw %>% 
  select(reporter_countries,
         partner_countries,
         element,
         item,
         year,
         value)

# Aggregate values for import/export  
produce_trade <- trade_partners %>% 
  select(element,
         item,
         year,
         value) %>% 
  group_by(element, item, year) %>%
  mutate(item = case_when(item == "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)"  ~ "Chillies and peppers",
                          TRUE ~ item)) %>% 
  summarise(value = round(sum(value)))

# Aggregate values for import/export line chart for each country
trade_trend <- trade_partners %>% 
  select(element,
         partner_countries,
         item,
         year,
         value) %>% 
  group_by(partner_countries, element, item, year) %>% 
  summarise(value = round(sum(value)))

##### Mapping and Yields data processing 

trade_yield$image <-"data/emojis/Bud.png"

#make state map
usstate_sf <- get_urbn_map("states", sf = TRUE)
usstate_sf["State"] <- usstate_sf["state_name"]
usstate_sf["State"] <- toupper(usstate_sf$"State")

#make file with geometry data
usda_state_maps <- merge(usstate_sf,usda_state,by="State")
#st_transform(usda_state_maps, "+proj=longlat +datum=WGS84")

usda_state_maps$Value_numeric <- as.numeric(gsub(",","",usda_state_maps$Value))


# Functions 
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


make_production_map2 <- function(fruit_veg, unit,year) {
  data_year <- filter(usda_state, Data.Item == paste0(fruit_veg, " - PRODUCTION, MEASURED IN ", unit),Year == year, State != "OTHER STATES")
  data_year_map <-  merge(usstate_sf,data_year,by="State",all=TRUE)
  data_year_map <- st_transform(data_year_map, "+proj=longlat +datum=WGS84")
  data_year_map$Value_numeric <- as.numeric(gsub(",","",data_year_map$Value))
  pal <- colorNumeric(palette = c("#c5f5ae","#2b7805"),domain=data_year_map$Value_numeric)
  content <- paste(year," ",data_year_map$State,
                   " PRODUCTION:",comma(data_year_map$Value_numeric))
  map <- leaflet(data_year_map) %>%
    addPolygons(data = data_year_map, 
                fillColor = ~pal(Value_numeric), 
                fillOpacity = 1, 
                color = "#e7298a",
                weight = 1, 
                label = content)  %>%
    #popupOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
    addLegend(position = "bottomright", 
              pal = pal, 
              values = data_year_map$Value_numeric,
              title = paste0(fruit_veg, " PRODUCED IN ", unit, " IN ", year), 
              opacity = 1)  %>%
    setMapWidgetStyle(list(background= "white")) 
  return(map)
}



# Define UI ####################################################################

ui <- fluidPage(
  # App title ----
  #titlePanel("Title of your project"),
  
  navbarPage("Farm stuff", 
             theme = shinytheme("flatly"),
             tabPanel("U.S. Agricultural Trade", fluid = TRUE, 
                      
                      ##### U.S. Produce Trade Import/Export Line Graph
                      selectInput(inputId = "item", 
                                  label = "Choose a Produce", 
                                  choices = sort(unique(produce_trade$item))), 
                      plotlyOutput("line"),
                      
                      ##### U.S. Trade Bar Graph
                      selectInput(inputId = "year_bar",
                                  label = "Select year:",
                                  choices = sort(unique(produce_trade$year))),
                      fluidRow(column(width = 6, plotlyOutput("import_bar",
                                   width = "900px", 
                                   height="500px")),
                      column(width = 6, plotlyOutput("export_bar",
                                   width = "900px", 
                                   height="500px"))),
                      
                      ##### U.S. Trade Network Graph
                      selectInput(inputId = "year_net",
                                  label = "Select year:",
                                  choices = sort(unique(trade_partners_raw$year))),
                      selectInput(inputId = "produce_net",
                                  label = "Choose a Produce:",
                                  choices = sort(unique(trade_partners_raw$item))),
                      fluidRow(
                        column(width = 6, plotlyOutput("import_net",
                                                       width = "900px", 
                                                       height="500px")),
                        column(width = 6, plotlyOutput("export_net",
                                                       width = "1000px", 
                                                       height="500px"))
                      ),
                      
                      ##### U.S. Trade Partners Import/Export Line Graph
                      selectInput(inputId = "produce_line",
                                  label = "Choose a Produce:",
                                  choices = sort(unique(trade_partners$item))),
                      fluidRow(
                        column(width = 6, plotlyOutput("import_plot",
                                                       width = "1000px", 
                                                       height="500px")),
                        column(width = 6, plotlyOutput("export_plot",
                                                       width = "1000px", 
                                                       height="500px"))
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
             tabPanel("U.S. Agricultural Production Map", fluid = TRUE,
                      leafletOutput("mymap"),
                      p(),
                      fluidRow(
                        column(12,
                               selectInput(
                                 "fruit2",
                                 "Select a fruit or vegetable:",
                                 choices = c(
                                   "Apples",
                                   "Apricots",
                                   "Artichokes",
                                   "Asparagus",
                                   "Broccoli",
                                   "Cauliflower",
                                   "Celery",
                                   "Cherries, Sweet",
                                   "Garlic",
                                   "Grapes",
                                   # "Lettuce, Head",
                                   #"Lettuce, Leaf",
                                   # "Lettuce, Romaine",
                                   # "Melons, Cantaloup",
                                   # "Melons, Honeydew",
                                   # "Melons, Watermelon",
                                   # "Onions, Dry",
                                   # "Orange, Mid & Navel, Utilized",
                                   # "Oranges, Valencia, Utilized",
                                   "Peaches",
                                   "Pears",
                                   "Peppers, Bell",
                                   "Peppers, Chile",
                                   "Potatoes",
                                   "Pumpkins",
                                   # "Spinach",
                                   "Squash",
                                   "Strawberries",
                                   "Sweet Potatoes"
                                   #"Tangerines, Utilized",
                                   #"Tomatoes, In the Open"
                                 ),
                                 selected = "Apple"
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               sliderInput(
                                 "year",
                                 "Year:", min = 2007, max = 2022, value = 2022, step = 1, sep = "" )
                        )
                      ),
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
server <- function(input, output, session) {
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
  
  ###################### U.S. PRODUCE TRADE IMPORT/EXPORT LINE GRAPH CODE ####################
  #Create standard import export line plot
  output$line <- renderPlotly({
    p <- ggplot(produce_trade %>% 
                  filter(item == input$item), 
                aes(year, 
                    value, 
                    color = element, 
                    group = element,
                    text = paste("Year:", year, "<br>",
                                 element,":", value))) + 
      geom_line() +
      scale_x_continuous(breaks = seq(2001, 2021, 1)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      scale_color_brewer(palette="Set2") +
      labs(y = "Quantity of Trade (in Tonnes)", 
           x = "Year",
           title = "U.S. Produce Trade Over Time",
           color = "Trade Type") +
      theme_minimal() + 
      theme(
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    # Convert ggplot object to plotly object
    ggplotly(p, tooltip = "text") %>%
      layout(title = NULL,
             hovermode = "x unified",
             hoverdistance = 20) 
  })
  
  ###################### U.S. TRADE BAR CHART CODE ####################
  # Import quantity chart
  output$import_bar <- renderPlotly({
    # Filter data based on year selection and import quantity
    filtered_data <- produce_trade %>% 
      filter(year == input$year_bar & element =="Import Quantity")
    
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
      ggtitle(paste("Top 10 Produce Imports in", input$year_bar)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      ) +
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
      filter(year == input$year_bar & element =="Export Quantity")
    
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
      ggtitle(paste("Top 10 Produce Exports in", input$year_bar)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      ) +
      coord_flip() +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
    # Convert ggplot to plotly
    ggplotly(chart, 
             tooltip="text") %>% config(displayModeBar = F) 
  })
  
  ###################### U.S. TRADE NETWORK VISUALIZATION CODE ####################
  output$import_net <- renderPlotly({
    
    # Filter data based on year and produce selection
    filtered_data <- trade_partners_raw %>%
      filter(year == input$year_net & item == input$produce_net & element == "Import Quantity")
    
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
      labs(title = paste(input$produce_net, "Import Trade in", input$year_net),
           color = "Country",
           subtitle = "Quantity in Tonnes") +
      theme_void()
    
    ggplotly(plot,
             tooltip = "text") %>%
      layout(hovermode = "x",
             hoverdistance = 1)
    
  })
  
  output$export_net <- renderPlotly({
    
    # Filter data based on year and produce selection
    filtered_data <- trade_partners_raw %>%
      filter(year == input$year_net & item == input$produce_net & element == "Export Quantity")
    
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
      labs(title = paste(input$produce_net, "Export Trade in", input$year_net),
           color = "Country",
           subtitle = "Quantity in Tonnes") +
      theme_void()
    
    ggplotly(plot,
             tooltip = "text") %>%
      layout(hovermode = "x",
             hoverdistance = 1)
    
  })
 
  ###################### U.S. PARTNERS TRADE IMPORT/EXPORT LINE GRAPH CODE ####################
  # Create import plot
  output$import_plot <- renderPlotly({
    # Filter data based on produce selection and element type
    filtered_data <- trade_trend %>% 
      filter(item == input$produce_line & element == "Import Quantity")
    
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
           title = paste("U.S.", input$produce_line, "Import Trade Partners Over Time"),
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
      filter(item == input$produce_line & element == "Export Quantity")
    
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
           title = paste("U.S.", input$produce_line, "Export Trade Partners Over Time"),
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
  
  ###################### U.S. PRODUCTION MAP CODE ####################
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
  
  output$mymap <- renderLeaflet({
    make_production_map2(
      fruit_veg = toupper(input$fruit2),
      if (input$fruit2 == "Apples" || input$fruit2 == "Bananas") {
        unit = "LB" 
      } else if (input$fruit2 == "Oranges, Mid & Navel, Utilized" || input$fruit2 == "Oranges, Valencia, Utilized" || input$fruit2 == "Lemons, Utilized" || input$fruit2 == "Grapefruit"|| input$fruit2 == "Grapes" || input$fruit2 == "Pears"|| input$fruit2 == "Tangerines, Utilized" || input$fruit2 == "Peaches" || input$fruit2 == "Apricots" || input$fruit2 == "Blueberries, Tame"||               
                 input$fruit2 == "Cherries, Sweet" || input$fruit2 =="Cherries, Tart" || input$fruit2 =="Raspberries" || input$fruit2 =="Dates") {
        unit = "TONS"
      }  else {
        unit = "CWT"
      },
      year = toString(input$year)
    )
  })
}

# Create Shiny app ----
shinyApp(ui, server)
