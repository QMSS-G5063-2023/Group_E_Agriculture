## Shiny with working Leaflet maps
#Format: https://rstudio.github.io/leaflet/shiny.html


library(shiny)
library(leaflet)
library(scales)
library(dplyr)
library(urbnmapr)

state1 <- read.csv("data/USDA_Data/state_usda_2022_2011.csv")
state2 <- read.csv("data/USDA_Data/state_usda_2010_2002.csv")
usda_state <- rbind(state1, state2)

#make state map
usstate_sf <- get_urbn_map("states", sf = TRUE)
usstate_sf["State"] <- usstate_sf["state_name"]
usstate_sf["State"] <- toupper(usstate_sf$"State")

#make file with geometry data
usda_state_maps <- merge(usstate_sf,usda_state,by="State")
#st_transform(usda_state_maps, "+proj=longlat +datum=WGS84")

usda_state_maps$Value_numeric <- as.numeric(gsub(",","",usda_state_maps$Value))



ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  fluidRow(
    column(12,
           selectInput(
             "fruit",
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
      )
    )
    




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

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    make_production_map2(
      fruit_veg = toupper(input$fruit),
      if (input$fruit == "Apples" || input$fruit == "Bananas") {
        unit = "LB" 
      } else if (input$fruit == "Oranges, Mid & Navel, Utilized" || input$fruit == "Oranges, Valencia, Utilized" || input$fruit == "Lemons, Utilized" || input$fruit == "Grapefruit"|| input$fruit == "Grapes" || input$fruit == "Pears"|| input$fruit == "Tangerines, Utilized" || input$fruit == "Peaches" || input$fruit == "Apricots" || input$fruit == "Blueberries, Tame"||               
                 input$fruit == "Cherries, Sweet" || input$fruit =="Cherries, Tart" || input$fruit =="Raspberries" || input$fruit =="Dates") {
        unit = "TONS"
      }  else {
        unit = "CWT"
      },
      year = toString(input$year)
    )
  })
}





shinyApp(ui, server)



# Other incomplete data or needs troubleshooting:
#"Avocados",
#"Bananas",
#"Beans, Green, Lima",
#"Beans, Snap",
#"Blackberries",
#"Blueberries, Tame",
#"Blueberries, Wild",
#"Boysenberries",
#"Cabbage",
#"Cherries",
#"Cherries, Tart",
#"Cranberries",
#"Figs",
#"Grapefruit",
#"Guavas",
#"Kiwifruit",
#"Nectarines",
#"Papayas",
# "Peaches, Clingstone",
#"Peaches, Freestone",'     
#"Plums",
#"Raspberries",
#"Tangelos",
#"Tangerines"
# "Dates", (does not work for 2022) 
#"Carrots", # VARIABLE CHANGES BEFORE 2015 OR SO TIO PROCESSING OR FRESH MARKET
# "Sweet Corn", # VARIABLE CHANGES OVER YEARS
#"Cucumbers",
#"Lemons, Utilized",



