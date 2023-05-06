# Load packages #########################################
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
library(ggnetwork)


#setwd('/Users/chiamakaazodo/Documents/data_vis_2023/Group_E_Agriculture/Website')

# Import data (put all your CSVs here) #########################################
#Calendar
calendar <- read.csv("data/NASS_Data/calendar.csv")

#Network data
trade_partners_raw <- read.csv('data/FAO_Data/FAO_data_US_agriculture_trade_quantity_2001-2021.csv')
trade_partners_raw <- clean_names(trade_partners_raw)

# Yield Plots
trade_yield <- read.csv('data/FAO_Data/FAO_data_US_annual_yield_production_2001-2021.csv')
trade_yield <- clean_names(trade_yield)

#State maps
state1 <- read.csv("data/USDA_Data/state_usda_2022_2011.csv")
state2 <- read.csv("data/USDA_Data/state_usda_2010_2002.csv")
usda_state <- rbind(state1, state2)


# Data processing ################################################################

######## Agricultural Production Map data processing #########
# Select months and produce of interest
df <- calendar %>% select(Program, 
                          Year, 
                          Period, 
                          Commodity, 
                          Data.Item, 
                          Value) 

df <- df %>% 
  filter(Commodity != "ARTICHOKES") %>%
  filter(Commodity != "BEETS") %>%
  filter(Commodity != "BLACKBERRIES") %>%
  filter(Commodity != "BOYSENBERRIES") %>%
  filter(Commodity != "CABBAGE") %>%
  filter(Commodity != "CUCUMBERS") %>%
  filter(Commodity != "EGGPLANT") %>%
  filter(Commodity != "GARLIC") %>%
  filter(Commodity != "GOOSEBERRIES") %>%
  filter(Commodity != "LOGANBERRIES") %>%
  filter(Commodity != "PEPPERS") %>%
  filter(Commodity != "RASPBERRIES") %>%
  filter(Commodity != "SPINACH") %>%
  filter(Commodity != "TEMPLES")

df$Price <- as.numeric(gsub(",", "", df$Value))

df$Period <- ifelse(df$Period == "JAN", "January", df$Period)
df$Period <- ifelse(df$Period == "FEB", "February", df$Period)
df$Period <- ifelse(df$Period == "MAR", "March", df$Period)
df$Period <- ifelse(df$Period == "APR", "April", df$Period)
df$Period <- ifelse(df$Period == "MAY", "May", df$Period)
df$Period <- ifelse(df$Period == "JUN", "June", df$Period)
df$Period <- ifelse(df$Period == "JUL", "July", df$Period)
df$Period <- ifelse(df$Period == "AUG", "August", df$Period)
df$Period <- ifelse(df$Period == "SEP", "September", df$Period)
df$Period <- ifelse(df$Period == "OCT", "October", df$Period)
df$Period <- ifelse(df$Period == "NOV", "November", df$Period)
df$Period <- ifelse(df$Period == "DEC", "December", df$Period)
df$Month <- factor(df$Period, levels = month.name)

# Create column in trade yield for emojis
trade_yield$image <-"data/emojis/Bud.png"

#make state map
usstate_sf <- get_urbn_map("states", sf = TRUE)
usstate_sf["State"] <- usstate_sf["state_name"]
usstate_sf["State"] <- toupper(usstate_sf$"State")

#make file with geometry data
usda_state_maps <- merge(usstate_sf,usda_state,by="State")
#st_transform(usda_state_maps, "+proj=longlat +datum=WGS84")

usda_state_maps$Value_numeric <- as.numeric(gsub(",","",usda_state_maps$Value))

######## Agriculture Trade data processing #########
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

# Functions ################################################################
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
    scale_y_continuous(labels = scales::comma) +
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
  
  navbarPage("🍉 🥝 U.S. Agricultural Exploration 🥕🌽",
             theme = shinytheme("flatly"), 
             tags$head(tags$style(HTML('.navbar-static-top {background-color: #4caf50;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: #f99976;}',
                                       '.navbar-nav>.active>a:hover {background-color: #f99976;}',
                                       '.navbar-default .navbar-nav>.active>a:focus {background-color: #f99976;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: #f99976;}',
                                       '.navbar-default .navbar-nav>li>a {color:white}',
                                       '.navbar-default .navbar-brand:hover,.navbar-default .navbar-brand:focus {color: #fcc6b3}',
                                       '.navbar-default .navbar-nav>li>a:hover,.navbar-default .navbar-nav>li>a:focus {color: #fcc6b3}',
                                       '.navbar-default .btn-link:hover,.navbar-default .btn-link:focus {color: #fcc6b3}'
                                       
             ))),
             tabPanel("Home", fluid = TRUE,
                      align="left",
                      h2('Food For Thought'),
                      p("Since the first rooster's crow, food and its associated culinary traditions have played a significant role in the culture of 
                        every major civilization. From childhood snacks we're nostalgic for, to the new cuisines we try together, food runs the gamut 
                        from the personal to the political."),
                      p("Literature has often reflected on the social and cultural significance of food. Steinbeck's classic 'Grapes of Wrath' describes 
                        the exploitation and inequities experienced by migrant farmworkers during the Great Depression. In 'To the Lighthouse', Sylvia Plath writes 
                        about the three-day stew at the center of the domestic sphere. Most recently, 'Crying in H-Mart' explores lead singer of Japanese Breakfast 
                        Michelle Zauner's relationship with her family and grief, reflecting on the ways in which food can both separate us from and connect us with others."),
                      br(),
                      img(src='grapes.jpg', 
                          align = "center",
                          style="width: 100px"),
                      img(src='lighthouse.jpg', 
                          align = "center",
                          style="width: 100px"),
                      img(src='hmart.webp', 
                          align = "center",
                          style="width: 100px"),
                      
                      br(),
                      h2("Chew It Over: U.S. Agricultural Production and Trade"),
                      p('The current project allows for exploration into the United States agricultural production, sale, and international trade.'),
                      p('1.) First, we take a look at American agricultural yield over the years.'),
                      p('2.) Then, we map produce production by geographic state.'),
                      p('3.) Next, our calendar charts the price of seasonal fruits and vegetables over the months.'),
                      p('4.) Finally, we draw patterns of food distribution, as shaped by global trade dynamics.'),
                      br(),
                      p(em("Food feeds humanity, and humanity's geopolitics impact food, with far-reaching implications.")),
                      
                      br(),
                      h3('Authors'),
                      p("Stella Wong, Chiamaka Azodo, Darci Kovacs"),
                      #br(),
                      h3("Data Sources"),
                      tags$a(href="https://www.nass.usda.gov/", "United States Department of Agriculture"), br(),
                      tags$a(href="https://www.fao.org/faostat/en/#data/TM", "Food and Agricultural Organization of the United Nations (Trade Data)"), br(),
                      tags$a(href="https://www.fao.org/faostat/en/#data/QCL", "Food and Agricultural Organization of the United Nations (Production Data)"), br(),
                      br(),
             ),
             tabPanel("U.S. Agricultural Production and Yield Trends", fluid = TRUE, 
                      h2('U.S. Agricultural Production and Yield'),
                      p(),
                      fluidRow(
                        column(width = 6,
                               selectInput(
                                 "fruit",
                                 "Choose a Produce:",
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
                                   #"Castor oil seeds",                                                        
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
                                   #"Mushrooms and truffles",                                                  
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
                                   #"Sesame seed",                                                             
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
                                   #"Tung nuts",                                                                                                             
                                   "Walnuts, in shell",                                                       
                                   "Watermelons",                                                             
                                   "Wheat"
                                 ),
                                 selected = "Apple"
                               ),
                               (column(width = 6, radioButtons(
                                 "value",
                                 "Choose an output:",
                                 choices = c("Production","Yield"),
                                 selected = "Production")))
                        )
                      ),
                      p("Select an emoji to see the values of that point. If the table is empty, please click closer to the center of the emoji icon."),
                      br(),
                      fluidRow(
                        (column(width = 6,h3("Plot"),plotOutput("fruit_plot",click = "plot_click", width = "600px", height="500px"))),
                        
                        (column(width = 6,h3("Information"),p("This plot uses data from the United Nation's Food and Agriculture Organization to show how U.S. production and yield has varied over time. Users can select a produce from the dropdown and then select either production or yield to map between 2001-2021."),tableOutput("data")))
                      )
                      
                      
             ),
             tabPanel("U.S. Agricultural Production Map", fluid = TRUE,
                      h2('State Production of Produce by Year'),
                      h3("Map"),
                      fluidRow(
                        (column(width=4,
                                sliderInput(
                                  "year",
                                  "Year:", min = 2007, max = 2022, value = 2022, step = 1, sep = "" )
                        )
                        ),
                        (column(width=6,
                                selectInput(
                                  "fruit2",
                                  "Choose a Produce:",
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
                                    "Peaches",
                                    "Pears",
                                    "Peppers, Bell",
                                    "Peppers, Chile",
                                    "Potatoes",
                                    "Pumpkins",
                                    "Squash",
                                    #"Strawberries",
                                    "Sweet Potatoes"
                                  ),
                                  selected = "Apple"
                                )
                        )
                        )
                      ),
                      br(),
                      leafletOutput("mymap"),
                      
                      br(),
                      h3("Information"),
                      p("This map is populated using USDA data generated from the 'Quick Stats' dataset tool. It includes production information at the state level between 2007 to 2022."),
                      br(),
                      p("One finding that is consistent across many of the fruits and vegetables is that as time moves forward, less and less states are populated on the map. This could be a feature of the data, perhaps the threshold for inclusion in the dataset changes over time, but could also indicate that there has been consolidation over the years in which states produce certain fruits and vegetables."),
                      br()),
             
             tabPanel("U.S. Agricultural Monthly Price Trends", fluid = TRUE, 
                      h2('U.S. Agricultural Monthly Prices'),
                      mainPanel(
                        plotOutput("ridgeline",
                                   width = "1200px", 
                                   height="1000px")
                      )
             ),
             tabPanel("U.S. Agricultural Trade", fluid = TRUE, 
                      
                      h2('U.S. Agricultural Trade Section Overview'),
                      p('This section shows U.S. Agricultural trade at the produce level and partnership level between 2001 - 2021.'),
                      
                      p('At the produce level, the visuals in this section show how user specified produce U.S. 
                        imports and exports have changed over time; and the top produce import or exports for a user specified time period.'),
                      
                      p('At the partnership level, the visuals in this section show which countries the U.S. is importing produce from and 
                        exporting produce to; the strength of the partnership/relationship signified by the quantity of trade; and how those 
                        trade relationships have changed over time.'),
                      br(),
                      h3('U.S. Produce Trade Over Time'),
                      p('This interactive line graph shows the quantity of trade, both import and export, over time for each user selected produce. 
                      A line graph was chosen to visualize this information to allow users to easily see how the relationships have changed over time. 
                      The hover interactivity shows detail on the year and both the import and export quantity at the same time so that users can compare 
                      without having to go back and forth and hover over each point to try and compare.
'),
br(),
p('Some interesting relationships shown by this graph are for cabbages which shows that in 2010 the import quantity exceeded the export 
                        quantity and that deviation became larger as time went on. Another interesting relationship shown is for avocados; the graph shows a 
                        marked increase in import that began around 2011/2012 and this is inline with when Avocado Toast and avocados in general gained unprecedented 
                        popularity and its likely that as the demand grew, the import size grew as well.'),


##### U.S. Produce Trade Import/Export Line Graph
selectInput(inputId = "item", 
            label = "Choose a Produce", 
            choices = sort(unique(produce_trade$item))), 
plotlyOutput("line"),

br(),
h3('Top 10 U.S. Produce Import and Export'),
p('These interactive bar charts show the top traded produce in the U.S. for each user selected year. The year toggle updates both the import 
                        and export chart allowing users to have consistency without having to manually update both charts. A bar graph was chosen because it quickly 
                        shows the top traded produce for each year without requiring much processing from the user and also allows for a quick visual scan of the other 
                        top traded produce. The hover was included so that users get more details into the volume without trying to guess. '),
br(),
p('The graphs shows that in 2001 the top import was bananas at over 3 million tonnes, and the largest export was maize(corn) at over 47 million tonnes. 
                        20 years later that relationship still holds where Bananas are the top import and maize is the top export. It is interesting to note that banana import 
                        quantity stayed relatively the same over the time frame roughly at 4.6 million tonnes in 2021 but Maize export increased to 70 million tonnes in 2021. 
                        For the increase in maize export, the previous line graph shows that this was not a steady increase but rather there were some fluctuating trends over time.'),

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

br(),
h3('U.S. Produce Trade Partners'),
p('This interactive network visualization shows the trade relationship between the U.S. and different countries for each user specified produce and year. 
                        The edges (lines) denote the quantity of trade from each country. This network visual was chosen to show this relationship because it allows users to 
                        get a sense of the diversity of trade relationships the U.S. has for each produce and the strength of the trade relationship (denoted by quantity) for 
                        each produce. The hover allows users to view the country and the quantity of trade for each produce. Because trade networks can become very concentrated 
                        and busy, users have the ability to focus on a country of interest using the double click functionality in the legend. This allows users to be in control 
                        of the amount of content they view. '),
p('This line graphs shows the quantity of trade for each user selected produce over time. A line graph was chosen to help users to easily see how trade relationships 
                        have changed over time. The produce toggle that updates the network graph also updated this line chart because it is likely users would want to see the full 
                        relationship for each produce they are exploring and this way they won’t need to constantly update selection across two toggles. The hover in the line graph shows 
                        the year, country, and trade quantity so that users see all necessary pieces of information easily. Similar to the network graph, users can double click a country 
                        of interest in the legend to isolate the view.'),
br(),
p('Earlier, it was shown that the top import and export for 2021 were bananas and maize respectively. The network graph shows that in 2021 for bananas, the U.S. 
                      imported over 1.9 million tonnes of their bananas from Guatemala. Other top import partners for bananas were Costa Rica (~810,000 tonnes) and Ecuador (~680,000 tonnes). 
                      The line graph for the banana import relationship shows that initially Guatemala, Costa Rica and Ecuador had similar trade relationships with the U.S. but In 2007 this 
                        changed and Guatemala steadily rose to being the largest banana import trade partner for the U.S.'),
br(),
p('The export trade network visual shows that for the top export, maize, the U.S. exports maize to a multiplicity of countries around the world; the U.S. largest trade 
                        export partner in 2021 was China at approximately 18.8 million tonnes exported. The other top maize export trade partners are Mexico (16.9 million tonnes) and 
                        Japan (11.5 million tonnes). The export trade over time line graph, reveals that the trade relationship for maize has markedly changed with China from 2019 (300,00 tonnes) to 2021 (18.8 million tonnes) 
                        with an over 5000% increase in trade volume. Similarly for Mexico and Japan, the line graph shows a spiked increase in trade volume between 2013 and 2014 that steadily 
                        increased over time.'),

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
# selectInput(inputId = "produce_line",
#             label = "Choose a Produce:",
#             choices = sort(unique(trade_partners$item))),
fluidRow(
  column(width = 6, plotlyOutput("import_plot",
                                 width = "1000px", 
                                 height="500px")),
  column(width = 6, plotlyOutput("export_plot",
                                 width = "1000px", 
                                 height="500px"))
)







             )



  ),
)

# Define server logic to display and download selected file ----
server <- function(input, output, session) {
  output$ridgeline <- renderPlot({
    
    ridgeline <- ggplot(df, aes(
      x = Month, 
      y = reorder(Commodity, desc(Commodity)),
      group = Commodity,
      alpha = .8,
      fill =  Commodity
    )) +
      geom_density_ridges(bandwidth = 1,
                          # scale = 1,
                          draw_baseline = FALSE,
      ) +
      labs(
        x = "Month",
        y = "Produce"
      ) +
      scale_y_discrete(expand = c(0, 0))
    scale_x_discrete(expand = c(0, 0))
    coord_cartesian(clip = "off")
    
    ridgeline
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
      filter(item == input$produce_net & element == "Import Quantity")
    
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
           title = paste("U.S.", input$produce_net, "Import Trade Partners Over Time"),
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
      filter(item == input$produce_net & element == "Export Quantity")
    
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
           title = paste("U.S.", input$produce_net, "Export Trade Partners Over Time"),
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
          input$fruit == "Cherries"|| input$fruit == "Grapes" || input$fruit == "Oranges" || input$fruit == "Olives" || 
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
      else if (input$fruit == "Eggplants (aubergines)") {icon_point = "Eggplants"}
      else if (input$fruit == "Kiwi fruit") {icon_point = "Kiwis"}
      else if (input$fruit == "Lemons and limes" || input$fruit == "Other citrus fruit, n.e.c.") {icon_point = "Lemons"}
      else if (input$fruit == "Maize (corn)" || input$fruit ==  "Green corn (maize)") {icon_point = "Corn"}
      else if (input$fruit == "Mangoes, guavas and mangosteens") {icon_point = "Mangoes"}
      else if (input$fruit == "Onions and shallots, dry (excluding dehydrated)") {icon_point = "Onions"}
      else if (input$fruit == "Peaches and nectarines") {icon_point = "Peaches"}
      else if (input$fruit == "Sour cherries") {icon_point = "Cherries"}
      else if (input$fruit == "Tangerines, mandarins, clementines") {icon_point = "Oranges"}
      else if (input$fruit == "Green garlic") {icon_point = "Garlic"}
      else if (input$fruit == "Sweet potatoes") {icon_point = "Sweet Potatoes"}
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
