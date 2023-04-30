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

# Data Setup

calendar <- read.csv("calendar.csv")

df <- calendar %>% select(Program, Year, Period, Commodity, Data.Item, Value) 

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

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
p <- df %>%
  ggplot( aes(y=Commodity, 
              x=Period,  
              #height=Value,
              fill=Value)) +
  geom_density_ridges(alpha=0.6, # transparency
                      stat="binline", # bins
                      binwidth = 0.5,) + 
  theme_ridges() +
  theme(legend.position="none")

}

# Create Shiny app ----
shinyApp(ui, server)
