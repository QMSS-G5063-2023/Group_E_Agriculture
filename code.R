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


# Data Setup

calendar <- read.csv("data/calendar.csv")

df <- calendar %>% select(Program, Year, Period, Commodity, Data.Item, Value) 

# Define UI
ui <- fluidPage(
  # App title ----
  #titlePanel("Title of your project"),
  
  navbarPage("Farm stuff", 
             theme = shinytheme("flatly"),
             tabPanel("Chiamaka", fluid = TRUE, 
                      # code
             ),
             tabPanel("Darci", fluid = TRUE, 
                      # code
             ),
             tabPanel("Stella", fluid = TRUE, 
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
}

# Create Shiny app ----
shinyApp(ui, server)
