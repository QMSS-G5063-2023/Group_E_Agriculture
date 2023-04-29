library(tidyverse)
library(ggnetwork)
library(network)

# Load data
# Upload the data set and clean the names
trade_partners_raw <- read.csv('us_trade_shiny/data/FAO_data_US_agriculture_trade_quantity_2001-2021.csv')
trade_partners_raw <- clean_names(trade_partners_raw)

# Select columns of interest
trade_partners <- trade_partners_raw %>% 
  select(reporter_countries,
         partner_countries,
         element,
         item,
         year,
         value) %>% 
  filter(element == "Import Quantity" & year == 2021 &  item != "Plantains and cooking bananas")

bananas_trade <- trade_partners[grepl("Bananas", trade_partners$item, ignore.case = TRUE),]
bananas_net <- network::network(bananas_trade[, c("partner_countries", "reporter_countries")], directed = TRUE)
set.edge.attribute(bananas_net, "export_value", bananas_trade$value)

ggplot(bananas_net, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = export_value, color = vertex.names)) +
  geom_nodes(aes(color = vertex.names)) +
  geom_nodelabel(aes(label = vertex.names, color = vertex.names)) +
  labs(title="Bananas trade") +
  theme_void()

# ggplot(bananas_net, aes(x, y, xend = xend, yend = yend)) +
#   geom_edges() +
#   geom_nodelabel(aes(label = vertex.names))

# ggplot(bananas_net, aes(x, y, xend = xend, yend = yend)) +
#   geom_edges(aes(size = export_value), curvature = 0.1) +
#   geom_nodelabel(aes(label = vertex.names)) +
#   labs(title="Bananas trade") +
#   theme_void()


