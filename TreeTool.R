library(tidyverse)
library(shiny)
library(googlesheets4)
library(sf)
library(leaflet)
library(RSocrata)
library(htmlwidgets)
library(htmltools)

trees_raw <- read.socrata(
  "https://data.cityofnewyork.us/resource/uvpi-gqnh.json?cncldist=1",
  app_token = "S16sBzqeXtOEHIDbwJrvmPBgq",
  email     = "coallerton@gmail.com",
  password  = "pLZ8ZurAbpxXE9K"
)
trees <- trees_raw %>%
  select(tree_id, spc_common, address, cb_num, nta_name, latitude, longitude) %>%
  mutate(tree_id = as.numeric(tree_id)) %>%
  mutate(latitude = as.numeric(latitude)) %>%
  mutate(longitude = as.numeric(longitude)) %>%
  mutate(spc_common = str_to_title(spc_common)) %>%
  mutate(address = str_to_title(address))

tree_signup <- read_sheet("https://docs.google.com/spreadsheets/d/1U6HZCJe7c9WxjjaCj5A_iP1AIq209N8DJWwZc8RCHBg/edit?usp=sharing", sheet = "Sign Up")
tree_update <- read_sheet("https://docs.google.com/spreadsheets/d/1U6HZCJe7c9WxjjaCj5A_iP1AIq209N8DJWwZc8RCHBg/edit?usp=sharing", sheet = "Status Update")
tree_update_newest <- tree_update %>%
  group_by(`Tree ID (must match exactly)`) %>%
  slice(which.max(as.Date(`Date of Status`)))

trees2 <- left_join(trees, tree_signup, by = c("tree_id" = "Tree ID (must match exactly)"))
trees_final <- left_join(trees2, tree_update_newest, by = c("tree_id" = "Tree ID (must match exactly)"))

maptitle <- tags$div(
  HTML('<b>District 1 Tree Care Map</b>'),
  br(),
  HTML('<a href="https://forms.gle/EigAr15q7PXYsT1T9">Sign up to steward a tree</a>'),
  br(),
  HTML('<a href="https://forms.gle/7jTpQzV4nVdLJo1i6">Update your tree status</a>')
)

treemap <- leaflet(data = trees_final, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -74.0, lat = 40.72, zoom = 14) %>%
  addControl(maptitle, position = "topleft") %>%
  addCircles(lng = ~longitude, lat = ~latitude, radius = 7, stroke = FALSE, fillColor = if_else(is.na(trees2$Timestamp), "green", "red"), fillOpacity = 0.7,
             popup = ~paste(strong("Tree ID:"), tree_id, br(),
                            strong("Address:"), address, br(),
                            strong("Species:"), spc_common, br(),
                            strong("Stewarded:"), if_else(is.na(trees_final$Timestamp.x), "No", "Yes"), br(),
                            strong("Date of Last Check In:"), trees_final$`Date of Status`, br(),
                            strong("Last Check In:"), trees_final$`Status Update`)
  )

saveWidget(treemap, file="docs/treemap.html")

