library(tidyverse)
library(shiny)
library(googlesheets4)
library(sf)
library(leaflet)
library(RSocrata)
library(htmlwidgets)
library(htmltools)
library(leaflet.providers)

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

gs4_auth(path = "tree-tool-4fc879e7b4b6.json")
  
tree_signup <- read_sheet("https://docs.google.com/spreadsheets/d/1U6HZCJe7c9WxjjaCj5A_iP1AIq209N8DJWwZc8RCHBg/edit?usp=sharing", sheet = "Sign Up")
tree_update <- read_sheet("https://docs.google.com/spreadsheets/d/1U6HZCJe7c9WxjjaCj5A_iP1AIq209N8DJWwZc8RCHBg/edit?usp=sharing", sheet = "Status Update")
tree_update_newest <- tree_update %>%
  group_by(`Tree ID (must match exactly - get the number from the map linked above)`) %>%
  slice(which.max(as.Date(`Date of Update`)))
  
trees2 <- left_join(trees, tree_signup, by = c("tree_id" = "Tree ID (must match exactly - get the number from the map linked above)"))
trees_final <- left_join(trees2, tree_update_newest, by = c("tree_id" = "Tree ID (must match exactly - get the number from the map linked above)"))
  
titlestyle <- tags$style(HTML('
.leaflet-control.map-title {
padding: 15px;
background-color: white;
border: 5px solid black;
opacity: 1;
font: 15px Arial;
color: black;
}
'))
  
legendstyle <- tags$style(HTML('
.leaflet-control.map-legend {
padding: 10px;
background-color: white;
border: 2px solid black;
opacity: 1;
font: 12px Arial;
color: black;
}
'))
  
maptitle <- tags$div(titlestyle, HTML('
<b>District 1 Tree Care Map</b><br><br>
Want to take care of a tree in District 1?<br>Select a tree to learn more.<br>Use the links below to get involved!<br><br>
<a href="https://forms.gle/EigAr15q7PXYsT1T9" target="_blank">Sign up to take care of a tree</a><br>
<a href="https://forms.gle/7jTpQzV4nVdLJo1i6" target="_blank">Update your tree&#39;s care status</a>
'))
  
maplegend <- tags$div(legendstyle, HTML('
<b>Tree Legend</b><br><br>
<svg width="10" height="10"><circle cx="5" cy="5" r="5" fill="#FDB515" /></svg>  Available<br>
<svg width="10" height="10"><circle cx="5" cy="5" r="5" fill="#028A0F" /></svg>  Already taken
'))
  
treemap <- leaflet(data = trees_final, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  setView(lng = -74.0, lat = 40.72, zoom = 15) %>%
  addControl(maptitle,  position = "topleft", className = "map-title") %>%
  addControl(maplegend,  position = "bottomright", className = "map-legend") %>%
  addCircles(lng = ~longitude, lat = ~latitude, radius = 7, stroke = FALSE, fillColor = if_else(is.na(trees2$Timestamp), "#FDB515", "#028A0F"), fillOpacity = 0.7,
              popup = ~paste(strong("Tree ID:"), tree_id, br(),
                            strong("Address:"), address, br(),
                            strong("Species:"), spc_common, br(),
                            strong("Available:"), if_else(is.na(trees_final$Timestamp.x), "Yes!", "No"), br(),
                            strong("Date of Last Check In:"), trees_final$`Date of Update`, br(),
                            strong("Last Check In:"), trees_final$`Update Details`)
  )

saveWidget(treemap, file="docs/treemap.html")
