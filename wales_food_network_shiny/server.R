



# -------- SERVER --------



# libraries -----------


library(shiny) #for shiny
library(shinyjs)
library(tidyverse) # data wrangling
library(tidygraph) # graph analysis/tidy
library(graphlayouts)
library(visNetwork)
library(sf)
library(leaflet)
library(htmltools)
library(scales)
library(htmlwidgets)

# # source data -----------
# 
# source('mock_data.R')
# 
# # data cleaning ---------
# source('data_clean.R') # full_graph
# 
# source('backend_cleaning_prep.R')
# # Define server logic required to draw a histogram

#!#!#!#!#!#!#!#! REDO RULES
# All backend analysis is done off-site and 
# written to an .rds object that is 
# ready-to-go out of the box. 


wfnm_graph_final <- 
  read_rds('www/wfnm_graph_final.rds')

leaflet_markers <- 
  read_rds('www/leaflet_markers.rds')

wales_region_boundary_lon_lat <- st_read('www/wales_region_boundary_lon_lat.gpkg')


shinyServer(function(input, output) {
  
  observeEvent(input$resetNetwork, {
    reset("form")
  })
  
  # Reactive filter
  
  dfInput <- reactive({
    
  
    
  color_df <-   wfnm_graph_final %>% 
      as_tibble() %>% 
      mutate(blank = '') %>%
      # This ensures that there is 
      # always something passed to 
      # color vector -- fix this in the 
      # future!
      select(input$node_checkbox, blank) 
  
  
  color_df[is.na(color_df)] <- ''
  
  
  color_vector <- 
    color_df %>% 
    unite('color', 
          sep = ' ') %>% 
    pull(color)
  
  

    new_color <- 
      ifelse(str_detect(
      color_vector, '#FDE725FF'), 
           '#FDE725FF', '#2BB07FFF')
  
    if(is.null(input$node_checkbox)){
      wfnm_graph_final %>% 
        mutate(color = '#2BB07FFF', 
               name = str_to_title(name)) %>% 
        toVisNetworkData()
    } else {
      wfnm_graph_final %>% 
        mutate(
          color = new_color, 
          name = str_to_title(name)
        ) %>% 
        toVisNetworkData()
    }
    
      
      
      

  })

  
  output$network_hello <- renderVisNetwork({
    
    #################
    #### Network ####
    #################
    
    visNetwork(edges = dfInput()$edges, 
               nodes = dfInput()$nodes, 
               main = list(
                 text = 
              'Social and organisational network graph', 
                           style =
                 "font-weight: bold;
                  font-family:DejaVu Sans;
                  color:black;
                  background-color:red;
                  font-size:25px;
                  text-align:left;"
                 ), 
               submain = list(
                 text = paste('Respondents were asked to answer the following:<br>',str_wrap("Are there organisations, businesses, networks or individuals that are important in helping you to achieve your goals? In other words would it be difficult or impossible to achieve your goals without this organisation or individual?", 100)),
                 style = 
                 "color:black;
                  background-color:grey;
                  font-size:17.5px;
                  text-align:left;"
               )) %>%
      visNodes(fixed = F,
        # color = list(
        #   background = viridis_pal()(12)[12],
        #   border = viridis_pal()(12)[1],
        #   highlight = viridis_pal()(12)[8]
        # ),
        shadow = list(enabled = TRUE,
                      size = 10)
      )  %>%
      visEdges(
        color = viridis_pal()(12)[5],
        length = 1,
        width = 8,
        dashes = F ,
        smooth = T,
        shadow = T#,
        # arrows = list(to = list(
        #   enabled = TRUE,
        #   scaleFactor = 0.5
        # )
    # )
      ) %>%
      visInteraction(navigationButtons = T) %>%
      visOptions(highlightNearest =
                   list(
                     enabled = T,
                     degree = 2,
                     hover = T
                   )
                 # ,
                 # selectedBy = 'id'
                 ) %>%
      visPhysics(repulsion =
                   list(centralGravity = 0.15,
                        nodeDistance  = 1)) %>%
      visLayout(randomSeed = 12) %>%
      visExport() 
  })
  
  observe({  # this is for network focusing
    visNetworkProxy("network_hello") %>%
      visFocus(id = input$Focus,
               scale = 1,
               locked = F)
  })
  
  #############
  #### Map ####
  #############
  
    
  
  ## This is the background map that showup at all times
  output$mymap <- renderLeaflet({

    leaflet() %>%
      addTiles() %>%
      addPolygons(data=wales_region_boundary_lon_lat,
                  weight = 2,
                  fillColor = 'transparent',
                  color = viridis_pal()(12)[1],
                  highlightOptions =
                    highlightOptions(
                      color = 'yellow',
                      weight = 2,
                      bringToFront = TRUE),
                  popup = ~htmlEscape(region)) %>% 
      leaflet::addAwesomeMarkers(
        lng = leaflet_markers$longitude,#dfInput_leaf()$longitude,
        lat = leaflet_markers$latitude,#dfInput_leaf()$latitude,
        popup = leaflet_markers$label  #dfInput_leaf()$label)
        )  %>% 
      addMiniMap() %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Wales",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    }) 
  

 

  
  
  
  
})
