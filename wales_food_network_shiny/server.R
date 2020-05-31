



# -------- SERVER --------



# libraries -----------


library(shiny) #for shiny
library(shinyjs)
library(tidyverse) # data wrangling
library(tidygraph) # graph analysis/tidy
library(graphlayouts)
library(visNetwork)


# shiny-helper-functions --------
source('functions.R')

# source data -----------

source('mock_data.R')

# data cleaning ---------

source('data_clean.R') # full_graph


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  # Reactive filter
  
  dfInput <- reactive({
    
   #  set.seed(123)
   #  # layout
   # layout_df <-
   #   layout_with_stress(full_graph) %>% 
   #    as_tibble() %>% 
   #    set_names('x', 'y') %>% 
   #   mutate_all(~rescale(.,c(-1000, 1000))) 
         
      
    
    full_graph %>%
      # mutate(x = layout_df$x, 
      #        y = layout_df$y) %>% 
      filter(
        # sustainability score
        between(sust_score,
                input$sust_score[1],
                input$sust_score[2]),
        # income_pre_tax
        between(
          income_pre_tax,
          input$income_pre_tax[1],
          input$income_pre_tax[2]
        ),
        # trust score
        between(trust_score,
                input$trust_score[1],
                input$trust_score[2])
      ) %>% 
      toVisNetworkData()

  })

  
  output$network_hello <- renderVisNetwork({
    

    visNetwork(edges = dfInput()$edges, 
               nodes = dfInput()$nodes, 
               main = list(
                 text = 'Social and organisational network graph', 
                           style = 
                 "font-weight: bold;
                  font-family:DejaVu Sans;
                  color:black;
                  background-color:grey;
                  font-size:17.5px;
                  text-align:right;"), 
               submain = list(
                 text = "Nodes are linked if they have worked together in the past 12 months.", 
                            style = 
                 "font-weight: bold;
                  font-family:DejaVu Sans;
                  color:black;
                  background-color:grey;
                  font-size:15px;
                  text-align:right;")) %>%
      visNodes(fixed = F,
        color = list(
          background = viridis_pal()(12)[12],
          border = viridis_pal()(12)[1],
          highlight = viridis_pal()(12)[8]
        ),
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
                   ),
                 selectedBy = 'Org Name') %>%
      visPhysics(repulsion =
                   list(centralGravity = 0.15,
                        nodeDistance  = 1)) %>%
      visLayout(randomSeed = 12) %>%
      visExport() 
  })
  
  observe({
    visNetworkProxy("network_hello") %>%
      visFocus(id = input$Focus, scale = 1, locked = F)
  })
  

  
  
  
  
})
