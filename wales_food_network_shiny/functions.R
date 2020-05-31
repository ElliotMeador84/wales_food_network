

# libraries ------------

library(shiny, quietly = T) #for shiny
library(tidyverse, quietly = T) # data wrangling
library(tidygraph, quietly = T) # graph analysis/tidy
library(visNetwork, quietly = T)




# to-nodes-edges ------------

to_nodes <- function(.x){
  
.x %>% 
  as_tibble()}


to_edges <- function(.x){
.x %>% 
   activate(edges) %>% 
   as_tibble()
} 
  