# libraries -----------



library(shiny) #for shiny
library(tidyverse) # data wrangling
library(tidygraph) # graph analysis/tidy
library(broom)
library(visNetwork)
library(sf)


# type parsing ----------

if(is.character(create_sna_data$income_pre_tax)== T){
  
create_sna_data <- create_sna_data %>% 
  mutate(income_pre_tax = 
           parse_number(income_pre_tax)) 
}


wales_food_network_g <-
  create_sna_data %>%
  as_tbl_graph()  





# add id and label
alfa_network <- wales_food_network_g %>%
  mutate(id = row_number()) %>%
  select(id, label = name)

# split into ndoes and edges
nodes <- alfa_network %>%
  as_tibble()

nodes_i <- nodes %>% 
  left_join(create_sna_data, 
            by = c('label' = 'resp_name')) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(id = as.character(id))%>% 
  mutate_if(is.numeric, 
            list(~ifelse(is.na(.), 
                         median(., na.rm = T), 
                         .))) 



edges <- alfa_network %>%
  activate(edges) %>%
  as_tibble() 



 # edge color
edges_i <- edges # %>%
  # distinct(trust_score) %>%
  # mutate(color = viridis_pal()
  #        (nrow(.))) %>%
  # right_join(edges)



full_graph <- as_tbl_graph(edges_i) %>% 
  left_join(nodes_i,
            by = c('name' = 'id')) %>% 
  mutate(title = label, 
         sust_score = 
           centrality_degree(mode = 'all'),
         sust_score = rescale(sust_score, c(0, 100)),
         size = centrality_degree(mode = 'all'))  




# "title" is variable to use for tooltip


org_names <- 
  c('Organisation', 
    'Limited', 
    'Farm', 
    'Farms', 
    'Market',
    'LLC', 
    'Charity', 
    'Wales', 
    'England', 
    'Cardiff')


food_names <- 
  c('Sunrise', 
    'Better days', 
    'Three-dog', 
    'Eat-better', 
    'Food', 
    'Sustainability', 
    'Eco', 
    'Better world', 
    'Local foods', 
    'Dairy-sellers', 
    'Meat market', 
    'Food-to-home', 
    'Raiders of the lost plate', 
    'Cabbage patch', 
    'Farmer-Union', 
    'Climate watch', 
    'Flour-power', 
    'Organic forever')

org_names_100 <- tibble(food_names, 
       org_names = sample(org_names, 
               length(food_names), T)) %>% 
  cross_df() %>% 
  mutate(org_name = 
           glue("{food_names} {org_names}")) %>% 
  distinct(org_name) %>% 
  sample_n(100, replace = F) %>% 
  pull(org_name)



full_graph <- full_graph %>% 
  mutate(title = 
           glue('<p>{org_names_100}<br>Employess - {trust_score}<p>'), 
         label = org_names_100, 
         'Org Name' = label, 
         value = 75, 
         size = 75,
         shape = 'dot', 
         name = label) 



# Network metrics --------------


# org_names_df <- 
#   full_graph %>% 
#   as_tibble() %>%
#   transmute(id. = row_number(), 
#             name)
# 
# get_id <- function(org_name){
#   
#   org_names_df %>% 
#     filter(label == org_name) %>% 
#     pull(id.)
#   }
#   
# 
# full_graph %>% 
#   to_undirected() %>% 
#   mutate(name = row_number(), 
#          id = row_number()) %>% 
#   to_shortest_path(from = 4, 
#                    to = 7)

# full_graph %>% 
#   mutate(name = row_number()) %>% 
#   ggraph()+
#   geom_edge_fan(start_cap = circle(5, 'mm'), 
#                 end_cap = circle(5, 'mm'))+
#   geom_node_text(aes(label = 
#                        str_wrap(id, 10)), 
#                  size = 2)






# Shiny maps ---------------

wales_region_boundary_lon_lat <- st_read('www/wales_region_boundary_lon_lat.gpkg')


full_graph_long_lat <- read_csv('www/full_graph_long_lat.csv')




all_pop_up_lables <- 
  full_graph_long_lat %>% 
  select(name, trust_score, sust_score) %>% 
  transmute(name_i = str_to_lower(name), 
            name_i = str_replace_all(name_i, 
                                     ' ', '-'), 
            name_i = glue('{name_i}@email.com'),
            lables = 
              glue('<b>{name}<b><br>Trust score: {trust_score}<br>Sustainability score: {sust_score}<br> Contact {name_i}')) %>% 
  pull(lables)




