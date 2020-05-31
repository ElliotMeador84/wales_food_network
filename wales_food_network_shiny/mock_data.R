# For a tidy framework
library(tidyverse)
library(glue)
library(scales)

# Our graphing libraries
library(igraph)
library(tidygraph)
library(ggraph)



library(randomNames)

create_sna_data <- 
  tibble( # let's pull 100 random names to start
    resp_name = randomNames(100,
                            which.names = 'both'))


g <- make_empty_graph() %>% 
  add_vertices(2)

# 2. Run a while loop to ensure that a connected
# graph is created -- this will help smooth over some of the graphing functions for later. 
# 
while (is.connected(g)== FALSE) {
  
  g <- create_sna_data %>% 
    mutate(info_one = sample(
      sample(resp_name, 80), # create 2nd column
      nrow(.), T)) %>%       # as subset of the
    as_tbl_graph()           # first.
}

# send it back to the original name
create_sna_data <-  g


create_sna_data <- create_sna_data %>% 
  mutate(income_pre_tax = map_chr(degree(create_sna_data), function(x){
    
    # random normal using degree as the mean
    # and a standard deviation of 2.5
    random_norm <- rnorm(n = 1, 
                         mean = x, 
                         sd = sample(2.5, 1, F))
    
    dollar(abs(random_norm)*15000, 
           prefix = '£')
  }))


influencers_df <- map_df(1:10, function(x){
  
  # pull a random node name
  node. <- sample(V(create_sna_data)$name, 1)
  
  # get the node id, because to_local_neighborhood requires a numeric identifier (this is due to igraph).
  
  node_id. <- match(node., V(create_sna_data)$name)
  
  # pull the neighbourhoods of each node from above. 
  neighbours. <- create_sna_data %>% 
    to_local_neighborhood(node = node_id., 
                          order = 1) %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    pull(name)
  
  # create a tibble of both values for use in the next step
  
  tibble(neighours. = neighbours.,
         centre = rep(node., length(neighbours.)))
  
}) 
# create new variable for each value returned above.

create_sna_data <- 
  create_sna_data %>% 
  mutate(buy_farm_mark = case_when(
    name %in% influencers_df$centre ~ 'Every meal',
    name %in% influencers_df$neighours. ~ 'Most meals',
    T ~ 'Hardly any meals'
  ), 
  buy_farm_mark = factor(buy_farm_mark, 
                         levels = c('Every meal', 
                                    'Most meals', 
                                    'Hardly any meals')))



create_sna_data <- create_sna_data %>% 
  to_undirected() %>% 
  mutate(cows_on_farm = 
           as.factor(group_infomap()))



create_sna_data <- create_sna_data %>% 
  mutate(trust_score = round(
    rescale(
      as.numeric(cows_on_farm), 
      c(1, 10))))



name_id_df <- create_sna_data %>% 
  as_tibble() %>% 
  transmute(name, 
            value = row_number())

create_sna_data <- create_sna_data %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  gather(key, value) %>% 
  left_join(name_id_df) %>% 
  split(.$key) %>% 
  bind_cols() %>% 
  select(resp_name = name, 
         recieve_info = name1) %>% 
  bind_cols(create_sna_data %>% 
              as_tibble() %>% 
              select(-name))



