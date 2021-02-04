# Libraries -------------------
library(tidyverse)
library(googlesheets4)

library(janitor)
library(scales)
library(glue)
library(here)

library(sf)
library(sp)


library(htmltools)
# source('~/OneDrive - SRUC/all_functions.R')


# Retrieve data --------------

# Data comes from my Google Drive, which I have authorised with the `googlesheets4::gs4_auth()` function.



# Wales Sustainable Food Network (Responses)

# gs4_auth(email = 'elliot.meador@gmail.com')
#  ## get the google sheet info and read it in/
# wsfn_df <- gs4_get('https://docs.google.com/spreadsheets/d/1DXhMCm2mis2RJgz_NQws7ZOM2N6ywXhgT3Uebt4fF0w/edit#gid=1600450793') %>%
#   read_sheet() %>%
#   clean_names() %>%
#   ## variable names are long! this is some renames
#
#   rename(org_name = what_is_the_name_of_your_organisation_3,
#          resp_name = what_is_your_name,
#          postcode_i = what_is_your_postcode_district_first_three_letters_number_5,
#          mission = what_do_you_your_organisation_seek_to_achieve_in_your_work_in_the_food_system_e_g_key_words_from_a_mission_statement_please_try_to_limit_to_10_words_this_will_show_up_as_a_pop_up_box_over_your_node_to_give_information)

## NOTE: There are issues with columns 'spontaniously' becoming list columns which write_csv doesn't like. I've written to a .rds wich can handle lists. This might crop up later when working with individual data types.


### ~~~~~~~~~~~~ Write data after import Google
# wsfn_df %>%
#   write_csv('wales_food_network_shiny/www/wsfn_df.csv')
#
# wsfn_df %>%
#   write_rds('wales_food_network_shiny/www/wsfn_df.rds')
### ~~~~~~~~~~~~

# NOTE: The below should work for reading data to work with to build the backend but should be careful to NOT include the 'wales_food_network_shiny' preface when inside the app. Need to comment out the one for reading in data while outside the app before closing.

# wsfn_df <-
#   read_csv('wales_food_network_shiny/www/wsfn_df.csv')


# wsfn_df <-
#   read_rds( # this one works for building
#     here(
#     'wales_food_network_shiny/www/wsfn_df.rds'
#      )
#     )

wsfn_df <-
  read_rds( # this one works for running

      'www/wsfn_df.rds'

  )



# read in postcode districts
post_dist <- st_read(
  get_data(
    'post_code/GB_postcodes/PostalDistrict.shp'
    )
  ) %>%
  mutate(postcode = str_sub(PostDist, 1, 3))



# boundaries
england_wales_boundary <-
  st_read(
    get_data('geographic/boundaries/england_wales_lower_layer_super_output/Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales.shp')
  )


# create graph -----------


new_network_names <-
  c('name',
    'relationship',
    'contact',
    'success',
    'percentage_activities',
    'share_information',
    'share_skills_labour',
    'share_assets')



network_ques <- wsfn_df %>%
  names() %>%
  str_subset('^x')



percentage_cols <- wsfn_df %>%
  names() %>%
  str_subset('percentage')

## issues with variable names changing after the big edit

# Error: object 'x1e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation' not found

wsfn_df_percentage_trial <- wsfn_df %>%
  mutate(x1e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation = as.character(x1e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation),
         x3e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation =
           as.character(x3e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation)) %>%
  unnest(cols = x1e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation)%>%
  unnest(cols = x3e_what_percentage_of_your_activities_do_you_collaborate_with_the_individual_or_organisation)

graphing_df <-
  map_df(paste0('x',1:3), function(x){
  # wsfn_df %>%
  wsfn_df_percentage_trial %>%
  select(org_name, resp_name,
         all_of(network_ques)) %>%
  select(org_name, resp_name, contains(x)) %>%
  set_names(c('org_name',
              'resp_name',
              new_network_names)) %>%
    mutate_if(is.character, str_to_title)
})%>%
  select(org_name, name, everything())



# get sustainable goals
x7_well_being_goals <- wsfn_df %>%
  select(contains('7_well_being_goals')) %>%
  names()


well_being_short <- c('Resilient',
  'Healthier',
  'More equal',
  'Cohesive communities',
  'Vibrant culture',
  'Prosperous',
  'Globally responsible')

well_being_wales_df <- wsfn_df %>%
  select(resp_name,
         org_name,
         contains(x7_well_being_goals),
         mission) %>%
  set_names(c('resp_name',
              'org_name',
              well_being_short),
            'mission') %>%
  clean_names()




html_wrap <- function(x, width = 16)
  {paste(strwrap(x, width), collapse = "<br>")}

# html_wrap(well_being_wales_df$mission, 45)

full_graph <- graphing_df %>%
  as_tbl_graph() %>%
  # join node attributes
  left_join(well_being_wales_df,
            by = c('name' = 'org_name')) %>%
  mutate_at(vars(resilient:globally_responsible),
            ~ifelse(is.na(.), 5, .)) %>%
  # add mission to node tool tip
  mutate(
    # mission = html_wrap(mission, 45),
         mission =
           ifelse(is.na(mission), 'Unknown', mission),
         title = glue('<p>About: {mission}</p>')) %>%
  activate(edges)  %>%
  # add relationship to edges tool tip
  mutate(title =
         glue('<b>Relationship:</b> {relationship}')) %>%
  activate(nodes)


# For focusing on nodes
focus_org_names <- full_graph %>%
  as_tibble() %>%
  pull(name)

focus_org_names <-
  focus_org_names[order(focus_org_names)]


# post_code_parse -------------

# 1. Identify post codes
user_post_code <-
  wsfn_df %>%
  select(
    org_name,
    resp_name,
    postcode)



user_post_ls <- post_dist %>%
  left_join(user_post_code) %>%
  drop_na(resp_name) %>%
  mutate(org_name_name =
           glue('{org_name}_{resp_name}')) %>%
  split(.$org_name_name)


user_centroid_df <-
  user_post_ls %>%
  map_df(function(x){

    # get tibble with names for merge later
    .tib <- x %>% #user_post_ls[[1]] %>%
      slice(1) %>%
      as_tibble() %>%
      select(org_name, resp_name)

    # get coordinates
   dat <-  x %>%
      slice(1) %>%
      st_centroid()%>%
     st_coordinates()


   east_north <- SpatialPoints(dat,
               proj4string=CRS("+init=epsg:27700"))


   bind_cols(.tib,
           as_tibble(spTransform(
               east_north,
                 CRS("+init=epsg:4326")))) %>%
     rename(longitude = X,
            latitude = Y)
  })



leaflet_markers <- wsfn_df %>%
  left_join(user_centroid_df,
            by = 'resp_name') %>%
  select(resp_name,
         org_name = org_name.x,
         mission,
         longitude,
         latitude)


## add leaflet markers
leaflet_markers <-
  leaflet_markers %>%
  mutate(label =
              glue('<b>Respondent Name:</b> {resp_name}<br><b>Organisation Name:</b> {org_name}<br><b>About:</b> {mission}'))


## test with Leaflet

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     data = wales_region_boundary_lon_lat, # background
#     weight = 2,
#     fillColor = 'transparent',
#     color = viridis_pal()(12)[1],
#     highlightOptions =
#       highlightOptions(
#         color = 'snow',
#         weight = 2,
#         bringToFront = TRUE
#       ),
#     popup = ~ htmlEscape(region)
#   ) %>%
#   leaflet::addAwesomeMarkers(
#     lng = leaflet_markers$longitude,
#     lat = leaflet_markers$latitude,
#     popup = leaflet_markers$label)





























