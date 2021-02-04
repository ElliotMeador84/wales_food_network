#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(tidyverse)
library(visNetwork)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(sf)
library(scales)
library(htmltools)
# shiny-helper-functions --------


# # source data -----------
# 
# source('mock_data.R')
# 
# # data cleaning ---------
# 
# source('data_clean.R') # full_graph
# 
# source('backend_cleaning_prep.R')
# 
# # Define UI for application that draws a histogram

focus_org_names <- 
    read_rds('www/focus_org_names.rds')


shinyUI(fluidPage(
    includeCSS("www/custom.css"),
    
    # Application title
    titlePanel("Wales Food Network (prototype)"),
    navbarPage(
        "MENU",
        tabPanel(
            'About WLF',
            
            h4('Introduction'),
            h3(
                'Welcome to the Wales Food Network Mapping tool. This tool is an open resource that anyone can interact with to support activities related to a sustainable and just food system in Wales. You can use the tool, for example, to search for entities working on similar objectives (e.g. food security, environmental sustainability, health) or entities working in a specific county, and the tool will show you which of these entities are already working together (as illustrated by the link between two entities).'),


h3('The Wales Food Network Mapping tool supports building food partnerships through mapping existing social and organisational networks in the Wales food system. This will help identify how to connect individuals and organisations to enhance transformative capacity, whilst generating a complete picture of Wales’ sustainable food landscape. There are many small-to-medium enterprises and CSOs in Wales who help to provide nutritious and sustainably produced food. These include producers (community supported agriculture, smallholders, etc.), sellers (farmers markets, local grocery stores, etc) and advocates (health organisations, gardening clubs, etc). These organisations are sometimes referred to as the missing middle.
'
            ),
            
            h4('How to use the Wales Food Network Mapping tool'),
            h3(
                'There are two tabs located in the navigation bar at the top: one called ‘User network’ and the other called ‘User Map’. The two tabs show the individual and organisational network linkages in a format called a social network as well as a geographical map of approximate individual and organisational locations across wales.'
            ), 
h3(
    'The User Network shows individuals and organisations as points that are connected by lines. These lines represent that a relationship exists between the two points as indicated when one of the points’ user identities completed the survey. The points may represent individuals or people who make up a larger organisation. This depends on who completed the survey data. The node labels show some organisations that did not complete the survey. For these, only their name is shown as part of a relationship – nothing else about them is included and won’t be unless they complete the survey. The labels include some information that has been collected during the survey, and in which the survey respondent agreed to have it displayed. Some respondents may not wish to have some information shown, and this is why some information is shown as ‘NA’ in the network. The buttons on the left-hand side can be used to highlight particularly interesting nodes.'
    ), 
h3(
    'The User Map shows a geographical and interactive map of Wales with pin locations of approximate respondent locations. The pins detail some information about the individual or organisation. Pin locations are only approximate and do not show the exact locations. There are two reasons for this: respondents were asked to identify part or all of their postcodes if they wanted to be included on the map. The centre of each postcode area is calculated for each respondent and this is shown on the map. The centre of postcode areas will usually appear at random places on the map – although it will be close to the actual location.'
), 
h4(
    'Our philosophy'
), 
h3(
    'We argue that organisations engaged in the promotion of sustainable food production and consumption within the food system in Wales are often faced with a collective action problem. They are acting mostly independent of one another but seek to accomplish a very similar goal: the creation of a healthy and sustainable food system. Organisations have told us that 1) they are unaware of other good work happening around the country meaning they cannot replicate success, and 2) there are inadequate public and policy awareness of their activity or potential. Our ambition is that the Network mapping tool will help identify how to connect actors to enhance transformative capacity, whilst generating a holistic picture of Wales’ sustainable food landscape.'
)
            
        ),
tabPanel('Data overview', 
         h4(
             'The Data'
         ), 
         h3(
             'The data collected from the survey will be held indefinitely but will be reviewed and updated on an annual basis, for as long as the website is live. The data may be used to produce reports, presentations, and academic publications. Your data, collected from the survey, may be shared with project team members who are authorised to work on the project and access the information. Google survey also has a data privacy policy, which you can access here: https://policies.google.com/privacy?hl=en-US. If you find you would like to later remove or edit data you have entered in the mapping tool, please email Dr Angelina Sanderson Bellamy (BellamyA1@cardiff.ac.uk). For further information about how the University may use your data, you see the Research Participants Data Protection Notice: https://www.cardiff.ac.uk/public-information/policies-and-procedures/data-protection/research-participants-data-protection-notice.'
         ), 
        h4(
            'GDPR statement and privacy notice'
        ), 
        h3(
            'The information provided on the consent form and made publicly available (via the website) will be held in compliance with GDPR regulations. Cardiff University is the data controller and the data protection officer can be reached at inforequest@cardiff.ac.uk. The contact information is being collected by Dr Angelina Sanderson Bellamy. This information will be held securely and separately from the research information you provide. Only the researcher will have access to the contact information, and it will be destroyed when the website is no longer live. The website will be reviewed on an annual basis. Every year you will have the opportunity to update your contact information, for as long as the website is live. The lawful basis for processing this information is public interest. Contact information stored will not be transferred outside of the European Economic Area (EEA).'
        ), 
        h4(
            'Contact and support'
        ), 
        h3(HTML(
            'Investigator contact details:<br>
Principal Investigator<br>                                                              
Dr Angelina Sanderson Bellamy<br>
BellamyA1@cardiff.ac.uk<br>
Tel.: +44(0)29 2087 5045<br><br>              

Elliot Meador<br>
Elliot.meador@sruc.ac.uk<br>

Please contact Cardiff School of Geography and Planning Ethics committee if you have any concerns regarding this project: Ethan Lumb, Secretary of the Ethics Committee, School of Geography and Planning, Cardiff University, Glamorgan Building, King Edward VII Avenue, Cardiff, CF10 3WA, UK; email: LumbE@cardiff.ac.uk<br><br>'
        ))
         ), 
        
        tabPanel("User network",
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                     sidebarPanel(
        selectInput("Focus",
        tags$h6("Focus on node"),
        choices =  str_to_title(focus_org_names)),
        useShinyjs(),
                         # wrap entire UI with this
                         div(
                             id = "form",
            checkboxGroupInput('node_checkbox', 
     label = tags$h6("Role in Food System"), 
     # selected = 'grower_farm_manager',
     inline = FALSE, 
     width = NULL, 
     choiceNames = c('Grower/Farm Manager', 
                     'Processor',
                     'Buyer',
                     'Supplier',
                     'Retailer',
                     'Business Owner',
                     'Community Practitioner', 
                     'Education', 
                     'Policy', 
                     'Researcher'),
   choiceValues =  c('grower_farm_manager',
                     'processor',
                     'buyer',
                     'supplier',
                     'retailer',
                     'business_owner',
                     'community_practitioner', 
                     'educator', 
                     'policy', 
                     'researcher')),
    helpText('Answers from the question: what are your/your organisation\'s role(s) in the food system?'),
                    actionButton('resetNetwork',
                      label = 'Reset all filters')
                         )
                     ),
                     # Show Network Plot
     mainPanel(visNetworkOutput("network_hello",
                                   height = "600px"))
                 )),

        tabPanel('User map',
                 sidebarLayout(
                     shiny::absolutePanel(
                     # sidebarPanel(

                     #     sliderInput(
                     #         inputId =  "fte_workers",
                     #         label =
                     #             tags$h6("Number of FTE Workers"),
                     #         min = 1,
                     #         max = 100,
                     #         value = c(1, 100)
                     #     ),
                     #     sliderInput(
                     #         inputId =  "fte_volunteers",
                     #         label =  tags$h6("Number of FTE Volunteers"),
                     #         min = 1,
                     #         max = 10,
                     #         value = c(1, 10)
                     #     ),
                     #     sliderInput(
                     #         inputId =  "num_group_member",
                     #         label =  tags$h6("Membership size"),
                     #         min = 1,
                     #         max = 2000,
                     #         value = c(1, 2000)
                     #     ),
                     #     helpText(
                     #         tags$p(
                     #             "Toggle one of the points above to switch points on."
                     #         )
                     #     ),
                     #     actionButton('resetMap',
                     #                  label = 'Reset all filters')
                     ),
                     
                     mainPanel(leafletOutput(
                         "mymap", 
                         width = '1100px', #675
                         height = '550px'
                     ))
                     

                 ))

    )
))
