#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(visNetwork)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)

# shiny-helper-functions --------

source('functions.R')

# source data -----------

source('mock_data.R')

# data cleaning ---------

source('data_clean.R') # full_graph


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    includeCSS("www/custom.css"),
    
    # Application title
        titlePanel("Wales Food Network (prototype)"),
    navbarPage("MENU",
               tabPanel('About WLF',

h4('Introduction'),
                        h3('The Wales Food Network supports building food partnerships through mapping existing social and organisational networks in the Wales food system. This will help identify how to connect actors to enhance transformative capacity, whilst generating a complete picture of Wales’ sustainable food landscape.  There are many small-to-medium enterprises and CSOs in Wales who help to provide nutritious and sustainably produced food. These include producers (community supported agriculture, smallholders, etc.), sellers (farmers markets, local grocery stores, etc) and advocates (health organisations, gardening clubs, etc). These organisations are sometimes referred to as the missing middle.'), 
                        
h4('Project stakeholders'),
                        h3('Third sector organisations, SMEs and CSOs working in food systems, policy, health and community development are making an impact on how the food system operates in Wales. Very often, these organisations have significant financial and political capital, and are organised in such a way that they can respond quickly and effectively to new and difficult challenges. There is also a high degree of overlap of both topical and geographical coverage area by third sector organisations.')

),
    
               tabPanel("Network map",
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("Focus", tags$h6("Focus on node"),
                        choices =  org_names_100[order(org_names_100)]),
          useShinyjs(), # wrap entire UI with this
           div(id = "form",
            sliderInput(
                inputId =  "sust_score",
                label =  
    tags$h6("Derived Sustainability Score"),        
                min = 0,
                max = 100,
                value = c(0, 100)
            ),
            helpText(tags$p("Derived sustainability score is a composite score comprised of 6 indicator-questions.")),
            sliderInput(
                inputId =  "income_pre_tax",
                label =  tags$h6("Number of people impacted"),
                min = 0,
                max = 150000,
                value = c(0, 150000)
            ),
            helpText(tags$p("Identified by the question: In your best judgement, how many people has your operation/organisation positively impacted in the past year?")),
            sliderInput(
                inputId =  "trust_score",
                label =  tags$h6("Number of employees"),
                min = 0,
                max = 10,
                value = c(0, 10)
            ), 
            helpText(tags$p("Full-time equivalent (paid and volunteer)")),
            actionButton('resetAll', 
                         label = 'Reset all filters')
        )),
        
        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("network_hello",
                             height = "600px")
        )
    ))
)))







