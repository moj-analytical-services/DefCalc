#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

source("./idx.R")

# UI for Indexation app
ui = navbarPage("Indexation Tool",
                
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#all variables pre-fixed with 'i_' to prevent duplication with other outputs                
                
    #guidance tab
    tabPanel("Guidance",
       mainPanel(
        h1("Guidance"), h3("Info"),
        h2("Indices"), h3("Info"),
        h2("Deflator Calculator"), h3("Info"),
        h2("Placeholder"), h3("Info")
       )
    ),
                
    #indices tab (inputs pre-fixed with: 'i_')
    tabPanel("Indices",
       sidebarPanel(
       #dropdown menus: indices = different indices available; period = frequency of data; baseyear = chosen base year
        selectInput(inputId = "i_indices", label = "Index", choices = colnames(index_options)),
        selectInput(inputId = "i_period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
        uiOutput("i_base")
       ),
       mainPanel(dataTableOutput("i_indextable")
       )
    ),
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
                
#all variables pre-fixed with 'dc_' to prevent duplication with other outputs                
                
    tabPanel("Deflator Calculator",
      sidebarPanel(
      #dropdown menus: indices = different indices available; period = frequency of data; realnom = real or nominal adjustment; slideryears = years covered in input data
      selectInput(inputId = "dc_indices", label = "Index", choices = colnames(index_options)),
      selectInput(inputId = "dc_realnom", label = "Conversion: Real/Nominal", choices = c("Real to Nominal", "Nominal to Real")),
      selectInput(inputId = "dc_period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
      uiOutput("dc_slideryears"),
      uiOutput("dc_from"),
      uiOutput("dc_to")
      ),
      
    mainPanel(
      tabsetPanel(type = "tabs",
                  #input tab for user data
                  tabPanel("Input"),
                  
                  #output tab to display transformed data
                  tabPanel("Output")
      )
    )
    )
)
