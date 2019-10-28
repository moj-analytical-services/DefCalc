#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("./idx.R")

# UI for Indexation app
ui = navbarPage("Indexation Tool",
                
                #guidance tab
                tabPanel("Guidance",
                         mainPanel(
                           h1("Guidance"), h3("Info"),
                           h2("Indices"), h3("Info"),
                           h2("Deflator Calculator"), h3("Info"),
                           h2("Placeholder"), h3("Info")
                         )
                ),
                
                #indices tab
                tabPanel("Indices",
                         sidebarPanel(
                           #dropdown menus: indices = different indices available; period = frequency of data; baseyear = year chosen as 'Index=100'
                           selectInput(inputId = "indices", label = "Index", choices = colnames(index_options)),
                           selectInput(inputId = "period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
                           uiOutput("base")
                         ),
                         mainPanel(dataTableOutput("indextable")
                         )
                ),
                
                #deflator calculator tab
                navbarMenu("Deflator Calculator",
                           #sub-tab for user to input data
                           tabPanel("Input"
                                    
                           ),
                           #sub-tab to display output data once user's input data is transformed
                           tabPanel("Output"
                                    
                           )
                           
                )
)