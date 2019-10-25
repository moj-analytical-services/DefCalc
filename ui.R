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
ui = shinyUI(fluidPage(
  #title of app
  headerPanel('Indexation Tool'),
  #text instructions/guidance
  helpText("Insert useful information/instructions here"),
  sidebarPanel(
    #dropdown menus: indices = different indices available; period = frequency of data; baseyear = year chosen as 'Index=100'
    selectInput(inputId = "indices", label = "Index", choices = colnames(index_options)),
    selectInput(inputId = "period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
    uiOutput("base")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Indices", dataTableOutput("indextable")),
      tabPanel("Deflator Calculator", rHandsontableOutput("defcalc"))
    )
  )
))
