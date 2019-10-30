# 
#  This is the user-interface definition of a Shiny web application. You can
#  run the application by clicking 'Run App' above.
# 
#  Find out more about building applications with Shiny here:
# 
#     http://shiny.rstudio.com/
# 

library(shiny)
library(shinyWidgets)

source("./idx.R")

# UI for Indexation app

ui = 
  fluidPage(

  div(img(src = "MoJ_logo.png", width = "8%", height = "8%"), div("DASD Indexation Tool", style = "text-align: center; position: absolute; top: 3%; left: 40%; font-weight: bold; font-family: Arial, Helvetica, sans-serif"), style= {"padding: 8px ; color: black ; font-size: 150%"}),
  
  navbarPage("",
                 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
# all variables pre-fixed with 'i_' to prevent duplication with other outputs                
                
# guidance tab

  tabPanel("Guidance",
         mainPanel(
           h1("Version", style = "font-weight: bold; font-family: Arial, Helvetica, sans-serif; border-bottom:1px solid black"),
           p("This is using", temp_obr_xlsx, style = "margin-left: 1em"),
           htmlTemplate("www/HTMLTEST.html"), width = 12
         )
  ),
                
  # indices tab (inputs pre-fixed with: 'i_')
  tabPanel("Indices",
     sidebarPanel(
       # dropdown menus: indices = different indices available; period = frequency of data; baseyear = chosen base year
       selectInput(inputId = "i_indices", label = "Index", choices = colnames(index_options)),
       selectInput(inputId = "i_period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
       uiOutput("i_base")
     ),
   mainPanel(dataTableOutput("i_indextable")
   )
  ),
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
                
# all variables pre-fixed with 'dc_' to prevent duplication with other outputs                
                
tabPanel("Deflator Calculator",
  sidebarPanel(
     # dropdown menus: indices = different indices available; period = frequency of data; realnom = real or nominal adjustment; slideryears = years covered in input data
     selectInput(inputId = "dc_indices", label = "Index", choices = colnames(index_options)),
     selectInput(inputId = "dc_realnom", label = "Conversion: Real/Nominal", choices = c("Real to Nominal", "Nominal to Real")),
     uiOutput("dc_fromto"),
     selectInput(inputId = "dc_period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
     uiOutput("dc_sliderperiod")
   ),
                         
  mainPanel(
     tabsetPanel(type = "tabs",
      # input tab for user data
      tabPanel("Input",
          rHandsontableOutput("hot")
      ),
                                   
      # output tab to display transformed data
      tabPanel("Output",
          rHandsontableOutput("cold")
     )
  )
)

)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

)
)
