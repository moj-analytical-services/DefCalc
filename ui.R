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

    htmlTemplate("www/Top_B.html"),
  
  navbarPage("",
                 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
# all variables pre-fixed with 'i_' to prevent duplication with other outputs                
                
# guidance tab

  tabPanel("Guidance",
         mainPanel(
           h1("Version", style = "font-weight: bold; font-family: Arial, Helvetica, sans-serif; border-bottom:1px solid black"),
           p("This version of the DASD Indexation Tool is using data published by the Office for Budget Responsibility (OBR) in the following publication:",tags$a(updatefilename, href= updateweblink, target="_blank"),".", style = "margin-left: 1em"),
           htmlTemplate("www/HTMLTEST.html"), width = 12
         )
  ),
                
  # indices tab (inputs pre-fixed with: 'i_')
  tabPanel("Indices",
     sidebarPanel(
       # dropdown menus: indices = different indices available; period = frequency of data; baseyear = chosen base year
       selectInput(inputId = "i_indices", label = "Index", choices = colnames(index_options)),
       selectInput(inputId = "i_period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
       uiOutput("i_base"),
       downloadButton("i_download", label = "Download Displayed Data"),
       downloadButton("i_downloadall", label = "Download Full Raw Data")
     ),
   mainPanel(dataTableOutput("i_indextable")
   )
  ),
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
                
# all variables pre-fixed with 'dc_' to prevent duplication with other outputs                
                
tabPanel("Deflator Calculator",
  sidebarPanel(
     p("Please select the correct options below before copying data into the 'Input Table' as changing options after may reset your data.", style = "font-weight: bold; font-family: Arial, Helvetica, sans-serif; color: red"),
     # dropdown menus: indices = different indices available; period = frequency of data; realnom = real or nominal adjustment; slideryears = years covered in input data
     selectInput(inputId = "dc_indices", label = "Index", choices = colnames(index_options)),
     selectInput(inputId = "dc_realnom", label = "Conversion: Real/Nominal", choices = c("Real to Nominal", "Nominal to Real")),
     uiOutput("dc_fromto"),
     conditionalPanel(
       condition = "input.dc_tabs == 'Input'",
        selectInput(inputId = "dc_period", label = "Period Reference", choices = c("Quarterly", "Calendar Year", "Financial Year")),
        sliderTextInput(inputId = "dc_slider", label = "Selected Time Period Range",
                        choices = rownames(index_obr_qtr), selected = rownames(index_obr_qtr)[c(1, nrow(index_obr_qtr))]),
        numericInput(inputId = "dc_inputrows", label = "Number of Required Rows", value = 10, min = 1, step = 1)
      ),
     conditionalPanel(
       condition = "input.dc_tabs == 'Output'",
        downloadButton("dc_download", label = "Download Results")
     )
   ),
                         
  mainPanel(
     tabsetPanel(id = "dc_tabs", type = "tabs",
                 
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
