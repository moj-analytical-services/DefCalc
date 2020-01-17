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

ui = fluidPage(
      htmlTemplate("www/Top_B.html"),
      navbarPage("",
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFAULT GUIDANCE TAB | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tabPanel("Guidance",
          mainPanel(
            h1("Version", style = "font-weight: bold; font-family: Arial, Helvetica, sans-serif; border-bottom:1px solid black"),
            p("This version of the DASD Indexation Tool is using data published by the Office for Budget Responsibility (OBR) in the following publication:",tags$a(updatefilename, href= updateweblink, target="_blank"),".", style = "margin-left: 1em"),
            htmlTemplate("www/HTMLTEST.html"), width = 12
          )
),

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFAULT GUIDANCE TAB | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables pre-fixed with 'i_' to prevent duplication with other outputs        

tabPanel("Indices",
         
  tabsetPanel(id = "i_tabs", type = "tabs",
    
    # Guidance tab
    tabPanel("Guidance",
      p("Placeholder")),
    
    # Indices tool
    tabPanel("Tool",
      sidebarPanel(
        # Dropdown menus: indices = different indices available; period = frequency of data; baseyear = chosen base year
        selectInput(inputId = "i_indices", label = "Index", choices = colnames(index_options)),
        selectInput(inputId = "i_period", label = "Period Reference", choices = c("Financial Year", "Quarterly", "Calendar Year")),
        uiOutput("i_base"),
        downloadButton("i_download", label = "Download Displayed Data"),
        downloadButton("i_downloadall", label = "Download Full Raw Data")
      ),
      mainPanel(
        dataTableOutput("i_indextable")
      )
    )
  ) 
),
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
                
# All variables pre-fixed with 'def_' to prevent duplication with other outputs                
                
tabPanel("Deflator Calculator",
         
  tabsetPanel(id = "def_tabs", type = "tabs",
            
    # Guidance tab
    tabPanel("Guidance",
      p("Placeholder")),
    
    # Deflator Calculator Tool   
    tabPanel("Tool",
      sidebarPanel(
      # Dropdown menus:
      selectInput(inputId = "def_indices", label = "Index", choices = colnames(index_options), selected = "GDP deflator"),
      selectInput(inputId = "def_realnom", label = "Conversion: Real/Nominal", choices = c("Real to Nominal", "Nominal to Real")),
      uiOutput("def_fromto"),
      conditionalPanel(
        condition = "input.def_tables == 'Input'",
          selectInput(inputId = "def_period", label = "Period Reference", choices = c("Financial Year","Quarterly", "Calendar Year")),
          sliderTextInput(inputId = "def_slider", label = "Selected Time Period Range",
                          choices = rownames(index_obr_fy), selected = rownames(index_obr_fy)[c(1, nrow(index_obr_fy))]),
          numericInput(inputId = "def_inputrows", label = "Number of Required Rows", value = 10, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = "input.def_tables == 'Output'",
          downloadButton("def_download", label = "Download Results")
      ),
      conditionalPanel(
        condition = "input.def_tables == '% Change'",
          selectInput(inputId = "def_pchange", label = "Percentage Change", choices = c("Base-to-period", "Period-to-period"))
      )
    ),
                         
      mainPanel(
        tabsetPanel(id = "def_tables", type = "tabs",
          # Input tab for user data
          tabPanel("Input",
            rHandsontableOutput("def_hot")
          ),
          # Output tab to display transformed data
          tabPanel("Output",
            rHandsontableOutput("def_cold")
          ),
          # Output tab to display transformed data (percentage change)
          tabPanel("% Change",
            rHandsontableOutput("def_coldest")     
          )
        )
      )
    )
  )
),

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables pre-fixed with 'disc_' to prevent duplication with other outputs

tabPanel("Discount Calculator",
         
  tabsetPanel(id = "disc_tabs", type = "tabs",
                     
    # Guidance tab
    tabPanel("Guidance",
      p("Placeholder")),
                     
    # Discounting Calculator Tool   
    tabPanel("Tool",
      sidebarPanel(
      # Dropdown menus:
      selectInput(inputId = "disc_rate", label = "Discount Rate", choices = c("Standard", "Health")),
      conditionalPanel(
        condition = "input.disc_tables == 'Input'",
          selectInput(inputId = "disc_period", label = "Period Reference", choices = c("Basic", "Financial Year", "Calendar Year")),
          uiOutput("disc_periodstart"),
          uiOutput("disc_periodend"),
          #textInput(inputId = "disc_periodstart", label = "Period Start", value = 1),
          #textInput(inputId = "disc_periodend", label = "Period End", value = 10),
          numericInput(inputId = "disc_inputrows", label = "Number of Required Rows", value = 10, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = "input.disc_tables == 'Output'",
          downloadButton("disc_download", label = "Download Results"),
          downloadButton("disc_downloadraw", label = "Download Discount Rates")
      )
      ),
                              
      mainPanel(
        tabsetPanel(id = "disc_tables", type = "tabs",
          # Input tab for user data
          tabPanel("Input",
            rHandsontableOutput("disc_hot")
          ),
          # Output tab to display transformed data
          tabPanel("Output",
            rHandsontableOutput("disc_cold")
          )
        )
      )
    )
  )
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      )
)
