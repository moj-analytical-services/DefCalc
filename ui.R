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
library(shinyjs)
library(shinycssloaders)

source("./idx.R")

# UI for Indexation app

ui = fluidPage(
      useShinyjs(),
      tags$head(includeHTML(("google-analytics.html"))),
      htmlTemplate("www/Top_B.html"),
      navbarPage("",

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFAULT GUIDANCE TAB | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tabPanel("Guidance",
            tabsetPanel(id = "g_tabs", type = "tabs",
              tabPanel("Overview",
                h1("Version", style = "font-weight: bold; font-family: Arial, Helvetica, sans-serif; border-bottom:1px solid black"),
                p("Version 7.0.6 of the DASD Indexation Tool is using data published by the Office for Budget Responsibility (OBR) in the following publication:",
                tags$a(updatefilename, href= updateweblink, target="_blank"),
                ".",
                style = "margin-left: 1em"
                ),
                htmlTemplate("www/MainPage1.html"), 
                p("Supporting guidance documentation can be found here:"),
                h5(tags$li(downloadLink("downloadToolGuidance", label = "Indexation Tool Guidance"))),
                h5(tags$li(downloadLink("downloadIndexationGuidance", label = "General Indexation Guidance"))),
                h5(tags$li(tags$a("Commercial and Contract Management Analytics (Intranet Webpage)", href = updateintranetlink, target="_blank"))),
                htmlTemplate("www/MainPage2.html"), width = 12
              ),
              tabPanel("FAQ",
                     htmlTemplate("www/FAQ.html"), width = 12
              )
            )
),

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFAULT GUIDANCE TAB | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables pre-fixed with 'i_' to prevent duplication with other outputs

tabPanel("Indices",

      sidebarPanel(
        # Dropdown menus: indices = different indices available; period = frequency of data; baseyear = chosen base year
        conditionalPanel(
          condition = "input.i_tabs == 'Tool' || input.i_tabs == 'Guidance'",
            selectInput(inputId = "i_indices", label = "Index", choices = colnames(index_options)),
            selectInput(inputId = "i_period", label = "Period Reference", choices = Period_References),
            uiOutput("i_base"),
            downloadButton("i_download", label = "Download Displayed Data"),
            downloadButton("i_downloadall", label = "Download Raw Data")
        ),
        conditionalPanel(
          condition = "input.i_tabs == 'Add Index'",
            textInput(inputId = "i_userindex", label = "Index Name", placeholder = "Provide the index name"),
            selectInput(inputId = "i_userperiod", label = "Period Reference", choices = c(Period_References)),
            uiOutput("i_userperiodstart"),
            uiOutput("i_userperiodend"),
            actionButton(inputId = "i_userupdate", label = "Generate Table"),
            actionButton(inputId = "i_useradd", label = "Update Local Index Set")
        )
      ),
      mainPanel(
        tabsetPanel(id = "i_tabs", type = "tabs",
          # Guidance tab
          tabPanel("Guidance",
                   htmlTemplate("www/IndicesTool.html"), width = 12
          ),
          # Indices table
          tabPanel("Tool",
            withSpinner(DTOutput("i_indextable"))
          ),
          # User Generated index
          tabPanel("Add Index",
            withSpinner(rHandsontableOutput("i_user"))
          )
        )
      )
  ),

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables pre-fixed with 'def_' to prevent duplication with other outputs

tabPanel("Indexation Calculator",

      sidebarPanel(
      # Dropdown menus:
        selectInput(inputId = "def_indices", label = "Index", choices = colnames(index_options), selected = "GDP deflator"),
        selectInput(inputId = "def_realnom", label = "Conversion: Real/Nominal", choices = c("Nominal to Real", "Real to Nominal")),
        uiOutput("def_fromto"),
        conditionalPanel(
          condition = "input.def_tabs == 'Input' || input.def_tabs == 'Guidance'",
            selectInput(inputId = "def_period", label = "Period Reference", choices = c(Period_References)),
            uiOutput("def_periodstart"),
            uiOutput("def_periodend"),
            numericInput(inputId = "def_inputrows", label = "Number of Required Rows", value = 10, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.def_tabs == 'Input' || input.def_tabs == 'Guidance'",
            actionButton(inputId = "def_update", label = "Generate Table")
        ),
        conditionalPanel(
          condition = "input.def_tabs == 'Output' || input.def_tabs == 'Guidance'",
            downloadButton("def_download", label = "Download Results")
        )
    ),

      mainPanel(
        tabsetPanel(id = "def_tabs", type = "tabs",
          # Guidance tab
          tabPanel("Guidance",
                   htmlTemplate("www/IndexationTool.html"), width = 12
          ),
          # Input tab for user data
          tabPanel("Input",
            withSpinner(rHandsontableOutput("def_hot"))
          ),
          # Output tab to display transformed data
          tabPanel("Output",
            withSpinner(rHandsontableOutput("def_cold"))
          )
        )
      )
),

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables pre-fixed with 'disc_' to prevent duplication with other outputs

tabPanel("Discount Calculator",

      sidebarPanel(
      # Dropdown menus:
        selectInput(inputId = "disc_rate", label = "Discount Rate", choices = c("Standard", "Health")),
        conditionalPanel(
          condition = "input.disc_tabs == 'Input' || input.disc_tabs == 'Guidance'",
            selectInput(inputId = "disc_period", label = "Period Reference", choices = c("Basic", "Financial Year", "Calendar Year")),
            uiOutput("disc_periodstart"),
            uiOutput("disc_periodend"),
            numericInput(inputId = "disc_inputrows", label = "Number of Required Rows", value = 10, min = 1, step = 1),
            actionButton(inputId = "disc_update", label = "Generate Table")
        ),
        conditionalPanel(
          condition = "input.disc_tabs == 'Output' || input.disc_tabs == 'Guidance'",
            downloadButton("disc_downloadraw", label = "Download Discount Rates"),
            downloadButton("disc_download", label = "Download Results")
        )
        ),

      mainPanel(
        tabsetPanel(id = "disc_tabs", type = "tabs",
          # Guidance tab
          tabPanel("Guidance",
                   htmlTemplate("www/DiscountingTool.html"), width = 12
          ),
          # Input tab for user data
          tabPanel("Input",
            withSpinner(rHandsontableOutput("disc_hot"))
          ),
          # Output tab to display transformed data
          tabPanel("Output",
            withSpinner(rHandsontableOutput("disc_cold"))
          )
        )
      )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      )
)
