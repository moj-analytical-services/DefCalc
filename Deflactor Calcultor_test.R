#Necessary packages
library(dplyr)
library(shiny)
library(readr)
library(tidyverse)
#library(openxlsx)
library(DT)
library(s3tools)
library(rhandsontable)

#Download cleaned OBR data .xlsx (not currently in use)
#download_file_from_s3("alpha-sandbox/obr.xlsx", "obr.xlsx", overwrite = TRUE)
#index_obr_all <- readWorkbook("obr.xlsx", sheet = "all", colNames = TRUE, rowNames = TRUE)
#index_obr_qtr <- readWorkbook("obr.xlsx", sheet = "qtr", colNames = TRUE, rowNames = TRUE)
#index_obr_pa <- readWorkbook("obr.xlsx", sheet = "pa", colNames = TRUE, rowNames = TRUE)
#index_obr_fy <- readWorkbook("obr.xlsx", sheet = "fy", colNames = TRUE, rowNames = TRUE)

#Download cleaned OBR data .csv, and re-adds rownames
index_obr_all <- s3_path_to_full_df("alpha-sandbox/obr_all.csv")
index_obr_all <- data.frame(index_obr_all, row.names = 1)
index_obr_qtr <- s3_path_to_full_df("alpha-sandbox/obr_qtr.csv")
index_obr_qtr <- data.frame(index_obr_qtr, row.names = 1)
index_obr_pa <- s3_path_to_full_df("alpha-sandbox/obr_pa.csv")
index_obr_pa <- data.frame(index_obr_pa, row.names = 1)
index_obr_fy <- s3_path_to_full_df("alpha-sandbox/obr_fy.csv")
index_obr_fy <- data.frame(index_obr_fy, row.names = 1)

#create a list of the indices
index <- colnames(index_obr_all[,9:16])
index <- index <- gsub('.*_',"",index)
index <- as.data.frame(index)

# excel type data frame

#User interface controls
ui <- shinyUI(fluidPage(
  headerPanel('Indexation Tool'),
  #title of app
  helpText("Insert useful information/instructions here"),
  #text instructions/guidance
  sidebarPanel(width = 3,
    #dropdown menus: indices = different indices available; period = time scale for indices; baseyear = year chosen as 'Index=100'
    selectInput(
      inputId = "indices",
      label = "Index",
      choices = index$index
    ),
    selectInput(
      inputId = "period",
      label = "Period Reference",
      choices = c("Quarterly", "Calendar Year", "Financial Year")),
    
    radioButtons("useType", "Use Data Types", c("TRUE", "FALSE")),
    
    uiOutput("base")
  ),
  mainPanel(
    tabsetPanel(
    tabPanel("Indices",dataTableOutput("indextable")),
    tabPanel("Deflator Calculator - Input", rHandsontableOutput("hot", width = 350)),
    tabPanel("Deflator Calculator - Output","HOLDER")
))))

#Behind-the-scenes code
server <- function(input, output) {
  #ensures correct time scale is chosen
  chosen_index = reactiveValues(rownames = rownames(index_obr_qtr),
                                data = index_obr_qtr)
  values = reactiveValues()
  
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = data.frame(val = 1:100, bool = TRUE, nm = LETTERS[1:100],
                        dt = seq(from = Sys.Date(), by = "days", length.out = 100),
                        stringsAsFactors = F)
      else
        DF = values[["DF"]]
    }
    
    values[["DF"]] = DF
    DF
  })
  
  observeEvent(input$period, {
    if (input$period == "Quarterly") {
      chosen_index$data = index_obr_qtr
      chosen_index$rownames = rownames(index_obr_qtr)
    } else if (input$period == "Calendar Year") {
      chosen_index$data = index_obr_pa
      chosen_index$rownames = rownames(index_obr_pa)
    } else if (input$period == "Financial Year") {
      chosen_index$data = index_obr_fy
      chosen_index$rownames = rownames(index_obr_fy)
    } else {
      chosen_index$data = index_obr_all
      chosen_index$rownames = rownames(index_obr_all)
    }
  })
  
  #generates correct base period dropdown menu
  output$base <- renderUI({
    selectInput(inputId = "base",
                label = "Base Period",
                choices = chosen_index$rownames)
  })
  
  #ensures index is rebased
  observe({
    if (!is.null(input$base)) {
      #ensures correct index (and year-on-year change) is chosen
      base_value <-
        index_obr_all[input$base, which(colnames(chosen_index$data) == gsub('^',"index_",input$indices))]
      
      chosen_index$mutate = chosen_index$data %>%
        dplyr::mutate_at(.vars = vars(which(
          colnames(chosen_index$data) == gsub('^',"index_",input$indices))),
        .funs = ~ 100 *. / base_value) %>%
        dplyr::mutate(Period = chosen_index$rownames) %>%
        select(Period,
               which(colnames(chosen_index$data) == gsub('^',"index_",input$indices)),
               gsub('^',"yoy_",input$indices))
      
      #produces output table, and provides customisability
      output$indextable <- DT::renderDT(
        datatable(
          chosen_index$mutate,
          rownames = F,
          options = list(
            pageLength = -1,
            info = FALSE,
            lengthMenu = list(c(-1,10), c("All", "10"))
          )
        ) %>%
          formatRound(columns = c(2), digits = 2) %>%
          formatRound(columns = c(3), digits = 2) %>%
          formatStyle(columns = c(2:3), 'text-align' = 'right')
      )
      
      output$hot <- renderRHandsontable({
        DF = data()
        if (!is.null(DF))
          rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")})
   
    }
    
  })}
  


#Line to run the app
shinyApp(ui = ui, server = server)