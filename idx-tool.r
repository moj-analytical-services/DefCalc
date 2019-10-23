#Necessary packages
library(shiny) #mandatory to create app structure
library(tidyverse) #dplyr & readr (plus other functionalities)
#library(openxlsx) #read in/manipulate Excel files
library(DT) #output table; better functionality than shiny's tables
library(s3tools) #importing data from AWS

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

#selects index drop down options needed for UI (i.e. removes duplicates)
index_options <- index_obr_all %>% select(starts_with("yoy_"))
colnames(index_options) <- substring(colnames(index_options),5)

#alters all column names in dataframes; necessary to match drop down with dataframe YoY column names
colnames(index_obr_all) <- substring(colnames(index_obr_all),5)
colnames(index_obr_qtr) <- substring(colnames(index_obr_qtr),5)
colnames(index_obr_pa) <- substring(colnames(index_obr_pa),5)
colnames(index_obr_fy) <- substring(colnames(index_obr_fy),5)

#User interface controls
ui <- shinyUI(fluidPage(
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
  mainPanel(dataTableOutput("indextable"))
))

#Behind-the-scenes code
server <- function(input, output) {
  
  #creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  chosen_index = reactiveValues(rownames = rownames(index_obr_qtr), data = index_obr_qtr)
  
  #ensures correct dataframe is chosen for future use, based on user input
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
  
  #generates correct base period dropdown menu in user interface, based on user input
  output$base <- renderUI({
    selectInput(inputId = "base", label = "Base Period", choices = chosen_index$rownames)
  })
  
  #rebases chosen index, and then creates a variable to use in output table
  observe({
    #prevents select() error because code runs too fast
    if (!is.null(input$base)) {
      #creates base_value to convert all values with for re-basing
      base_value <- index_obr_all[input$base, which(colnames(chosen_index$data) == input$indices) + 8]
      
      #mutates index (i.e. re-bases it)
      chosen_index$mutate = chosen_index$data %>%
        dplyr::mutate_at(.vars = vars(which(colnames(chosen_index$data) == input$indices) + 8), .funs = ~ 100 * . / base_value) %>%
        #rename 'yoy_' and 'index_' to be constant
        rename("YoY (%)" = input$indices) %>%
        rename("Index" = which(colnames(chosen_index$data) == input$indices) + 8) %>%
        #creates column for periods (e.g. 2008, 2009...)
        dplyr::mutate(Period = chosen_index$rownames) %>%
        #selects columns for output table
        select("Period", "Index", "YoY (%)")
      
      #produces output table,
      output$indextable <- DT::renderDT(
        datatable(chosen_index$mutate, rownames = F,
                  #creates display options (i.e. show '10' rows or 'All')
                  options = list(pageLength = 10, info = FALSE, lengthMenu = list(c(10,-1), c("10", "All")))
                  
        ) %>%
          #formats table to display 2 digits rather than all
          formatRound(columns = c(2), digits = 2) %>%
          formatRound(columns = c(3), digits = 2) %>%
          #aligns output values to provide readable formatting
          formatStyle(columns = c(2:3), 'text-align' = 'right') %>%
          #highlights row which is Base Period
          formatStyle(columns = "Index", target = 'row', backgroundColor = styleEqual(c('100'), c('lightBlue'))) #%>%
          #highlights rows which are forecasts (DOES NOT CURRENTLY WORK)
          formatStyle(columns = "Period", target = 'row',
                      backgroundColor = styleEqual(c(''), c('lightYellow')))
      ) 
    }
  })
}

#Line to run the app
shinyApp(ui = ui, server = server)