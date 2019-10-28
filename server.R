#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Server logic required to create output
shinyServer(function(input, output) {

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #all variables pre-fixed with 'i_' to prevent duplication with other outputs
  
  #creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  i_chosenindex = reactiveValues(rownames = rownames(index_obr_qtr), data = index_obr_qtr)
  
  #ensures correct dataframe is chosen for future use, based on user input
  observeEvent(input$i_period, {
    if (input$i_period == "Quarterly") {
      i_chosenindex$data = index_obr_qtr
      i_chosenindex$rownames = rownames(index_obr_qtr)
    } else if (input$i_period == "Calendar Year") {
      i_chosenindex$data = index_obr_pa
      i_chosenindex$rownames = rownames(index_obr_pa)
    } else if (input$i_period == "Financial Year") {
      i_chosenindex$data = index_obr_fy
      i_chosenindex$rownames = rownames(index_obr_fy)
    } else {
      i_chosenindex$data = index_obr_all
      i_chosenindex$rownames = rownames(index_obr_all)
    }
  })
  
  #generates correct base period dropdown menu in indices user interface, based on user input
  output$i_base <- renderUI({
    selectInput(inputId = "i_base", label = "Base Period", choices = c("Default", i_chosenindex$rownames))
  })
  
  #rebases chosen index, and then creates a variable to use in output table
  observe({
    #prevents select() error because code runs too fast
    if (!is.null(input$i_base)) {
      #creates base_value to convert all values with for re-basing; equals 100 if 'Default' base selected (to not-affect index)
        base_value <- ifelse(input$i_base == "Default",
        100,
        index_obr_all[input$i_base, which(colnames(i_chosenindex$data) == input$i_indices) + 8]
        )
      
      #mutates index (i.e. re-bases it)
      i_chosenindex$mutate = i_chosenindex$data %>%
        mutate_at(.vars = vars(which(colnames(i_chosenindex$data) == input$i_indices) + 8), .funs = ~ 100 * . / base_value) %>%
        
        #rename 'yoy_' and 'index_' to be constant
        rename("YoY (%)" = input$i_indices) %>%
        rename("Index" = which(colnames(i_chosenindex$data) == input$i_indices) + 8) %>%
        
        #creates column for periods (e.g. 2008, 2009...)
        mutate(Period = i_chosenindex$rownames) %>%
        
        #creates flag columns for use in output table ('|' is the R 'or' function, '&' is the 'and' function)
        #is the base year, and not forecast ('round()' required for 'Default' option)
        mutate(is_base = ifelse(
          !grepl(paste(fcst_years, collapse ="|"), Period)
          & ((round(Index, digits = 2)) == 100),
          1, 0)
        ) %>%
        
        #is forecast, but not base year
        mutate(is_forecast = ifelse(
          grepl(paste(fcst_years, collapse ="|"), Period) 
          & ((round(Index, digits = 2)) != 100),
          1, 0)
        ) %>%
        
        #is base year and forecast
        mutate(is_forecast_base = ifelse(
          grepl(paste(fcst_years, collapse ="|"), Period)
          & ((round(Index, digits = 0)) == 100),
          1, 0)
        ) %>%
        
        #selects columns for output table
        select("Period", "Index", "YoY (%)", "is_base", "is_forecast", "is_forecast_base")

      #produces output table,
      output$i_indextable <- DT::renderDT(
        datatable(i_chosenindex$mutate, rownames = F,
                  
                  #creates display options (i.e. show '10' rows or 'All' rows)
                  options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(-1, 10), c("All", "10")), 
                                 columnDefs = list(list(visible = FALSE, targets = c(3:5)))
                                 )
                  
        ) %>%
          #formats table to display 2 digits rather than all
          formatRound(columns = c(2), digits = 2) %>%
          formatRound(columns = c(3), digits = 2) %>%
          
          #aligns output values to provide readable formatting
          formatStyle(columns = c(2:3), 'text-align' = 'right') %>%
          
          #highlights row which is Base Period
          formatStyle(columns = "is_base", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightBlue'))) %>%
          #highlights rows which are forecasts
          formatStyle(columns = "is_forecast", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightYellow'))) %>%
          #highlights row which is base and forecast (if applicable)
          formatStyle(columns = "is_forecast_base", target = 'row',
                    backgroundColor = styleEqual(c('1'), c('lightGreen')))
      )
    }
  })
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #all variables pre-fixed with 'dc_' to prevent duplication with other outputs
  
  #creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  dc_chosenindex = reactiveValues(rownames = rownames(index_obr_qtr), data = index_obr_qtr)
  
  #ensures correct dataframe is chosen for future use, based on user input
  observeEvent(input$dc_period, {
    if (input$dc_period == "Quarterly") {
      dc_chosenindex$data = index_obr_qtr
      dc_chosenindex$rownames = rownames(index_obr_qtr)
    } else if (input$dc_period == "Calendar Year") {
      dc_chosenindex$data = index_obr_pa
      dc_chosenindex$rownames = rownames(index_obr_pa)
    } else if (input$dc_period == "Financial Year") {
      dc_chosenindex$data = index_obr_fy
      dc_chosenindex$rownames = rownames(index_obr_fy)
    } else {
      dc_chosenindex$data = index_obr_all
      dc_chosenindex$rownames = rownames(index_obr_all)
    }
  })
  
  #generates correct slider options in defcalc user interface, based on user input
  output$dc_slideryears <- renderUI({
    sliderTextInput(inputId = "dc_slideryears", label = "Input Time Period",
                choices = dc_chosenindex$rownames,
                selected = dc_chosenindex$rownames[c(1, nrow(dc_chosenindex$data))]
    )
  })
  
  #generates dropdown options for convert to/from, based on user input
  output$dc_from <- renderUI({
    selectInput(inputId = "dc_from", label = "Period to Convert From", choices = c(dc_chosenindex$rownames))
  })

  output$dc_to <- renderUI({
    selectInput(inputId = "dc_to", label = "Period to Convert To", choices = c(dc_chosenindex$rownames))
  })
  
})

