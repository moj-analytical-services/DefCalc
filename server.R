#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Server logic required to create output
shinyServer(function(input, output) {
  
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
    selectInput(inputId = "base", label = "Base Period", choices = c("Default", chosen_index$rownames))
  })
  
  #rebases chosen index, and then creates a variable to use in output table
  observe({
    #prevents select() error because code runs too fast
    if (!is.null(input$base)) {
      #creates base_value to convert all values with for re-basing; equals 100 if 'Default' base selected (to not-affect index)
        base_value <- ifelse(input$base == "Default",
        100,
        index_obr_all[input$base, which(colnames(chosen_index$data) == input$indices) + 8]
        )
      
      #mutates index (i.e. re-bases it)
      chosen_index$mutate = chosen_index$data %>%
        mutate_at(.vars = vars(which(colnames(chosen_index$data) == input$indices) + 8), .funs = ~ 100 * . / base_value) %>%
        
        #rename 'yoy_' and 'index_' to be constant
        rename("YoY (%)" = input$indices) %>%
        rename("Index" = which(colnames(chosen_index$data) == input$indices) + 8) %>%
        
        #creates column for periods (e.g. 2008, 2009...)
        mutate(Period = chosen_index$rownames) %>%
        
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
      output$indextable <- DT::renderDT(
        datatable(chosen_index$mutate, rownames = F,
                  
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
})

