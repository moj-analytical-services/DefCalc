#  
#  This is the server logic of a Shiny web application. You can run the
#  application by clicking 'Run App' above.
# 
#  Find out more about building applications with Shiny here:
# 
#     http://shiny.rstudio.com/
# 

library(shiny)
library(shinyWidgets)

# Server logic required to create output
shinyServer(function(session, input, output) {
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# all variables pre-fixed with 'i_' to prevent duplication with other outputs
  
# creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  i_chosenindex = reactiveValues(rownames = rownames(index_obr_qtr), data = index_obr_qtr)
  
# ensures correct dataframe is chosen for future use, based on user input
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
  
# generates correct base period dropdown menu in indices user interface, based on user input
  output$i_base <- renderUI({
    selectInput(inputId = "i_base", label = "Base Period", choices = c("Default", i_chosenindex$rownames))
  })
  
# rebases chosen index, and then creates a variable to use in output table
  observe({
    # prevents select() error because code runs too fast
    if (!is.null(input$i_base)) {
      # creates base_value to convert all values with for re-basing; equals 100 if 'Default' base selected (to not-affect index)
      base_value <- ifelse(input$i_base == "Default",
                           100,
                           index_obr_all[input$i_base, which(colnames(i_chosenindex$data) == input$i_indices) + 8]
      )
      
      # mutates index (i.e. re-bases it)
      i_chosenindex$mutate = i_chosenindex$data %>%
        mutate_at(.vars = vars(which(colnames(i_chosenindex$data) == input$i_indices) + 8), .funs = ~ 100 * . / base_value) %>%
        
        # rename 'yoy_' and 'index_' to be constant
        rename("YoY (%)" = input$i_indices) %>%
        rename("Index" = which(colnames(i_chosenindex$data) == input$i_indices) + 8) %>%
        
        # creates column for periods (e.g. 2008, 2009...)
        mutate(Period = i_chosenindex$rownames) %>%
        
        # creates flag columns for use in output table ('|' is the R 'or' function, '&' is the 'and' function)
        # is the base year, and not forecast ('round()' required for 'Default' option)
        mutate(is_base = ifelse(
          !grepl(paste(fcst_years, collapse ="|"), Period)
          & ((round(Index, digits = 2)) == 100),
          1, 0)
        ) %>%
        
        # is forecast, but not base year
        mutate(is_forecast = ifelse(
          grepl(paste(fcst_years, collapse ="|"), Period) 
          & ((round(Index, digits = 2)) != 100),
          1, 0)
        ) %>%
        
        # is base year and forecast
        mutate(is_forecast_base = ifelse(
          grepl(paste(fcst_years, collapse ="|"), Period)
          & ((round(Index, digits = 0)) == 100),
          1, 0)
        ) %>%
        
        # selects columns for output table
        select("Period", "Index", "YoY (%)", "is_base", "is_forecast", "is_forecast_base")
      
      # produces output table,
      output$i_indextable <- DT::renderDT(
        datatable(i_chosenindex$mutate, rownames = F,
                  
                  # creates display options (i.e. show '10' rows or 'All' rows)
                  options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(-1, 10), c("All", "10")), 
                                 columnDefs = list(list(visible = FALSE, targets = c(3:5)))
                  )
                  
        ) %>%
          # formats table to display 2 digits rather than all
          formatRound(columns = c(2), digits = 2) %>%
          formatRound(columns = c(3), digits = 2) %>%
          
          # aligns output values to provide readable formatting
          formatStyle(columns = c(2:3), 'text-align' = 'right') %>%
          
          # highlights row which is Base Period
          formatStyle(columns = "is_base", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightBlue'))) %>%
          # highlights rows which are forecasts
          formatStyle(columns = "is_forecast", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightYellow'))) %>%
          # highlights row which is base and forecast (if applicable)
          formatStyle(columns = "is_forecast_base", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightGreen')))
      )
    }
  })
  
  # download selected data
    output$i_download <- downloadHandler(
      filename = function() {
        paste("DASD Indexation Tool - Indices - SELECTED ", Sys.Date(), '.csv', sep = '')
      },
      content = function(con) {
        write.csv(i_chosenindex$mutate[, 1:3], con)
      }
    )
    
  # download full raw data
  output$i_downloadall <- downloadHandler(
    filename = function() {
      paste("DASD Indexation Tool - Indices - RAW ", Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(index_obr_raw, con)
    }
  )
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# all variables pre-fixed with 'dc_' to prevent duplication with other outputs
  
  # creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  dc_chosenindex = reactiveValues(rownames = rownames(index_obr_qtr), data = index_obr_qtr)
  
  # ensures correct dataframe is chosen for future use, based on user input
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

  # generates correct slider options in defcalc user interface, based on user input
    observeEvent(input$dc_period, {
    updateSliderTextInput(session = session, inputId = "dc_slider",
                           choices = dc_chosenindex$rownames, selected = dc_chosenindex$rownames[c(1, nrow(dc_chosenindex$data))])
    }, ignoreInit = TRUE)
    
  # generates dropdown options for convert from/to, based on user input
  output$dc_fromto <- renderUI({
    if (input$dc_realnom == "Real to Nominal") {
      selectInput(inputId = "dc_fromto", label = "Convert From:", choices = c(dc_chosenindex$rownames))
      } else { selectInput(inputId = "dc_fromto", label = "Convert To:", choices = c(dc_chosenindex$rownames))
      }
  })
  
  # generates the correct periods to display in table ('if' statement prevents app crashing before UI loads)
  startrow = reactiveValues()
  endrow = reactiveValues()
  
  observeEvent({input$dc_slider
                }, {
    
    if(!is.null(input$dc_slider[1]) & !is.null(input$dc_slider[2])) {
      
      startrow$dc = which(dc_chosenindex$rownames == input$dc_slider[1])
      endrow$dc = which(dc_chosenindex$rownames == input$dc_slider[2])
      
      dc_chosenindex$inputperiods = dc_chosenindex$rownames[startrow$dc:endrow$dc]
      
    } else { dc_chosenindex$inputperiods = dc_chosenindex$rownames }                 
    
  })
  
  # creates variables necessary for input table
  values_input = reactiveValues()
  dc_data = reactiveValues()
  
  # generates the basic input table...
  observeEvent({input$dc_inputrows
                input$dc_slider
                }, {

    if (is.null(values_input[["dc_data$df_input_default"]])) {
      dc_data$df_input_default = as.data.frame(matrix(0, nrow = input$dc_inputrows, ncol = length(dc_chosenindex$inputperiods)))
        
    } else {
      dc_data$df_input_default = values_input[["dc_data$df_input_default"]]
    }
    
    dc_data$df_input_default
    
  })
  
  #produces input table
  output$hot <- renderRHandsontable({
    dc_df_input = dc_data$df_input_default
    if (!is.null(dc_df_input)){
      rhandsontable(dc_df_input, 
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(dc_chosenindex$inputperiods)))
      }
    })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  dc_chosenindex$final = reactiveValues()
  dc_chosenindex$mutate = reactiveValues()
  dc_base_value = reactiveValues()

    observeEvent({input$dc_fromto
                  input$dc_indices
                  input$dc_realnom
                  input$dc_slider
                  input$dc_period
                  }, {
                    
      if(!is.null(dc_chosenindex$rownames)){
   
      #Finding the relevant base index and
   
      dc_base_value <- dc_chosenindex$data[input$dc_fromto, which(colnames(dc_chosenindex$data) == input$dc_indices) + 8]
   
      dc_chosenindex$mutate <- dc_chosenindex$data[, which(colnames(dc_chosenindex$data) == input$dc_indices) + 8]
      
      ifelse(input$dc_realnom == "Real to Nominal",
                                     
              (dc_chosenindex$final <- dc_chosenindex$mutate / dc_base_value), 

              (dc_chosenindex$final <- dc_base_value / dc_chosenindex$mutate))

      dc_chosenindex$final = t(dc_chosenindex$final)
      
      dc_chosenindex$final
      
    }
                    })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#all variables pre-fixed with 'dc_' to prevent duplication with other outputs
# 

  #generates user output table layout
  dc_data_output = reactive({
      
      dc_df_out = hot_to_r(input$hot)
      
      dc_df_output = as.data.frame(mapply('*', dc_df_out, dc_chosenindex$final[,startrow$dc:endrow$dc]))
  
 })
  
  #produces output table
  output$cold <- renderRHandsontable({
    dc_df_output = dc_data_output()
    if (!is.null(dc_df_output)) {
      rhandsontable(dc_df_output, 
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(dc_chosenindex$inputperiods)), readOnly = TRUE)
  }
  })
        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
})
