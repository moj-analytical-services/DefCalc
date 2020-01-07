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

source("./idx.R")

# Server logic required to create output
shinyServer(function(session, input, output) {
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'i_' to prevent duplication with other outputs
  
# Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  i_chosenindex = reactiveValues(rownames = rownames(index_obr_qtr), data = index_obr_qtr)
  i_shift = reactiveValues()
  
# Ensures correct dataframe is chosen for future use, based on user input
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
  
# Generates correct base period dropdown menu in indices user interface, based on user input
  output$i_base <- renderUI({
    selectInput(inputId = "i_base", label = "Base Period", choices = c("Default", i_chosenindex$rownames))
  })
  
# Rebases chosen index, and then creates a variable to use in output table
  observe({
    # Prevents select() error because code runs too fast
    if (!is.null(input$i_base)) {
      
      i_shift = ifelse(input$i_indices == "None", 1, 8)
      
      # Creates base_value to convert all values with for re-basing; equals 100 if 'Default' base selected (to not-affect index)
      
      base_value <- ifelse(input$i_base == "Default",
                           100,
                           index_obr_all[input$i_base, which(colnames(i_chosenindex$data) == input$i_indices) + i_shift]
      )
      
      # Mutates index (i.e. re-bases it)
      i_chosenindex$mutate = i_chosenindex$data %>%
        mutate_at(.vars = vars(which(colnames(i_chosenindex$data) == input$i_indices) + i_shift), .funs = ~ 100 * . / base_value) %>%
        
        # Rename 'yoy_' and 'index_' to be constant
        rename("YoY (%)" = input$i_indices) %>% #ERROR HERE, the Index rename line below is replacing the YOY%. 
        rename("Index" = which(colnames(i_chosenindex$data) == input$i_indices) + i_shift) %>%
        
        # Creates column for periods (e.g. 2008, 2009...)
        mutate(Period = i_chosenindex$rownames) %>%
        
        # Creates flag columns for use in output table ('|' is the R 'or' function, '&' is the 'and' function)
        
        # Is the base year, and not forecast ('round()' required for 'Default' option)
        mutate(is_base = ifelse(
          !grepl(paste(year_forecast, collapse ="|"), Period)
          & ((round(Index, digits = 2)) == 100),
          1, 0)
        ) %>%
        
        # Is forecast, but not base year
        mutate(is_forecast = ifelse(
          grepl(paste(year_forecast, collapse ="|"), Period) 
          & ((round(Index, digits = 2)) != 100),
          1, 0)
        ) %>%
        
        # Is base year and forecast
        mutate(is_forecast_base = ifelse(
          grepl(paste(year_forecast, collapse ="|"), Period)
          & ((round(Index, digits = 0)) == 100),
          1, 0)
        ) %>%
        
        # Selects columns for output table
        select("Period", "Index", "YoY (%)", "is_base", "is_forecast", "is_forecast_base")
    
      # Produces output table,
      output$i_indextable <- DT::renderDT({
        datatable(i_chosenindex$mutate, rownames = F,
                  
                  # Creates display options (i.e. show '10' rows or 'All' rows)
                  options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(-1, 10), c("All", "10")), 
                                 columnDefs = list(list(visible = FALSE, targets = c(3:5)))
                  )
                  
        ) %>%
          # Formats table to display 2 digits rather than all
          formatRound(columns = c(2), digits = 2) %>%
          formatRound(columns = c(3), digits = 2) %>%
          
          # Aligns output values to provide readable formatting
          formatStyle(columns = c(2:3), 'text-align' = 'right') %>%
          
          # Highlights row which is Base Period
          formatStyle(columns = "is_base", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightBlue'))) %>%
          # Highlights rows which are forecasts
          formatStyle(columns = "is_forecast", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightYellow'))) %>%
          # Highlights row which is base and forecast (if applicable)
          formatStyle(columns = "is_forecast_base", target = 'row',
                      backgroundColor = styleEqual(c('1'), c('lightGreen')))
      })
    }
  })
  
  # Download selected data
    output$i_download <- downloadHandler(
      filename = function() {
        paste("DASD Indexation Tool - Indices - SELECTED ", Sys.Date(), '.csv', sep = '')
      },
      content = function(con) {
        write.csv(i_chosenindex$mutate[, 1:3], con)
      }
    )
    
  # Download full raw data
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
  
# All variables pre-fixed with 'dc_' to prevent duplication with other outputs
  
  # Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  dc_chosenindex = reactiveValues(rownames = rownames(index_obr_fy), data = index_obr_fy)
  
  # Ensures correct dataframe is chosen for future use, based on user input
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

  # Generates correct slider options in defcalc user interface, based on user input
    observeEvent(input$dc_period, {
    updateSliderTextInput(session = session, inputId = "dc_slider",
                           choices = dc_chosenindex$rownames, selected = dc_chosenindex$rownames[c(1, nrow(dc_chosenindex$data))])
    }, ignoreInit = TRUE)

  # Generates the correct periods to display in table ('if' statement prevents app crashing before UI loads)
  startrow = reactiveValues()
  endrow = reactiveValues()
  
  observeEvent({input$dc_slider
                }, {
    
    if(!is.null(input$dc_slider[1]) & !is.null(input$dc_slider[2])) {
      
      startrow$dc = which(dc_chosenindex$rownames == input$dc_slider[1])
      endrow$dc = which(dc_chosenindex$rownames == input$dc_slider[2])
      
      dc_chosenindex$inputperiods = dc_chosenindex$rownames[startrow$dc:endrow$dc]
      
    } else { dc_chosenindex$inputperiods = dc_chosenindex$rownames }                 
    
    output$dc_fromto <- renderUI({
      if (input$dc_realnom == "Real to Nominal") {
        selectInput(inputId = "dc_fromto", label = "Convert From (Base-year):", choices = c(dc_chosenindex$inputperiods))
      } else { selectInput(inputId = "dc_fromto", label = "Convert To (Base-year):", choices = c(dc_chosenindex$inputperiods))
      }
    })
                  
  })
  
  # Creates variables necessary for input table
  values_input = reactiveValues()
  dc_data = reactiveValues()
  
  # Generates the basic input table...
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
  
  # Produces input table
  output$hot <- renderRHandsontable({
    dc_df_input = dc_data$df_input_default
    if (!is.null(dc_df_input)){
      rhandsontable(dc_df_input, col_highlight = 2,
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(dc_chosenindex$inputperiods))) %>% hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
  }")
      }
    })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  dc_chosenindex$final = reactiveValues()
  dc_chosenindex$mutate = reactiveValues()
  dc_base_value = reactiveValues()
  dc_shift = reactiveValues()
  
  dc_download = reactiveValues()

    observeEvent({input$dc_fromto
                  input$dc_indices
                  input$dc_realnom
                  input$dc_slider
                  input$dc_period
                  }, {
                    
      if(!is.null(dc_chosenindex$rownames)){
   
      dc_shift = ifelse(input$dc_indices == "None", 1, 8)
        
      # Finding the relevant base index and:
   
      dc_base_value <- dc_chosenindex$data[input$dc_fromto, which(colnames(dc_chosenindex$data) == input$dc_indices) + dc_shift]
   
      dc_chosenindex$mutate <- dc_chosenindex$data[, which(colnames(dc_chosenindex$data) == input$dc_indices) + dc_shift]
      
      # Converting input values to output values 
      ifelse(input$dc_realnom == "Real to Nominal",
              (dc_chosenindex$final <- dc_chosenindex$mutate / dc_base_value),
              (dc_chosenindex$final <- dc_base_value / dc_chosenindex$mutate))
      
      # Transmuting the dataframe
      dc_chosenindex$final = t(dc_chosenindex$final)
      dc_chosenindex$final
      dc_chosenindex$colnumber <- which(dc_chosenindex$inputperiods == input$dc_fromto)
    
      # Creates clear rownames for output download option
      dc_download$inputrows <- 1:input$dc_inputrows
      dc_download$inputrows <- paste('Input', dc_download$inputrows, sep = ' ')
      dc_download$outputrows <- 1:input$dc_inputrows
      dc_download$outputrows <- paste('Output', dc_download$outputrows, sep = ' ')
      dc_download$indexname <- input$dc_indices
      # Length of dc_download$rownames must equal length of dc_download_df (see below) else download error
      dc_download$rownames <- unlist(rbind(list(dc_download$inputrows), list(dc_download$outputrows), dc_download$indexname, "Timestamp"))
      
    }
                    })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'dc_' to prevent duplication with other outputs

  # Generates user output table layout
  dc_data_output = reactive({
      
      dc_df_out = hot_to_r(input$hot)
      
      dc_df_output = as.data.frame(mapply('*', dc_df_out, dc_chosenindex$final[,startrow$dc:endrow$dc]))
  
 })
  
  # Produces output table
  output$cold <- renderRHandsontable({
    dc_df_output = dc_data_output()
    if (!is.null(dc_df_output)) {
      rhandsontable(dc_df_output, 
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(dc_chosenindex$inputperiods)), readOnly = TRUE) %>% hot_cols(renderer = 
                    "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0.00){
                    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'; } else if (value != 0) {td.style.color = 'black';}
                    }")
      }
  })
  
  # Produce dataframe that combines input, output and deflator index
  dc_download$date <- paste("This spreadsheet was downloaded using the DASD Indexation Tool on",
                            Sys.Date(), "at", sub("(.*-.*-.*) ","", Sys.time()), sep = " ")
  
  dc_download_df = reactive({
    
    dc_download$vector <- 0*dc_chosenindex$final[, startrow$dc:(endrow$dc-1)]
    dc_download$combine <- t(matrix(c(dc_download$date, sub(0, "", dc_download$vector))))
    
    dc_download$input = hot_to_r(input$hot)
    dc_download$output = hot_to_r(input$cold)
    dc_download$index = dc_chosenindex$final[, startrow$dc:endrow$dc]
  
    dc_download_df = rbind(dc_download$input, dc_download$output, dc_download$index, dc_download$combine)
    
    colnames(dc_download_df) <- dc_chosenindex$inputperiods
    
    rownames(dc_download_df) <- dc_download$rownames
      
    dc_download_df
    
  })
  
  # Download full data (inputs, outputs, and original deflated index)
  output$dc_download <- downloadHandler(
    filename = function() {
      paste("DASD Indexation Tool - Deflator ", Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(dc_download_df(), con, row.names = dc_download$rownames)
    }
  )
        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT (% CHANGE) | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  observeEvent(input$dc_fromto,{
               
  change = ifelse(input$dc_fromto == "Real to Nominal", "Nominal", "Real")})
               
  pc = function(x,y){(x-y)/y}
  
  fix_nan <- function(x){
    x[is.nan(x)] <- 0
    x
  }
  
  dc_data_output_pc = reactive({
    
    dc_df_out_pc = hot_to_r(input$cold)
    
    dc_df_out_pc_lag = dc_df_out_pc[,1:(length(dc_df_out_pc)-1)]
    
    dc_df_out_pc_lag = cbind(a = dc_df_out_pc[,1],dc_df_out_pc_lag)

    if(input$dc_pchange == "Base-to-period"){
    
        (dc_df_output_pc = as.data.frame(pc(dc_df_out_pc,dc_df_out_pc[,which(dc_chosenindex$inputperiods == input$dc_fromto)])))} 
    
      else {
        
        (dc_df_output_pc = as.data.frame(pc(dc_df_out_pc,dc_df_out_pc_lag)))
        
        dc_df_output_pc[is.na(dc_df_output_pc)] <- 0
        
        print(dc_df_output_pc)
            }
      })
  
  output$coldest <- renderRHandsontable({
    dc_df_output_pc = dc_data_output_pc()
    if (!is.null(dc_df_output_pc)) {
      rhandsontable(dc_df_output_pc, 
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(dc_chosenindex$inputperiods)), readOnly = TRUE) %>% hot_cols(format = "0.0%", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0.00){
    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'; } else if (value != 0) {td.style.color = 'black';}
  }") 
    }
  })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT - % CHANGE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  

})
