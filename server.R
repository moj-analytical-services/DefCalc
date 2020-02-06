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
library(shinyjs)
library(shinycssloaders)

source("./idx.R")

# Server logic required to create output
shinyServer(function(session, input, output) {

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'i_' to prevent duplication with other outputs
  
  # Disable inputs if Guidance tab is open; !is.null required to disable uiOutput correctly  
  observe({
    if((input$i_tabs == 'Guidance') & !is.null(input$i_base)) {
        disable("i_indices")
        disable("i_period")
        disable("i_base")
        disable("i_download")
    }
    else {
        enable("i_indices")
        enable("i_period")
        enable("i_base")
        enable("i_download")
    }
  })
  
# Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  i_chosenindex = reactiveValues(rownames = rownames(index_obr_fy), data = index_obr_fy)
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
  
# Rebases chosen index, and then creates a variable to use in output table, and creates display table
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
        rename("YoY (%)" = input$i_indices) %>%
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
        paste("DASD Indexation Tool - Indices - SELECTED - ", Sys.Date(), '.csv', sep = '')
      },
      content = function(con) {
        write.csv(i_chosenindex$mutate[, 1:3], con)
      }
    )
    
  # Download full raw data
  output$i_downloadall <- downloadHandler(
    filename = function() {
      paste("DASD Indexation Tool - Indices - RAW - ", Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(index_obr_raw, con)
    }
  )
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'def_' to prevent duplication with other outputs
  
  # Disable inputs if Guidance tab is open; !is.null required to disable uiOutput correctly  
  observe({
    if((input$def_tabs == 'Guidance') & !is.null(input$def_fromto)) {
        disable("def_indices")
        disable("def_realnom")
        disable("def_fromto")
        disable("def_period")
        disable("def_slider")
        disable("def_inputrows")
        disable("def_pchange")
        disable("def_download")
    }
    else {
        enable("def_indices")
        enable("def_realnom")
        enable("def_fromto")
        enable("def_period")
        enable("def_slider")
        enable("def_inputrows")
        enable("def_pchange")
        enable("def_download")
    }
  })
  
  # Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
  def_chosenindex = reactiveValues(rownames = rownames(index_obr_fy), data = index_obr_fy)
  
  # Ensures correct dataframe is chosen for future use, based on user input
  observeEvent(input$def_period, {
      if (input$def_period == "Quarterly") {
      def_chosenindex$data = index_obr_qtr
      def_chosenindex$rownames = rownames(index_obr_qtr)
    } else if (input$def_period == "Calendar Year") {
      def_chosenindex$data = index_obr_pa
      def_chosenindex$rownames = rownames(index_obr_pa)
    } else if (input$def_period == "Financial Year") {
      def_chosenindex$data = index_obr_fy
      def_chosenindex$rownames = rownames(index_obr_fy)
    } else {
      def_chosenindex$data = index_obr_all
      def_chosenindex$rownames = rownames(index_obr_all)
    }
      
  })

  # Generates correct slider options in defcalc user interface, based on user input
    observeEvent(input$def_period, {
    updateSliderTextInput(session = session, inputId = "def_slider",
                           choices = def_chosenindex$rownames, selected = def_chosenindex$rownames[c(1, nrow(def_chosenindex$data))])
    }, ignoreInit = TRUE)

  # Generates the correct periods to display in table ('if' statement prevents app crashing before UI loads)
  startrow = reactiveValues()
  endrow = reactiveValues()
  
  observeEvent({input$def_slider
                }, {
    
    if(!is.null(input$def_slider[1]) & !is.null(input$def_slider[2])) {
      
      startrow$dc = which(def_chosenindex$rownames == input$def_slider[1])
      endrow$dc = which(def_chosenindex$rownames == input$def_slider[2])
      
      def_chosenindex$inputperiods = def_chosenindex$rownames[startrow$dc:endrow$dc]
      
    } else { def_chosenindex$inputperiods = def_chosenindex$rownames }                 
    
    output$def_fromto <- renderUI({
      if (input$def_realnom == "Real to Nominal") {
        selectInput(inputId = "def_fromto", label = "Convert From (Base-year):", choices = c(def_chosenindex$inputperiods))
      } else { selectInput(inputId = "def_fromto", label = "Convert To (Base-year):", choices = c(def_chosenindex$inputperiods))
      }
    })
                  
  })
  
  # Creates variables necessary for input table
  def_values_input = reactiveValues()
  def_data = reactiveValues()
  
  # Generates the basic input table...
  observeEvent({input$def_inputrows
                input$def_slider
                }, {

    if (is.null(def_values_input[["def_data$df_input_default"]])) {
      def_data$df_input_default = as.data.frame(matrix(0, nrow = input$def_inputrows, ncol = length(def_chosenindex$inputperiods)))
        
    } else {
      def_data$df_input_default = def_values_input[["def_data$df_input_default"]]
    }
    
    def_data$df_input_default
    
  })
  
  # Produces input table
  output$def_hot <- renderRHandsontable({
    def_df_input = def_data$df_input_default
    if (!is.null(def_df_input)){
      rhandsontable(def_df_input, col_highlight = 2,
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(def_chosenindex$inputperiods))) %>% hot_cols(renderer = 
                    "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
                    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
                    }")
      }
    })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  def_chosenindex$final = reactiveValues()
  def_chosenindex$mutate = reactiveValues()
  def_base_value = reactiveValues()
  def_shift = reactiveValues()
  
  def_download = reactiveValues()

    observeEvent({input$def_fromto
                  input$def_indices
                  input$def_realnom
                  input$def_slider
                  input$def_period
                  }, {
                    
      if(!is.null(def_chosenindex$rownames)){
   
      def_shift = ifelse(input$def_indices == "None", 1, 8)
        
      # Finding the relevant base index and:
   
      def_base_value <- def_chosenindex$data[input$def_fromto, which(colnames(def_chosenindex$data) == input$def_indices) + def_shift]
   
      def_chosenindex$mutate <- def_chosenindex$data[, which(colnames(def_chosenindex$data) == input$def_indices) + def_shift]
      
      # Converting input values to output values 
      ifelse(input$def_realnom == "Real to Nominal",
              (def_chosenindex$final <- def_chosenindex$mutate / def_base_value),
              (def_chosenindex$final <- def_base_value / def_chosenindex$mutate))
      
      # Transmuting the dataframe
      def_chosenindex$final = t(def_chosenindex$final)
      def_chosenindex$final
      def_chosenindex$colnumber <- which(def_chosenindex$inputperiods == input$def_fromto)
    
      # Creates clear rownames for output download option
      def_download$inputrows <- 1:input$def_inputrows
      def_download$inputrows <- paste('Input', def_download$inputrows, sep = ' ')
      def_download$outputrows <- 1:input$def_inputrows
      def_download$outputrows <- paste('Output', def_download$outputrows, sep = ' ')
      def_download$indexname <- input$def_indices
      # Length of def_download$rownames must equal length of def_download_df (see below) else download error
      def_download$rownames <- unlist(rbind(list(def_download$inputrows), list(def_download$outputrows), def_download$indexname, "Timestamp"))
      
    }
                    })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'def_' to prevent duplication with other outputs

  # Generates user output table layout
  def_data_output = reactive({
      
      def_df_out = hot_to_r(input$def_hot)
      
      def_df_output = as.data.frame(mapply('*', def_df_out, def_chosenindex$final[,startrow$dc:endrow$dc]))
  
 })
  
  # Produces output table
  output$def_cold <- renderRHandsontable({
    def_df_output = def_data_output()
    if (!is.null(def_df_output)) {
      rhandsontable(def_df_output, 
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(def_chosenindex$inputperiods)), readOnly = TRUE) %>% hot_cols(renderer = 
                    "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0.00){
                    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'; } else if (value != 0) {td.style.color = 'black';}
                    }")
      }
  })
  
  # Produce dataframe that combines input, output and deflator index
  def_download$date <- paste("This spreadsheet was downloaded using the DASD Indexation Tool on",
                            Sys.Date(), "at", sub("(.*-.*-.*) ","", Sys.time()), sep = " ")
  
  def_download_df = reactive({
    
    def_download$vector <- 0*def_chosenindex$final[, startrow$dc:(endrow$dc-1)]
    def_download$combine <- t(matrix(c(def_download$date, sub(0, "", def_download$vector))))
    
    def_download$input = hot_to_r(input$def_hot)
    def_download$output = hot_to_r(input$def_cold)
    def_download$index = def_chosenindex$final[, startrow$dc:endrow$dc]
  
    def_download_df = rbind(def_download$input, def_download$output, def_download$index, def_download$combine)
    
    colnames(def_download_df) <- def_chosenindex$inputperiods
    
    rownames(def_download_df) <- def_download$rownames
      
    def_download_df
    
  })
  
  # Download full data (inputs, outputs, and original deflated index)
  output$def_download <- downloadHandler(
    filename = function() {
      paste("DASD Indexation Tool - Deflator ", Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(def_download_df(), con, row.names = def_download$rownames)
    }
  )
        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT (% CHANGE) | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables pre-fixed with 'def_' to prevent duplication with other outputs    
  
  observeEvent(input$def_fromto, {
    change = ifelse(input$def_fromto == "Real to Nominal", "Nominal", "Real")
  })
  
  # Percentage change function creation             
  pc = function(x,y){(x-y)/y}
  
  # Correct NA errors to return 0
  fix_nan <- function(x){
    x[is.nan(x)] <- 0
    x
  }
  
  # Create percentage change dataframe (inc. function)
  def_data_output_pc = reactive({
    
    def_df_out_pc = hot_to_r(input$def_cold)
    
    def_df_out_pc_lag = def_df_out_pc[, 1:(length(def_df_out_pc) - 1)]
    
    def_df_out_pc_lag = cbind(a = def_df_out_pc[, 1], def_df_out_pc_lag)

    if(input$def_pchange == "Base-to-period"){
    
        (def_df_output_pc = as.data.frame(pc(def_df_out_pc, def_df_out_pc[, which(def_chosenindex$inputperiods == input$def_fromto)])))} 
    
      else {
        
        (def_df_output_pc = as.data.frame(pc(def_df_out_pc, def_df_out_pc_lag)))
        
        def_df_output_pc[is.na(def_df_output_pc)] <- 0
        
            }
      })
  
  # Produce percentage change output table
  output$def_coldest <- renderRHandsontable({
    def_df_output_pc = def_data_output_pc()
    if (!is.null(def_df_output_pc)) {
      rhandsontable(def_df_output_pc, 
                    useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(def_chosenindex$inputperiods)), readOnly = TRUE) %>% hot_cols(format = "0.0%", renderer = 
                    "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0.00){
                    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'; } else if (value != 0) {td.style.color = 'black';}
                    }") 
    }
  })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT (% CHANGE) | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: RATE GENERATION | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  
# All variables pre-fixed with 'disc_' to prevent duplication with other outputs
  
# Discount factors, as supplied by HMT Green Book Supplementary Guidance: Discount Factors. Unexpected to change, so hard-coded.
# https://www.gov.uk/government/publications/the-green-book-appraisal-and-evaluation-in-central-governent (yes, government is spelt incorrectly as at 14.01.2020)
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/685912/Discount_Factors.xlsx
disc_standard = reactiveValues()
disc_health = reactiveValues()
  
# Create Year 0 for when generating discount rates
disc_s0 <- data.frame("Year" = 0, "SDR" = 0, "SDF" = 1)
disc_h0 <- data.frame("Year" = 0, "HDR" = 0, "HDF" = 1)
  
# Generate standard discount rates (SDR - .) and standard discount factors (SDF - %)
  
  # Create hardcoded standard discount rates, and convert to data.frame
  disc_standard <- list()
    for (short in c(1:30)) {
      disc_standard[[short]] <- 0.035
    }
    for (medium in c(31:75)) {
      disc_standard[[medium]] <- 0.03
    }
    for (long in c(76:125)) {
      disc_standard[[long]] <- 0.025
    }
  
  disc_standard <- disc_standard %>% as.numeric() %>% as.data.frame()
  colnames(disc_standard)[1] <- "SDR"
  
  # Create column for years for easy referencing
  disc_standard$Year <- unlist(list(c(1:125)))
  
  # Bind Year 0 and SDR
  disc_standard <- bind_rows(disc_s0, disc_standard)
  
  # Create standard discount factors
  for (s in 2:nrow(disc_standard)) {
    
    disc_standard$SDF[s] = (disc_standard$SDF[s-1])/(1+disc_standard$SDR[s])
  }
  
  # Split out base year
  disc_standard0 <- disc_standard
  disc_standard <- disc_standard[2:126, 1:3]
  
# Generate health discount rates (HDR - .) and health discount factors (HDF - %)
  # Create hardcoded health discount rates, and convert to data.frame
  disc_health <- list()
  for (short in c(1:30)) {
    disc_health[[short]] <- 0.015
  }
  for (medium in c(31:75)) {
    disc_health[[medium]] <- 0.0128571428571429
  }
  for (long in c(76:125)) {
    disc_health[[long]] <- 0.0107142857142857
  }
  
  disc_health <- disc_health %>% as.numeric() %>% as.data.frame()
  colnames(disc_health)[1] <- "HDR"
  
  # Create column for years for easy referencing
  disc_health$Year <- unlist(list(c(1:125)))
  
  # Bind Year 0 and HDR
  disc_health <- bind_rows(disc_h0, disc_health)
  
  # Create health discount factors
  for (h in 2:nrow(disc_health)) {
    disc_health$HDF[h] = (disc_health$HDF[h-1])/(1+disc_health$HDR[h])
  }
  
  # Split out base year
  disc_health0 <- disc_health
  disc_health <- disc_health[2:126, 1:3]
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: RATE GENERATION | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: INPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  
# All variables pre-fixed with 'disc_' to prevent duplication with other outputs
  
  # Disable inputs if Guidance tab is open; !is.null required to disable uiOutput correctly
  observe({
    if((input$disc_tabs == 'Guidance') & !is.null(input$disc_periodstart) & !is.null(input$disc_periodend)) {
        disable("disc_rate")
        disable("disc_period")
        disable("disc_periodstart")
        disable("disc_periodend")
        disable("disc_inputrows")
        disable("disc_download")
    }
    else {
        enable("disc_rate")
        enable("disc_period")
        enable("disc_periodstart")
        enable("disc_periodend")
        enable("disc_inputrows")
        enable("disc_download")
    }
  })
  
# Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
disc_chosen = reactiveValues()

#  Chooses which discount rate to use (Standard, or Health)
observeEvent(input$disc_rate, {
  if (input$disc_rate == "Standard") {
    disc_chosen$year = as.data.frame(t(disc_standard$Year))
    disc_chosen$rate = as.data.frame(t(disc_standard$SDR))
    disc_chosen$factor = as.data.frame(t(disc_standard$SDF))
  } else {
        disc_chosen$year = as.data.frame(t(disc_health$Year))
        disc_chosen$rate = as.data.frame(t(disc_health$HDR))
        disc_chosen$factor = as.data.frame(t(disc_health$HDF))
  }
})


# Generates correct slider options in defcalc user interface, based on user input
# Read carefully before making changes. Splitting prevents re-rendering errors/resets.

# Generates starting positions for each period
observeEvent({input$disc_period
              }, {

  output$disc_periodstart <- renderUI({
                if (input$disc_period == "Basic") {
                  numericInput(inputId = "disc_periodstart", label = "Start Period",
                                  value = 1, min = 1, max = 1)
                 } else if (input$disc_period == "Calendar Year") {
                   numericInput(inputId = "disc_periodstart", label = "Start Period",
                                   value = year_now
                                   , step = 1)
                 } else if (input$disc_period == "Financial Year") {
                   textInput(inputId = "disc_periodstart", label = "Start Period",
                             value = paste0(year_now - 1, "/", str_sub(year_now, -2, -1)))
                 }
  })
  
  output$disc_periodend <- renderUI({
    
                if (input$disc_period == "Basic") {
                  numericInput(inputId = "disc_periodend", label = "End Period",
                                value = 10, min = 2, max = 125)
                 } else if (input$disc_period == "Calendar Year") {
                   numericInput(inputId = "disc_periodend", label = "End Period",
                                value = year_now + 10, 
                                step = 1)
                 } else if (input$disc_period == "Financial Year") {
                  textInput(inputId = "disc_periodend", label = "End Period",
                            value = paste0(year_now + 9, "/", str_sub(year_now + 10, -2, -1)))
                }
  })

}, ignoreInit = FALSE)

# Generates new min/max positions for each period, if applicable.
observeEvent({
              input$disc_periodstart
              input$disc_periodend
              }, {
  
  if (input$disc_period == "Calendar Year") {
    updateNumericInput(session = session, inputId = "disc_periodstart",
                        value = NULL,
                        max = input$disc_periodend - 1, step = 1)
    
    updateNumericInput(session = session, inputId = "disc_periodend",
                        value = NULL,
                        min = input$disc_periodstart + 1, max = input$disc_periodstart + 125, step = 1)
  }
}, ignoreInit = TRUE)    


# Generates column headers for table
disc_chosen$columns = list() 

observeEvent({input$disc_period
              input$disc_periodstart
              input$disc_periodend}, {
                
                # Delay required to allow switching between periods without app crashing
                delay(500, {
                
  if ((input$disc_period == "Basic") | (input$disc_period == "Calendar Year")) {
    disc_chosen$collength <- as.numeric(input$disc_periodend) - as.numeric(input$disc_periodstart) + 1
    for (p in 1:disc_chosen$collength) {
        disc_chosen$columns[[p]] <- as.numeric(input$disc_periodstart) - 1 + p
    }
  } else if (input$disc_period == "Financial Year") {
      disc_chosen$collength <- as.numeric(str_sub(input$disc_periodend, 1, 4)) - as.numeric(str_sub(input$disc_periodstart, 1, 4)) + 1
      for (p in 1:disc_chosen$collength) {
        disc_chosen$columns[[p]] <- paste0(as.numeric(str_sub(input$disc_periodstart, 1, 4)) - 1 + p, "/",
                                            as.numeric(str_sub(input$disc_periodstart, -2, -1)) - 1 + p)
      }
  }
                })
})

# Creates variables necessary to generate input table
disc_values_input = reactiveValues()
disc_data = reactiveValues()

observeEvent({input$disc_inputrows
              input$disc_period
              input$disc_periodstart
              input$disc_periodend}, {
                
                # Delay required to allow switching changing start/end periods without app crashing
                delay(1000, {
              
  if (is.null(disc_values_input[["disc_data$df_input_default"]])) {
              disc_data$df_input_default = as.data.frame(matrix(0, nrow = input$disc_inputrows, ncol = disc_chosen$collength))
                  
  } else { disc_data$df_input_default = disc_values_input[["disc_data$df_input_default"]]
  }
                })
                        
})

# Produces input table
output$disc_hot <- renderRHandsontable({
  disc_df_input = disc_data$df_input_default
  if (!is.null(disc_df_input)){
    rhandsontable(disc_df_input, col_highlight = 2,
                  useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(disc_chosen$columns))) %>% hot_cols(renderer = 
                  "function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
                  td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
                  }")
  }
})
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: INPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: OUTPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    

# All variables pre-fixed with 'disc_' to prevent duplication with other outputs

# Generates user output table layout
disc_data_output = reactive({
  
  disc_df_out = hot_to_r(input$disc_hot)
  
  disc_df_output = as.data.frame(mapply('*', disc_df_out, disc_chosen$factor[, 1:disc_chosen$collength]))
  
})

# Produces output table
output$disc_cold <- renderRHandsontable({
  disc_df_output = disc_data_output()
  if (!is.null(disc_df_output)) {
    rhandsontable(disc_df_output, 
                  useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(disc_chosen$columns)), readOnly = TRUE) %>% hot_cols(renderer = 
                  "function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0.00){
                  td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'; } else if (value != 0) {td.style.color = 'black';}
                  }")
  }
})

# Produce dataframe that combines input, output, and discount rate
disc_download = reactiveValues()

# Download info:
disc_download$date <- paste("This spreadsheet was downloaded using the DASD Indexation Tool on",
                           Sys.Date(), "at", sub("(.*-.*-.*) ","", Sys.time()), sep = " ")

observeEvent({input$disc_inputrows
              input$disc_period
              input$disc_periodend
              input$disc_periodstart
              input$disc_rate}, {
                
  # Generate rownames              
  disc_download$inputrows <- 1:input$disc_inputrows
  disc_download$inputrows <- paste('Input', disc_download$inputrows, sep = ' ')
  disc_download$outputrows <- 1:input$disc_inputrows
  disc_download$outputrows <- paste('Output', disc_download$outputrows, sep = ' ')
  disc_download$factorname <- input$disc_rate
  # Length of disc_download$rownames must equal length of disc_download_df (see below) else download error
  disc_download$rownames <- unlist(rbind(list(disc_download$inputrows), list(disc_download$outputrows), disc_download$factorname, "Timestamp"))
  
})  

# Combine inputs, outputs and discount rate into one dataframe
disc_download_df = reactive({
  
  # Actual data frames
  disc_download$input = hot_to_r(input$disc_hot)
  disc_download$output = hot_to_r(input$disc_cold)
  disc_download$factor = disc_chosen$factor[, 1:ncol(disc_download$input)]
  
  # Generate one-dimensional data.frame for date to be same length
  disc_download$oned <- 0*disc_download$factor[, 2:length(disc_download$factor)]
  disc_download$datematrix <- t(matrix(c(disc_download$date, sub(0, "", disc_download$oned))))
  
  # Data frames combined, and named
  disc_download_df = rbind(disc_download$input, disc_download$output, disc_download$factor, disc_download$datematrix)
  
  colnames(disc_download_df) <- disc_chosen$columns
  rownames(disc_download_df) <- disc_download$rownames
  
  disc_download_df
  
})

# Combine base standard/health discount rates into one file
disc_download_raw <- cbind(disc_standard0, disc_health0[2:3])
disc_download_raw <- disc_download_raw %>% rename("Standard Discount Rate" = SDR, "Standard Discount Factor" = SDF,
                             "Health Discount Rate" = HDR, "Health Discount Factor" = HDF)
rownames(disc_download_raw) <- disc_standard0$Year

# Download full data (inputs, outputs, and original discount rate index)
output$disc_download <- downloadHandler(
  filename = function() {
    paste("DASD Indexation Tool - Discount Rate - USER - ", Sys.Date(), '.csv', sep = '')
  },
  content = function(con) {
    write.csv(disc_download_df(), con)
  }
)

# Download raw data (original discount rate indices)
output$disc_downloadraw <- downloadHandler(
  filename = function() {
    paste("DASD Indexation Tool - Discount Rate - RAW - ", Sys.Date(), '.csv', sep = '')
  },
  content = function(con) {
    write.csv(disc_download_raw, con, row.names = FALSE)
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: OUTPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

# Allows app to reconnect after disconnecting due to idle
  session$allowReconnect(TRUE)
    
})
