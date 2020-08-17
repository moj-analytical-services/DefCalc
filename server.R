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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE: MAIN | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'i_' to prevent duplication with other outputs
  
# Disable inputs if Guidance tab is open; !is.null required to disable uiOutput correctly  
observe({
  
  if({
    input$i_tabs == 'Guidance' &
    !is.null(input$i_base)  
    })
    {
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
i_chosenindex = reactiveValues(index = index_obr_fy_index,
                               yoy = index_obr_fy_yoy,
                               rownames = rownames(index_obr_fy),
                               indexnames = colnames(index_obr_fy_yoy)
                               )
  
# Ensures correct dataframe is chosen for future use, based on user input
# Generates starting positions for each index
index_qtr = reactiveValues(index = index_obr_qtr_index,
                            yoy = index_obr_qtr_yoy,
                            rownames = rownames(index_obr_qtr),
                            indexnames = colnames(index_obr_qtr_yoy)
                          )

index_pa = reactiveValues(index = index_obr_pa_index,
                          yoy = index_obr_pa_yoy,
                          rownames = rownames(index_obr_pa),
                          indexnames = colnames(index_obr_pa_yoy)
                          )

index_fy = reactiveValues(index = index_obr_fy_index,
                          yoy = index_obr_fy_yoy,
                          rownames = rownames(index_obr_fy),
                          indexnames = colnames(index_obr_pa_yoy)
                          )

observe({                
                
  # Select the correct period
  if ({
      input$i_period == "Quarterly"
      })
      {
        i_chosenindex$index = index_qtr$index
        i_chosenindex$yoy = index_qtr$yoy
        i_chosenindex$rownames = rownames(index_qtr$index)
        i_chosenindex$indexnames = index_qtr$indexnames
      }
      else if ({
      input$i_period == "Calendar Year"  
      })
      {
        i_chosenindex$index = index_pa$index
        i_chosenindex$yoy = index_pa$yoy
        i_chosenindex$rownames = rownames(index_pa$index)
        i_chosenindex$indexnames = index_pa$indexnames
      }
      else if ({
      input$i_period == "Financial Year"
      })
      {
        i_chosenindex$index = index_fy$index
        i_chosenindex$yoy = index_fy$yoy
        i_chosenindex$rownames = rownames(index_fy$index)
        i_chosenindex$indexnames = index_fy$indexnames
      }
  
})
  
# Generates correct base period dropdown menu in indices user interface, based on user input
output$i_base <- renderUI({
  
  selectInput(inputId = "i_base", label = "Base Period",
              choices = c("Default", i_chosenindex$rownames[which(!is.na(i_chosenindex$index[, which(input$i_indices == i_chosenindex$indexnames)]))])
              )
  
})


observe({
  
  updateSelectInput(session = session, 
                    inputId = "i_indices",
                    choices = i_chosenindex$indexnames
                    )
  
})

observe({
                
  if ({
      grepl("User Index:", input$i_indices)
      })
      {
        disable("i_period")
      }
      else {
        enable("i_period")
      }
                
})
  
# Rebases chosen index, and then creates a variable to use in output table, and creates display table
i_shift = reactiveValues()

observe({
  
  # Prevents select() error because code runs too fast
  if ({
      !is.null(input$i_base)
      })
      {
      # Creates base_value to convert all values with for re-basing; equals 100 if 'Default' base selected (to not-affect index)
      i_shift = which(colnames(i_chosenindex$yoy) == input$i_indices)
      
      base_value <- if ({
                        input$i_base == "Default"
                        })
                        {
                          100
                        }
                        else {
                          i_chosenindex$index[which(i_chosenindex$rownames == input$i_base),
                                              i_shift]
                        }
      
      # Mutates index (i.e. re-bases it)
      i_chosenindex$mutateindex = i_chosenindex$index %>%
        mutate_at(.vars = vars(i_shift),
                  .funs = ~ 100 * . / base_value) %>%
        
        # Rename to be constant
        rename("Index" = i_shift) %>%
        
        # Creates column for periods (e.g. 2008, 2009...)
        mutate(Period = i_chosenindex$rownames) %>%
        
        # Creates flag columns for use in output table ('|' is the R 'or' function, '&' is the 'and' function):
        
        # Is the base year, and not forecast ('round()' required for 'Default' option)
        mutate(is_base = ifelse ({
                                !grepl(paste(year_forecast, collapse ="|"), Period) &
                                (round(Index, digits = 2)) == 100
                                }
                                ,
                                  1
                                ,
                                  0
                                )
              ) %>%
        
        # Is forecast, but not base year
        mutate(is_forecast = ifelse ({
                                    grepl(paste(year_forecast, collapse ="|"), Period) &
                                    (round(Index, digits = 2)) != 100
                                    }
                                    ,
                                      1
                                    ,
                                      0
                                    )
              ) %>%
        
        # Is base year and forecast
        mutate(is_forecast_base = ifelse ({
                                          grepl(paste(year_forecast, collapse ="|"), Period) &
                                          (round(Index, digits = 0)) == 100
                                          }
                                          ,
                                            1
                                          ,
                                            0
                                          )
              )
      
      # Renames year-on-year columns
      i_chosenindex$mutateyoy = i_chosenindex$yoy %>%
        rename("YoY (%)" = i_shift)
      
      i_chosenindex$mutate <- cbind(i_chosenindex$mutateyoy, i_chosenindex$mutateindex) %>%
      
        # Selects columns for output table
        select("Period",
               "Index",
               "YoY (%)",
               "is_base",
               "is_forecast",
               "is_forecast_base"
               ) %>%
        
      remove_missing(na.rm = TRUE)
    
      # Produces output table,
      output$i_indextable <- DT::renderDT({
        datatable(i_chosenindex$mutate,
                  rownames = FALSE,
                  # Creates display options (i.e. show '10' rows or 'All' rows)
                  options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(-1, 10), c("All", "10")), 
                                 columnDefs = list(list(visible = FALSE, targets = c(3:5)))
                                  )
                  
        ) %>%
          # Formats table to display 2 digits rather than all
          formatRound(columns = c(2),
                      digits = 2
                      ) %>%
          formatRound(columns = c(3),
                      digits = 2
                      ) %>%
          
          # Aligns output values to provide readable formatting
          formatStyle(columns = c(2:3),
                      'text-align' = 'right'
                      ) %>%
          
          # Highlights row which is Base Period
          formatStyle(columns = "is_base",
                      target = 'row',
                      backgroundColor = styleEqual(c('1'),
                                                   c('lightBlue'))
                      ) %>%
          # Highlights rows which are forecasts
          formatStyle(columns = "is_forecast",
                      target = 'row',
                      backgroundColor = styleEqual(c('1'), 
                                                   c('lightYellow'))
                      ) %>%
          # Highlights row which is base and forecast (if applicable)
          formatStyle(columns = "is_forecast_base",
                      target = 'row',
                      backgroundColor = styleEqual(c('1'), 
                                                   c('lightGreen'))
                      )
      })
      
  }
  
})
  
# Dataframe for download of indices
i_download = reactiveValues()

# Timestamp creation
i_download$date <- paste("This spreadsheet was downloaded using the DASD Indexation Tool on",
                            Sys.Date(),
                            "at",
                            sub("(.*-.*-.*) ","", Sys.time()),
                            "using: ", 
                            updateweblink, 
                            sep = " "
                          )

# Dataframe for selected indices download
i_download_select = reactive({
    
  i_download$selectvector <- 0*i_chosenindex$mutate[,(ncol(i_chosenindex$mutate)-3)]
  
  i_download$selectcombine <- c(i_download$date,
                                sub(0, "", i_download$selectvector)
                                )
    
  i_download$selectindex <- i_chosenindex$mutate[,2:3]
    
  i_download_select = rbind(i_download$selectindex,
                            i_download$selectcombine
                            )
    
  colnames(i_download_select) <- c(paste(input$i_indices,"Index"),
                                   "YoY (%)"
                                   )
  
  rownames(i_download_select) <- c(i_chosenindex$rownames[which(complete.cases(i_chosenindex$index) == TRUE)],
                                   "Timestamp"
                                   )
    
  i_download_select
    
})

# Dataframe for raw indices download
i_download_raw = reactive({
    
  i_download$rawvector <- 0*index_obr_all[,ncol(index_obr_all)]
  
  i_download$rawcombine <- c(i_download$date, 
                             sub(0, "", i_download$rawvector)
                             )
  
  i_download$rawindex <- index_obr_raw
    
  i_download_raw = rbind(i_download$rawindex, i_download$rawcombine)
    
  i_download_raw
    
})
  
# Download selected data
output$i_download <- downloadHandler(
  filename = function() {
                        paste("DASD Indexation Tool - Indices - ", input$i_indices, "- ", Sys.Date(), '.csv', sep = '')
                        },
  content = function(con) {
                          write.csv(i_download_select(), con)
                          }
)
    
  # Download full raw data
output$i_downloadall <- downloadHandler(
  filename = function() {
                        paste("DASD Indexation Tool - Indices - ALL - ", Sys.Date(), '.csv', sep = '')
                        },
  content = function(con) {
                          write.csv(i_download_raw(), con, row.names = FALSE)
                          }
)
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE: MAIN | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE: USER INDEX | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Generates starting positions for each period
observeEvent({
            input$i_userperiod
            }, {
  
  output$i_userperiodstart <- renderUI({
    
    if ({
        input$i_userperiod == "Quarterly"
        }) 
        {
          textInput(inputId = "i_userperiodstart", label = "Start Period",
                    value = paste0(year_now - 1, "Q1")
                    )
        }
        else if ({
        input$i_userperiod == "Calendar Year"
        })
        {
          numericInput(inputId = "i_userperiodstart", label = "Start Period",
                       value = year_now,
                       step = 1
                      )
        }
        else if ({ 
        input$i_userperiod == "Financial Year"
        })
        {
          textInput(inputId = "i_userperiodstart", label = "Start Period",
                    value = paste0(year_now - 1, "/", str_sub(year_now, -2, -1))
                    )
        }
    
  })
  
  output$i_userperiodend <- renderUI({
    
    if ({
        input$i_userperiod == "Quarterly"
        }) 
        {
          textInput(inputId = "i_userperiodend", label = "End Period",
                    value = paste0(year_now + 3, "Q4")
                    )
        }
        else if ({
        input$i_userperiod == "Calendar Year"
        })
        {
          numericInput(inputId = "i_userperiodend", label = "End Period",
                       value = year_now + 5, 
                       step = 1
                      )
        }
        else if ({
        input$i_userperiod == "Financial Year"
        })
        {
          textInput(inputId = "i_userperiodend", label = "End Period",
                    value = paste0(year_now + 4, "/", str_sub(year_now + 5, -2, -1))
                    )
        }
    
  })
  
}, ignoreInit = FALSE)

# Generates new min/max positions for each period, if applicable.
observeEvent({
            input$i_userperiodstart
            input$i_userperiodend
            }, {
  
  if ({
      input$i_userperiod == "Calendar Year"
      })
      {
        updateNumericInput(session = session, inputId = "i_userperiodstart",
                           value = NULL,
                           max = input$i_userperiodend - 1, 
                           step = 1
                          )
    
        updateNumericInput(session = session, inputId = "i_userperiodend",
                           value = NULL,
                           min = input$i_userperiodstart + 1, 
                           step = 1
                          )
    }
  
}, ignoreInit = TRUE)

# Checks that inputs are valid, else disables table update
observeEvent({
  input$i_userperiod
  input$i_userperiodstart
  input$i_userperiodend
}, {
  
  if ({
      input$i_tabs == 'Add Index' &
      !is.null(input$i_userperiodstart) &
      !is.null(input$i_userperiodend) &
      !is.na(as.numeric(str_sub(input$i_userperiodstart, 1, 4))) &
      !is.na(as.numeric(str_sub(input$i_userperiodend, 1, 4))) &
      as.numeric(str_sub(input$i_userperiodend, 1, 4)) > as.numeric(str_sub(input$i_userperiodstart, 1, 4))
  }) {
    
    if ({
      input$i_userperiod == "Calendar Year"
    })
    {
      enable("i_userupdate")
    } 
    else if ({
      input$i_userperiod == "Quarterly" &
        grepl(pattern = 'Q', input$i_userperiodstart, ignore.case = TRUE) &
        grepl(pattern = 'Q', input$i_userperiodend, ignore.case = TRUE) &
        as.numeric(nchar(input$i_userperiodstart)) - as.numeric(str_locate(input$i_userperiodstart, pattern = 'Q'))[1] == 1 &
        as.numeric(nchar(input$i_userperiodend)) - as.numeric(str_locate(input$i_userperiodend, pattern = 'Q'))[1] == 1 &
        as.numeric(str_sub(input$i_userperiodstart, -1)) <= 4 &
        as.numeric(str_sub(input$i_userperiodend, -1)) <= 4
    }) 
    {
      enable("i_userupdate")
    } 
    else if ({
      input$i_userperiod == "Financial Year" &
        grepl(pattern = '/', input$i_userperiodstart) &
        grepl(pattern = '/', input$i_userperiodend) &
        as.numeric(nchar(input$i_userperiodstart)) - as.numeric(str_locate(input$i_userperiodstart, pattern = '/'))[1] == 2 &
        as.numeric(nchar(input$i_userperiodend)) - as.numeric(str_locate(input$i_userperiodend, pattern = '/'))[1] == 2 &
        as.numeric(str_sub(input$i_userperiodstart, -2, -1)) - as.numeric(str_sub(input$i_userperiodstart, 3, 4)) == 1 &
        as.numeric(str_sub(input$i_userperiodend, -2, -1)) - as.numeric(str_sub(input$i_userperiodend, 3, 4)) == 1 
    }) 
    {
      enable("i_userupdate")
    } 
    else {
      disable("i_userupdate")
    }
    
  }
  else {
    disable("i_userupdate")
  }
})

# Creates the required rows/rownames for dataframe
i_user = reactiveValues()

observeEvent({
            input$i_userperiod
            input$i_userupdate
            }, {
  
  i_user$rows <- list()         
              
  # Delay required to allow switching between periods without app crashing
  delay(250, {
    
    if ({
        input$i_userperiod == "Calendar Year"
        })
        {
          i_user$newrows <- paste0(
            rep(input$i_userperiodstart:input$i_userperiodend, each = 1)
            )
      
          i_user$rows <- i_user$newrows[(which(i_user$newrows == input$i_userperiodstart)):(which(i_user$newrows==input$i_userperiodend))]
          i_user$rowlength <- length(i_user$rows)
      
        } 
        else if ({
        input$i_userperiod == "Financial Year"
        })
        {
          i_user$newrows <- paste0(
            rep(as.numeric(str_sub(input$i_userperiodstart, 1, 4)):as.numeric(str_sub(input$i_userperiodend, 1, 4)), each = 1),
            "/",
            rep(as.numeric(str_sub(input$i_userperiodstart, -2, -1)):as.numeric(str_sub(input$i_userperiodend, -2, -1)), each = 1)
            )
          
          i_user$rows <- i_user$newrows[(which(i_user$newrows == input$i_userperiodstart)):(which(i_user$newrows==input$i_userperiodend))]
          i_user$rowlength <- length(i_user$rows)
      
        } 
        else if ({
        input$i_userperiod == "Quarterly"
        })
        {
          i_user$newrows <- paste0(
            rep(as.numeric(str_sub(input$i_userperiodstart, 1, 4)):as.numeric(str_sub(input$i_userperiodend, 1, 4)), each = 4),
            "Q",
            rep(1:4, length = 4)
            )
          
          i_user$rows <- i_user$newrows[(which(i_user$newrows == input$i_userperiodstart)):(which(i_user$newrows == input$i_userperiodend))]
          i_user$rowlength <- length(i_user$rows)
        }
    
  })
  
})

# Creates the basic input table...
i_values_userinput = reactiveValues()
i_userdata = reactiveValues()

observeEvent({
            input$i_userperiod
            input$i_userupdate
            }, {
  
  # Delay required to allow switching changing start/end periods without app crashing
  delay(500, {
    
    if ({
      is.null(i_values_userinput[["i_userdata$df_input"]])
    }) 
    {
      i_userdata$df_input = as.data.frame(matrix(0,
                                               nrow = i_user$rowlength,
                                               ncol = 2))
    }
    else {
      i_userdata$df_input = i_values_userinput[["i_userdata$df_input"]]
    }
    
    i_userdata$df_input
    
  })
  
})

# Generates table at start
observe({
  
  if ({
    input$i_tabs == 'Add Index'
  })
  {
    click("i_userupdate")
  }
  
})

# Produces input table
output$i_user <- renderRHandsontable({
  
  i_df_userinput = i_userdata$df_input
  
  if ({
    !is.null(i_df_userinput)
  })
  {
    rhandsontable(i_df_userinput, col_highlight = 2,
                  useTypes = TRUE, stretchH = "all",
                  rowHeaders = unlist(list(i_user$rows)), rowHeaderWidth = 75,
                  colHeaders = c("Index", "YoY (%)")) %>% 
                  hot_cols(renderer = 
                    "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
                    td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
                    }"
                  )
  }
  
})

# Disables option to generate new index if empty table
observe({
  
  req(input$i_user, {
  
  if ({
      all(as.data.frame(hot_to_r(input$i_user))[1] != 0) | all(as.data.frame(hot_to_r(input$i_user))[2] != 0)
      })
      {
        enable("i_useradd")
      }
      else {
        disable("i_useradd")
      } 
    
  })
  
})


# Generates completed pair of index and year-on-year inflation, if not provided
i_userbase <- reactiveValues()
i_df_useroutput <- reactiveValues()

i_addcount = reactiveValues(qtr = 0,
                            pa = 0,
                            fy = 0
                            )

observeEvent({
            input$i_useradd
            }, {
              
  if ({
      input$i_userperiod == "Quarterly"
      })  
      {
        i_addcount$qtr = i_addcount$qtr + 1
      }
      else if ({
      input$i_userperiod == "Calendar Year"  
      })        
      {
        i_addcount$pa = i_addcount$pa + 1
      }
      else if ({
      input$i_userperiod == "Financial Year"  
      })
      {
        i_addcount$fy = i_addcount$fy + 1
      }  
                          
  # Identifies starting position
  i_df_useroutput$all <- as.data.frame(hot_to_r(input$i_user))
  rownames(i_df_useroutput$all) <- i_user$rows
  colnames(i_df_useroutput$all) <- c(paste0("x_","User Index: ", input$i_userindex), 
                                     paste0("User Index: ", input$i_userindex)
                                     )
  if ({
      i_df_useroutput$all[1, 1] == 0
      })
      {
        i_userbase$index = 100
      }
      else {
        i_userbase$index = i_df_useroutput$all[1, 1]
      }
  
  if ({
      i_df_useroutput$all[1, 2] == 0
      })
      {
        i_userbase$yoy = 0
      }
      else {
        i_userbase$yoy = i_df_useroutput$all[1, 2]
      }
  
  # Create index & year-on-year columns
  if ({
      !is.null(input$i_user) &
      any(i_df_useroutput$all[1] != 0)
      })
      {
        i_df_useroutput$index <- i_df_useroutput$all[1]
      }
  
  if ({
      !is.null(input$i_user) &
      any(i_df_useroutput$all[2] != 0)
      })
      {
        i_df_useroutput$yoy <- i_df_useroutput$all[2]
  }
  
  # If missing, generate other column
  if ({
      is.null(i_df_useroutput$index) &
      !is.null(i_df_useroutput$yoy)
      })
      {
        i_df_useroutput$yoy <- as.numeric(unlist(list(i_df_useroutput$yoy)))
        
        i_df_useroutput$index <- list()
        i_df_useroutput$index[1] <- i_userbase$index
        
        for (i in 2:i_user$rowlength) {
          i_df_useroutput$index[i] <- as.numeric(i_df_useroutput$index[i-1])*(1+(i_df_useroutput$yoy[i]/100))
        }
      }
  
  if ({
      !is.null(i_df_useroutput$index) &
      is.null(i_df_useroutput$yoy)
      })
      {
        i_df_useroutput$index <- as.numeric(unlist(list(i_df_useroutput$index)))
        
        i_df_useroutput$yoy <- list()
        i_df_useroutput$yoy[1] <- i_userbase$yoy
    
        for (y in 2:i_user$rowlength) {
          i_df_useroutput$yoy[y] <- (((i_df_useroutput$index[y])/(i_df_useroutput$index[y-1]))-1)*100
        }
      }
  
  # Create new dataframe for use
  i_df_useroutput$index <- as.data.frame(unlist(i_df_useroutput$index), row.names = i_user$rows)
  i_df_useroutput$yoy <- as.data.frame(unlist(i_df_useroutput$yoy), row.names = i_user$rows)
  colnames(i_df_useroutput$index) <- colnames(i_df_useroutput$all)[1]
  colnames(i_df_useroutput$yoy) <- colnames(i_df_useroutput$all)[2]
  
  # Stores value so changing other variables does not reset added indices
  if ({
      input$i_userperiod == "Quarterly"
    })
    {
      index_qtr$index <- merge(index_qtr$index,
                                   i_df_useroutput$index,
                                   by = 0,
                                   all = TRUE
                                   )
      
      index_qtr$yoy <- merge(index_obr_qtr_yoy,
                                 i_df_useroutput$yoy,
                                 by = 0,
                                 all = TRUE
                                 )
      
      index_qtr$rownames <- sort.int(unique(c(index_qtr$rownames, i_user$rows)))
      index_qtr$indexnames <- colnames(index_qtr$yoy)[2:ncol(index_qtr$yoy)]
      index_qtr$index <- as.data.frame(index_qtr$index[,2:ncol(index_qtr$index)], row.names = index_qtr$rownames)
      index_qtr$yoy <- as.data.frame(index_qtr$yoy[,2:ncol(index_qtr$yoy)], row.names = index_qtr$rownames)
    }  
    else if ({
    input$i_userperiod == "Calendar Year"
    })
    {
      index_pa$index <- merge(index_pa$index,
                                   i_df_useroutput$index,
                                   by = 0,
                                   all = TRUE
                                    )
      
      index_pa$yoy <- merge(index_pa$yoy,
                                 i_df_useroutput$yoy,
                                 by = 0,
                                 all = TRUE
                                  )
      
      index_pa$rownames <- sort.int(unique(c(index_pa$rownames, i_user$rows)))
      index_pa$indexnames <- colnames(index_pa$yoy)[2:ncol(index_pa$yoy)]
      index_pa$index <- as.data.frame(index_pa$index[,2:ncol(index_pa$index)], row.names = index_pa$rownames)
      index_pa$yoy <- as.data.frame(index_pa$yoy[,2:ncol(index_pa$yoy)], row.names = index_pa$rownames)
      
    }    
    else if ({
    input$i_userperiod == "Financial Year"
    })
    {
      index_fy$index <- merge(index_fy$index,
                                  i_df_useroutput$index,
                                  by = 0,
                                  all = TRUE
                                  )
    
      index_fy$yoy <- merge(index_fy$yoy,
                                i_df_useroutput$yoy,
                                by = 0,
                                all = TRUE
                                )
      
      index_fy$rownames <- sort.int(unique(c(index_fy$rownames, i_user$rows)))
      index_fy$indexnames <- colnames(index_fy$yoy)[2:ncol(index_fy$yoy)]
      index_fy$index <- as.data.frame(index_fy$index[,2:ncol(index_fy$index)], row.names = index_fy$rownames)
      index_fy$yoy <- as.data.frame(index_fy$yoy[,2:ncol(index_fy$yoy)], row.names = index_fy$rownames)
      
    }
  
  showNotification("Index added.")
  
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDICES TABLE: USER INDEX | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'def_' to prevent duplication with other outputs
  
# Disable inputs if Guidance tab is open; !is.null required to disable uiOutput correctly  
observe({
  
  if({
    input$def_tabs == 'Guidance' &
    !is.null(input$def_periodstart) &
    !is.null(input$def_periodend)  
    })
    {
      disable("def_indices")
      disable("def_realnom")
      disable("def_fromto")
      disable("def_period")
      disable("def_periodstart")
      disable("def_periodend")
      disable("def_inputrows")
      disable("def_pchange")
      disable("def_download")
      disable("def_update")
    }
    else {
      enable("def_indices")
      enable("def_realnom")
      enable("def_fromto")
      enable("def_period")
      enable("def_periodstart")
      enable("def_periodend")
      enable("def_inputrows")
      enable("def_pchange")
      enable("def_download")
      enable("def_update")
    }
    
})
  
# Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
def_chosenindex = reactiveValues(index = index_obr_fy_index,
                                yoy = index_obr_fy_yoy,
                                rownames = rownames(index_obr_fy),
                                indexnames = colnames(index_obr_fy_yoy)
                                )
  
# Ensures correct dataframe is chosen for future use, based on user input
observe({                
  
  # Select the correct period
  if ({
    input$def_period == "Quarterly"
  })
  {
    def_chosenindex$index = index_qtr$index
    def_chosenindex$yoy = index_qtr$yoy
    def_chosenindex$rownames = rownames(index_qtr$index)
    def_chosenindex$indexnames = index_qtr$indexnames
    def_chosenindex$nrows = nrow(index_qtr$index)
  }
  else if ({
    input$def_period == "Calendar Year"  
  })
  {
    def_chosenindex$index = index_pa$index
    def_chosenindex$yoy = index_pa$yoy
    def_chosenindex$rownames = rownames(index_pa$index)
    def_chosenindex$indexnames = index_pa$indexnames
    def_chosenindex$nrows = nrow(index_pa$index)
  }
  else if ({
    input$def_period == "Financial Year"
  })
  {
    def_chosenindex$index = index_fy$index
    def_chosenindex$yoy = index_fy$yoy
    def_chosenindex$rownames = rownames(index_fy$index)
    def_chosenindex$indexnames = index_fy$indexnames
    def_chosenindex$nrows = nrow(index_fy$index)
  }
  
})


# Generates correct base period dropdown menu in indices user interface, based on user input
observe({
  
  updateSelectInput(session = session, 
                    inputId = "def_indices",
                    choices = def_chosenindex$indexnames
                    )
  
})

observe({
  
  if ({
    grepl("User Index:", input$def_indices)
  })
  {
    disable("def_period")
  }
  else {
    enable("def_period")
  }
  
})
  
# Checks that inputs are valid, else disables table update
observeEvent({
               input$def_period
               input$def_periodstart
               input$def_periodend
               }, {
                  
   if ({
       input$def_tabs != "Guidance" &
       !is.null(input$def_periodstart) &
       !is.null(input$def_periodend) &
       !is.na(as.numeric(str_sub(input$def_periodstart, 1, 4))) &
       !is.na(as.numeric(str_sub(input$def_periodend, 1, 4))) #&
       as.numeric(str_sub(input$def_periodstart, 1, 4)) >= as.numeric(str_sub(def_chosenindex$rownames[1], 1, 4)) &
       as.numeric(str_sub(input$def_periodend, 1, 4)) > as.numeric(str_sub(input$def_periodstart, 1, 4))
       }) {
     
       if ({
           input$def_period == "Calendar Year"
           })
           {
             enable("def_update")
           } 
           else if ({
           input$def_period == "Quarterly" &
           grepl(pattern = 'Q', input$def_periodstart, ignore.case = TRUE) &
           grepl(pattern = 'Q', input$def_periodend, ignore.case = TRUE) &
           as.numeric(nchar(input$def_periodstart)) - as.numeric(str_locate(input$def_periodstart, pattern = 'Q'))[1] == 1 &
           as.numeric(nchar(input$def_periodend)) - as.numeric(str_locate(input$def_periodend, pattern = 'Q'))[1] == 1 &
           as.numeric(str_sub(input$def_periodstart, -1)) <= 4 &
           as.numeric(str_sub(input$def_periodend, -1)) <= 4
           }) 
           {
             enable("def_update")
           } 
           else if ({
           input$def_period == "Financial Year" &
           grepl(pattern = '/', input$def_periodstart) &
           grepl(pattern = '/', input$def_periodend) &
           as.numeric(nchar(input$def_periodstart)) - as.numeric(str_locate(input$def_periodstart, pattern = '/'))[1] == 2 &
           as.numeric(nchar(input$def_periodend)) - as.numeric(str_locate(input$def_periodend, pattern = '/'))[1] == 2 &
           as.numeric(str_sub(input$def_periodstart, -2, -1)) - as.numeric(str_sub(input$def_periodstart, 3, 4)) == 1 &
           as.numeric(str_sub(input$def_periodend, -2, -1)) - as.numeric(str_sub(input$def_periodend, 3, 4)) == 1 
           }) 
           {
             enable("def_update")
           } 
           else {
             disable("def_update")
           }
     
           }
       else {
             disable("def_update")
             }
 })

# Generates starting positions for each period
observeEvent({
              input$def_period
              }, {
    
  output$def_periodstart <- renderUI({
      
    if ({
        input$def_period == "Quarterly"
        }) 
        {
          textInput(inputId = "def_periodstart", label = "Start Period",
                    value = paste0(year_now - 1, "Q1")
                    )
        }
        else if ({
        input$def_period == "Calendar Year"
        })
        {
          numericInput(inputId = "def_periodstart", label = "Start Period",
                       value = year_now
                       , step = 1
                       )
        }
        else if ({ 
        input$def_period == "Financial Year"
        })
        {
          textInput(inputId = "def_periodstart", label = "Start Period",
                    value = paste0(year_now - 1, "/", str_sub(year_now, -2, -1))
                    )
        }
      
  })
    
  output$def_periodend <- renderUI({
      
    if ({
        input$def_period == "Quarterly"
        }) 
        {
          textInput(inputId = "def_periodend", label = "End Period",
                     value = paste0(year_now + 3, "Q4")
                    )
        }
        else if
        ({
        input$def_period == "Calendar Year"
        })
        {
          numericInput(inputId = "def_periodend", label = "End Period",
                      value = year_now + 5, 
                      step = 1
                      )
        }
        else if ({
        input$def_period == "Financial Year"
        })
        {
          textInput(inputId = "def_periodend", label = "End Period",
                    value = paste0(year_now + 4, "/", str_sub(year_now + 5, -2, -1))
                    )
        }
      
    })
    
}, ignoreInit = FALSE)
  
# Generates new min/max positions for each period, if applicable.
observeEvent({
              input$def_periodstart
              input$def_periodend
              }, {
    
  if ({
      input$def_period == "Calendar Year"
      })
      {
        updateNumericInput(session = session, inputId = "def_periodstart",
                           value = NULL,
                           max = input$def_periodend - 1, 
                           step = 1
                           )
      
        updateNumericInput(session = session, inputId = "def_periodend",
                           value = NULL,
                           min = input$def_periodstart + 1, 
                           step = 1
                           )
  }
                
}, ignoreInit = TRUE)
  
# Creates correct options for use in real/nominal options, and column headers
def_chosen = reactiveValues()
  
observeEvent({
              input$def_period
              input$def_update
              }, {
    
  # Delay required to allow switching between periods without app crashing
  delay(250, {
      
    if ({
        input$def_period == "Calendar Year"
        })
        {
          def_chosen$columns <- paste0(
                                      rep(input$def_periodstart:input$def_periodend, each = 1)
                                      )
          
          def_chosen$columns <- def_chosen$columns[(which(def_chosen$columns == input$def_periodstart)):(which(def_chosen$columns == input$def_periodend))]
          def_chosen$collength <- length(def_chosen$columns)

        } 
        else if ({
        input$def_period == "Financial Year"
        })
        {
          def_chosen$columns <- paste0(
                                      rep(as.numeric(str_sub(input$def_periodstart, 1, 4)):as.numeric(str_sub(input$def_periodend, 1, 4)), each = 1),
                                      "/",
                                      rep(as.numeric(str_sub(input$def_periodstart, -2, -1)):as.numeric(str_sub(input$def_periodend, -2, -1)), each = 1)
                                      )
          def_chosen$columns <- def_chosen$columns[(which(def_chosen$columns == input$def_periodstart)):(which(def_chosen$columns==input$def_periodend))]
          def_chosen$collength <- length(def_chosen$columns)

        } 
        else if ({
        input$def_period == "Quarterly"
        })
        {
          def_chosen$columns <- paste0(
                                      rep(as.numeric(str_sub(input$def_periodstart, 1, 4)):as.numeric(str_sub(input$def_periodend, 1, 4)), each = 4),
                                      "Q",
                                      rep(1:4, length = 4)
                                      )
        def_chosen$columns <- def_chosen$columns[(which(def_chosen$columns == input$def_periodstart)):(which(def_chosen$columns == input$def_periodend))]
        def_chosen$collength <- length(def_chosen$columns)
        }
    
  })
                
})

# Generates correct Real/Nominal option for input              
output$def_fromto <- renderUI({

  if ({
      input$def_realnom == "Real to Nominal"
      }) 
      {
        selectInput(inputId = "def_fromto",
                    label = "Convert From (Base-year):",
                    choices = unique(c(
                                      def_chosenindex$rownames[which(!is.na(def_chosenindex$index[, which(input$def_indices == def_chosenindex$indexnames)]))],
                                      def_chosen$columns)),
                    selected = if ({
                                  input$def_period == "Calendar"
                                  })
                                  {
                                    year_known[nchar(year_known) == 4]
                                  }
                                  else if ({
                                  input$def_period == "Financial Year"
                                  })
                                  {
                                    year_known[grepl(pattern = '/', year_known)]
                                  }
                                  else {
                                    year_known[grepl(pattern = 'Q', year_known)]
                                  }
                    )
      }
      else {
        selectInput(inputId = "def_fromto",
                    label = "Convert To (Base-year):",
                    choices = unique(c(
                                      def_chosenindex$rownames[which(!is.na(def_chosenindex$index[, which(input$def_indices == def_chosenindex$indexnames)]))],
                                      def_chosen$columns)),
                    selected = if ({
                                  input$def_period == "Calendar"
                                  })
                                  {
                                    year_known[nchar(year_known) == 4]
                                  }
                                  else if ({
                                  input$def_period == "Financial Year"
                                  })
                                  {
                                    year_known[grepl(pattern = '/', year_known)]
                                  }
                                  else {
                                    year_known[grepl(pattern = 'Q', year_known)]
                                  }
                    )
      }
  
})

# Generates the basic input table...
def_values_input = reactiveValues()
def_data = reactiveValues()

observeEvent({
              input$def_inputrows
              input$def_period
              input$def_update
              }, {
  
  # Delay required to allow switching changing start/end periods without app crashing
  delay(500, {
    
    if ({
        is.null(def_values_input[["def_data$df_input"]])
        }) 
        {
          def_data$df_input = as.data.frame(matrix(0,
                                                   nrow = input$def_inputrows,
                                                   ncol = def_chosen$collength))
        }
        else {
          def_data$df_input = def_values_input[["def_data$df_input"]]
        }
    
    def_data$df_input
    
  })
                
})

# Generates table at start
observe({
  
  if ({
      input$def_tabs == 'Input'
      })
      {
        click("def_update")
      }
  
})
  
# Produces input table
output$def_hot <- renderRHandsontable({
  
  def_df_input = def_data$df_input
  
  if ({
      !is.null(def_df_input)
      })
      {
        rhandsontable(def_df_input, col_highlight = 2,
                      useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(def_chosen$columns))) %>% 
                      hot_cols(renderer = 
                        "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
                        td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
                        }"
                      )
      }
})
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: INPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Generates necessary variables
def_chosenindex$final = reactiveValues()
def_chosenindex$mutate = reactiveValues()

def_base_value = reactiveValues()
def_shift = reactiveValues()

observeEvent({
              input$def_fromto
              input$def_indices
              input$def_realnom
              input$def_period
              input$def_update
              }, {
                    
  delay(500, {              
                    
    if ({
        !is.null(def_chosenindex$rownames)
        })
        {
          def_shift = which(colnames(def_chosenindex$yoy) == input$def_indices)
        
          # Finding the relevant index:
          def_chosenindex$mutateindex <- def_chosenindex$index[, def_shift]
          def_chosenindex$mutateyoy <- def_chosenindex$yoy[, def_shift]
          
          # Removing the non-NA rownames/nrows:
          def_chosenindex$mutateindex <- def_chosenindex$mutateindex[!is.na(def_chosenindex$mutateindex)]
          def_chosenindex$mutateyoy <- def_chosenindex$mutateyoy[!is.na(def_chosenindex$mutateyoy)]
          def_chosenindex$rownames <- def_chosenindex$rownames[!is.na(def_chosenindex$mutateindex)]
          def_chosenindex$nrows <- length(def_chosenindex$mutateindex[!is.na(def_chosenindex$mutateindex)])
          def_chosenindex$lastrow <- max(which(!is.na(def_chosenindex$mutateindex)))
      
          # Adding additional rows to mutated index if input period extends beyond selection
          if ({
              (input$def_period == "Calendar Year") | (input$def_period == "Financial Year")
              }) 
              {
                def_chosenindex$newrows <- as.numeric(str_sub(input$def_periodend, 1, 4)) - as.numeric(str_sub(def_chosenindex$rownames[def_chosenindex$lastrow], 1, 4))
              }
              else if ({
              input$def_period == "Quarterly"
              })
              {
                def_chosenindex$newyears <- 4*(as.numeric(str_sub(input$def_periodend, 1, 4)) - as.numeric(str_sub(def_chosenindex$rownames[def_chosenindex$lastrow], 1, 4)))
                def_chosenindex$newquarters <- as.numeric(str_sub(input$def_periodend, -1)) - as.numeric(str_sub(def_chosenindex$rownames[def_chosenindex$lastrow], -1))
                def_chosenindex$newrows <- def_chosenindex$newyears + def_chosenindex$newquarters
              }
      
          if ({
              def_chosenindex$newrows > 0
              })
              {
                def_chosenindex$addrows <- list()
                def_chosenindex$addrownames <- list()
      
                for (d in 1:def_chosenindex$newrows) {
                  if ({
                      (input$def_period == "Calendar Year") | (input$def_period == "Financial Year")
                      }) 
                      {
                        def_chosenindex$addrows[d] = def_chosenindex$mutateindex[length(def_chosenindex$mutateindex)]*{
                                                        (1+(def_chosenindex$mutateyoy[length(def_chosenindex$mutateindex)]/100))^d}
                      }
                }
                
                for (q in 1:(4*def_chosenindex$newrows)) {
                  if ({
                      input$def_period == "Quarterly"
                      })
                      {
                        def_chosenindex$addrows[q] <- def_chosenindex$mutateindex[(length(def_chosenindex$mutateindex))]*{
                                                         (1+((def_chosenindex$mutateyoy[length(def_chosenindex$mutateindex)]/100)/4))^q}
                      }
                }
      
                def_chosenindex$addrownames <- def_chosen$columns %>% subset(
                                                                            (def_chosen$columns %in% def_chosenindex$rownames)
                                                                            == FALSE
                                                                            )
      
                def_chosenindex$mutateindex = c(def_chosenindex$mutateindex,
                                           unlist(def_chosenindex$addrows)
                                            )
    
                def_chosenindex$rownames = unique(
                                                  c(def_chosenindex$rownames, unlist(def_chosenindex$addrownames))
                                                  )
      
          }
          
          
      
      # Finding the correct base year in the index
      def_base_value <- def_chosenindex$mutateindex[which(def_chosenindex$rownames == input$def_fromto)]
      
      # Converting input values to output values 
      if ({
          input$def_realnom == "Real to Nominal"
          })
          {
            def_chosenindex$final <- def_chosenindex$mutateindex / def_base_value
          } 
          else {
            def_chosenindex$final <- def_base_value / def_chosenindex$mutateindex
          }
      
      # Transmuting the dataframe
      def_chosenindex$final = t(def_chosenindex$final)
      def_chosenindex$colnumber <- which(def_chosenindex$inputperiods == input$def_fromto)
      
      }
  })
})
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: DEFLATOR SELECT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFLATOR CALCULATOR: OUTPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# All variables pre-fixed with 'def_' to prevent duplication with other outputs
    
# Generates user output table layout
def_data_output = reactive({
      
  def_df_out = hot_to_r(input$def_hot)
      
  def_df_output = as.data.frame(mapply('*', def_df_out, def_chosenindex$final[,
                                                                              which(def_chosenindex$rownames == input$def_periodstart):
                                                                              which(def_chosenindex$rownames == input$def_periodend)]))
      
})
  
# Produces output table
output$def_cold <- renderRHandsontable({
  
  def_df_output = def_data_output()
  
  if ({
      !is.null(def_df_output)
      })
      {
        rhandsontable(def_df_output, 
                      useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(def_chosen$columns)), readOnly = TRUE) %>% 
                      hot_cols(renderer = 
                        "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0.00){
                        td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'; } else if (value != 0) {td.style.color = 'black';}
                        }"
                      )
      }
})
  
# Produce dataframe that combines input, output and deflator index
def_download = reactiveValues()

def_download_df = reactive({
  
  # Creates clear rownames for output download option
  def_download$inputrows <- 1:input$def_inputrows
  def_download$inputrows <- paste('Input', def_download$inputrows, sep = ' ')
  def_download$outputrows <- 1:input$def_inputrows
  def_download$outputrows <- paste('Output', def_download$outputrows, sep = ' ')
  def_download$indexname <- input$def_indices

  # Length of def_download$rownames must equal length of def_download_df (see below) else download error
  def_download$rownames <- unlist(
                                  rbind(list(def_download$inputrows),
                                        list(def_download$outputrows),
                                        def_download$indexname,
                                        "Timestamp"
                                        )
                                  )
  # Generates timestamp for download options  
  def_download$date <- paste("This spreadsheet was downloaded using the DASD Indexation Tool on",
                              Sys.Date(), 
                              "at", 
                              sub("(.*-.*-.*) ","", Sys.time()),
                              "using: ",
                              updateweblink,
                              sep = " "
                             )

  # Generates the download table  
  def_download$vector <- 0*def_chosenindex$final[,
                                                  which(def_chosenindex$rownames == input$def_periodstart):
                                                  (which(def_chosenindex$rownames == input$def_periodend)-1)]
  
  def_download$combine <- t(matrix(c(def_download$date,
                                     sub(0, "", def_download$vector)
                                     )))
    
  def_download$input = hot_to_r(input$def_hot)
  def_download$output = hot_to_r(input$def_cold)
    
  def_download$index = def_chosenindex$final[, 
                                              which(def_chosenindex$rownames == input$def_periodstart):
                                              which(def_chosenindex$rownames == input$def_periodend)]
  
  def_download_df = rbind(def_download$input,
                          def_download$output, 
                          def_download$index, 
                          def_download$combine
                          )
    
  colnames(def_download_df) <- def_chosen$columns
    
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
  
  if ({
      input$disc_tabs == 'Guidance' & 
      !is.null(input$disc_periodstart) & 
      !is.null(input$disc_periodend)
      })
      {
        disable("disc_rate")
        disable("disc_period")
        disable("disc_periodstart")
        disable("disc_periodend")
        disable("disc_inputrows")
        disable("disc_download")
        disable("disc_update")
      }
      else {
        enable("disc_rate")
        enable("disc_period")
        enable("disc_periodstart")
        enable("disc_periodend")
        enable("disc_inputrows")
        enable("disc_download")
        enable("disc_update")
      }
})
  
# Creates variable to that aligns with the app's default settings (i.e. prevents loading errors)
disc_chosen = reactiveValues()

# Chooses which discount rate to use (Standard, or Health)
observeEvent({
              input$disc_rate
              }, {
                
  if ({
      input$disc_rate == "Standard"
      })
      {
        disc_chosen$year = as.data.frame(t(disc_standard0$Year))
        disc_chosen$rate = as.data.frame(t(disc_standard0$SDR))
        disc_chosen$factor = as.data.frame(t(disc_standard0$SDF))
      } 
      else {
        disc_chosen$year = as.data.frame(t(disc_health0$Year))
        disc_chosen$rate = as.data.frame(t(disc_health0$HDR))
        disc_chosen$factor = as.data.frame(t(disc_health0$HDF))
      }
})

# Generates correct slider options in defcalc user interface, based on user input
# Read carefully before making changes. Splitting prevents re-rendering errors/resets.

# Generates starting positions for each period
observeEvent({
              input$disc_period
              }, {

  output$disc_periodstart <- renderUI({
    
    if ({
        input$disc_period == "Basic"
        })
        {
          numericInput(inputId = "disc_periodstart", label = "Start Period",
                        value = 0, min = 0, max = 0
                       )
        } 
        else if ({
        input$disc_period == "Calendar Year"
        })
        {
         numericInput(inputId = "disc_periodstart", label = "Start Period",
                       value = year_now
                       , step = 1
                      )
        } 
        else if ({
        input$disc_period == "Financial Year" 
        })
        {
          textInput(inputId = "disc_periodstart", label = "Start Period",
                     value = paste0(year_now - 1, "/", str_sub(year_now, -2, -1))
                   )
        }
  })
  
  output$disc_periodend <- renderUI({
    
    if ({
        input$disc_period == "Basic"
        })
        {
          numericInput(inputId = "disc_periodend", label = "End Period",
                        value = 10, min = 1, max = 125
                       )
        }
        else if ({
        input$disc_period == "Calendar Year"
        })
        {
          numericInput(inputId = "disc_periodend", label = "End Period",
                        value = year_now + 10, 
                        step = 1
                       )
        } 
        else if ({
        input$disc_period == "Financial Year"
        }) 
        {
          textInput(inputId = "disc_periodend", label = "End Period",
                    value = paste0(year_now + 9, "/", str_sub(year_now + 10, -2, -1))
                    )
        }
  })

}, ignoreInit = FALSE)

# Generates new min/max positions for each period, if applicable.
observeEvent({
              input$disc_periodstart
              input$disc_periodend
              }, {
  
  if ({
      input$disc_period == "Calendar Year"
      })
    {
      updateNumericInput(session = session, inputId = "disc_periodstart",
                          value = NULL,
                          max = input$disc_periodend - 1, 
                          step = 1
                         )
    
      updateNumericInput(session = session, inputId = "disc_periodend",
                          value = NULL,
                          min = input$disc_periodstart + 1, max = input$disc_periodstart + 125, 
                          step = 1
                         )
  }
}, ignoreInit = TRUE)

# Disable table generation if invalid inputs
observeEvent({
              input$disc_period
              input$disc_periodstart
              input$disc_periodend
              }, {
  
  if ({
      input$disc_tabs != "Guidance" &
      !is.null(input$disc_periodstart) &
      !is.null(input$disc_periodend) &
      is.numeric(str_sub(input$disc_periodstart, 1, 4)) &
      is.numeric(str_sub(input$disc_periodend, 1, 4))
      })
      {    
    
        if ({
            input$disc_period == "Financial Year" &
            grepl(pattern = '/', input$disc_periodstart) &
            grepl(pattern = '/', input$disc_periodend) &
            as.numeric(nchar(input$disc_periodstart)) - as.numeric(str_locate(input$disc_periodstart, pattern = '/'))[1] == 2 &
            as.numeric(nchar(input$disc_periodend)) - as.numeric(str_locate(input$disc_periodend, pattern = '/'))[1] == 2 &
            as.numeric(str_sub(input$disc_periodstart, -2, -1)) - as.numeric(str_sub(input$disc_periodstart, 3, 4)) == 1 &
            as.numeric(str_sub(input$disc_periodend, -2, -1)) - as.numeric(str_sub(input$disc_periodend, 3, 4)) == 1 
            }) 
            {
              enable("disc_update")
            } 
            else if ({
            input$disc_periodend > input$disc_periodstart &
            input$disc_period == "Basic" | input$disc_period == "Calendar Year"
            })
            {
              enable("disc_update")
            } 
            else {
              disable("disc_update")
            }
    
    } else {
        disable("disc_update")
    }
                
})

# Generates column headers for table
disc_chosen$columns = list() 

observeEvent({
              input$disc_period
              input$disc_update
              }, {
                
  # Delay required to allow switching between periods without app crashing
  delay(250, {
                
    if ({
        (input$disc_period == "Basic") | (input$disc_period == "Calendar Year")
        })
        {
          disc_chosen$collength <- (as.numeric(input$disc_periodend) - as.numeric(input$disc_periodstart)) + 1
          for (p in 1:disc_chosen$collength) {
            disc_chosen$columns[[p]] <- as.numeric(input$disc_periodstart) - 1 + p
          }
        } 
        else if ({
        input$disc_period == "Financial Year"
        })
        {
          disc_chosen$collength <- (as.numeric(str_sub(input$disc_periodend, 1, 4)) - as.numeric(str_sub(input$disc_periodstart, 1, 4))) + 1
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

observeEvent({
              input$disc_inputrows
              input$disc_period
              input$disc_update
              }, {
                
  # Delay required to allow switching changing start/end periods without app crashing
  delay(500, {
              
    if ({
        is.null(disc_values_input[["disc_data$df_input"]])
        })
        {
          disc_data$df_input = as.data.frame(matrix(0, nrow = input$disc_inputrows, ncol = disc_chosen$collength))
        } 
        else { 
          disc_data$df_input = disc_values_input[["disc_data$df_input"]]
        }
                  
  })
                        
})

# Generates table at start
observe({
  
  if ({
        input$disc_tabs == 'Input'
      })
      {
        click("disc_update")
      }
})

# Produces input table
output$disc_hot <- renderRHandsontable({
  
  disc_df_input = disc_data$df_input
  
  if ({
      !is.null(disc_df_input)
      })
      {
        rhandsontable(disc_df_input, col_highlight = 2,
                      useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(disc_chosen$columns))) %>% 
                      hot_cols(renderer = 
                        "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
                        td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
                        }"
                      )
  }
})
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: INPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: OUTPUT | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    

# All variables pre-fixed with 'disc_' to prevent duplication with other outputs

# Generates user output table layout
disc_data_output = reactive({
  
  if ({
      !is.null(input$disc_hot) & 
      !is.null(disc_chosen$collength)
      }) 
      {
        disc_df_out = hot_to_r(input$disc_hot)
        disc_data_output = as.data.frame(mapply('*', disc_df_out, disc_chosen$factor[, 1:disc_chosen$collength]))
      }
})

# Produces output table
output$disc_cold <- renderRHandsontable({
  
  disc_df_output = disc_data_output()
  
  if ({
      !is.null(disc_df_output)
      }) 
      {
        rhandsontable(disc_df_output, 
                      useTypes = TRUE, stretchH = "all", colHeaders = unlist(list(disc_chosen$columns))) %>% 
                      hot_cols(renderer = 
                        "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.NumericRenderer.apply(this, arguments); if(value == 0 ){
                        td.style.color = 'rgb(235, 235, 235)'; td.style.background = 'white'} else if (value != 0) {td.style.color = 'black';} 
                        }"
                      )
      }
})

# Produce dataframe that combines input, output, and discount rate
disc_download = reactiveValues()

# Download info:
disc_download$date <- paste("This spreadsheet was downloaded using the DASD Indexation Tool on",
                            Sys.Date(),
                            "at", 
                            sub("(.*-.*-.*) ","", Sys.time()),
                            "using: ", 
                            updateweblink, 
                            sep = " "
                            )

observeEvent({
              input$disc_inputrows
              input$disc_period
              input$disc_periodend
              input$disc_periodstart
              input$disc_rate
              input$disc_update
              }, {
                
  # Generate rownames              
  disc_download$inputrows <- 1:input$disc_inputrows
  disc_download$inputrows <- paste('Input', disc_download$inputrows, sep = ' ')
  disc_download$outputrows <- 1:input$disc_inputrows
  disc_download$outputrows <- paste('Output', disc_download$outputrows, sep = ' ')
  disc_download$factorname <- input$disc_rate
  
  # Length of disc_download$rownames must equal length of disc_download_df (see below) else download error
  disc_download$rownames <- unlist(
                                  rbind(list(disc_download$inputrows),
                                        list(disc_download$outputrows),
                                        disc_download$factorname, 
                                        "Timestamp"
                                        )
                                  )
})  

# Combine inputs, outputs and discount rate into one dataframe
disc_download_select = reactive({
  
  # Actual data frames
  disc_download$input = hot_to_r(input$disc_hot)
  disc_download$output = hot_to_r(input$disc_cold)
  disc_download$factor = disc_chosen$factor[, 1:ncol(disc_download$input)]
  
  # Generate one-dimensional data.frame for date to be same length
  disc_download$oned <- 0*disc_download$factor[, 2:length(disc_download$factor)]
  disc_download$datematrix <- t(matrix(c(disc_download$date,
                                         sub(0, "", disc_download$oned)
                                         )
                                ))
  
  # Data frames combined, and named
  disc_download_select = rbind(disc_download$input,
                               disc_download$output, 
                               disc_download$factor, 
                               disc_download$datematrix
                               )
  
  colnames(disc_download_select) <- disc_chosen$columns
  rownames(disc_download_select) <- disc_download$rownames
  
  disc_download_select
  
})

# Combine base standard/health discount rates into one file
disc_download_raw = reactive({

  disc_download$rawindex <- cbind(disc_standard0, disc_health0[2:3])
  disc_download$rawindex <- disc_download$rawindex %>% rename("Standard Discount Rate" = SDR,
                                                              "Standard Discount Factor" = SDF,
                                                              "Health Discount Rate" = HDR, 
                                                              "Health Discount Factor" = HDF
                                                              )

  disc_download$rawvector <- 0*disc_download$rawindex[,ncol(disc_download$rawindex)]
  
  disc_download$rawcombine <- c(disc_download$date,
                                sub(0, "", disc_download$rawvector)
                                )
  
  disc_download_raw = rbind(disc_download$rawindex,
                            disc_download$rawcombine
                            )

  rownames(disc_download_raw) <- c(disc_standard0$Year,
                                   "Timestamp"
                                   )

  disc_download_raw

})

# Download full data (inputs, outputs, and original discount rate index)
output$disc_download <- downloadHandler(
  
  filename = function() {
                          paste("DASD Indexation Tool - Discount Rate - USER - ",
                                Sys.Date(),
                                '.csv',
                                sep = ''
                                )
                        },
  
  content = function(con) {
                            write.csv(disc_download_select(),
                                      con
                                      )
                          }
)

# Download raw data (original discount rate indices)
output$disc_downloadraw <- downloadHandler(
  filename = function() {
                          paste("DASD Indexation Tool - Discount Rate - RAW - ", 
                                Sys.Date(), 
                                '.csv', 
                                sep = ''
                                )
                        },
  
  content = function(con) {
                            write.csv(disc_download_raw(),
                                      con,
                                      row.names = FALSE
                                      )
                          }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DISCOUNTING CALCULATOR: OUTPUT | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

# Allows app to reconnect after disconnecting due to idle
session$allowReconnect(TRUE)
    
})

