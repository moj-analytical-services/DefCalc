
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(botor)
library(openxlsx)
library(DT)
library(readxl)
library(dplyr)
library(officer)


###### DEVELOPER NOTES #####
# UPDATE OBR DATA SETS WITH EACH FISCAL EVENT - LONG-TERM ECONOMIC OUTLOOK, HISTORICAL OFFICIAL FORECASTS, ECONOMIC AND FISCAL OUTLOOK
# UPDATE HISTOIRC INDEX DATA TABLE IN S3 BUCKET - DA_INFLATION_INDEXATION_TABLE_MONTH_YEAR

shinyServer(function(input, output, session) {
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HOMEPAGE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

#~~~~~~~~~~~~~~~~~~~~~~~~~ NAVIGATION BUTTONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 
  #SWITCH TO GUIDANCE PAGE 
    observeEvent(input$guidance_switch, {
    guidancesubtab <- switch(input$sbMenu,
                      "overview_tab" = "guidance_subtab",
                      "guidance_subtab" = "overview_tab"
                      )
    updateTabItems(session, "sbMenu", guidancesubtab)
  })
  
  #SWITCH TO FAQ PAGE 
  observeEvent(input$faq_switch, {
    faqsubtab <- switch(input$sbMenu,
                      "overview_tab" = "faq_subtab",
                      "faq_subtab" = "overview_tab"
    )
    updateTabItems(session, "sbMenu", faqsubtab)
  })
  
  #SWITCH TO TROUBLESHOOT PAGE 
#  observeEvent(input$trblsht_switch, {
 #   trblshtsubtab <- switch(input$sbMenu,
  #                      "overview_tab" = "trblsht_subtab",
   #                     "trblsht_subtab" = "overview_tab"
    #)
#    updateTabItems(session, "sbMenu", trblshtsubtab)
 # })
  
  #SWITCH TO OBR DATA PAGE
  observeEvent(input$obrdata_switch, {
    obrsubtab <- switch(input$sbMenu,
                      "overview_tab" = "obrdata_subtab",
                      "obrdata_subtab" = "overview_tab"
    )
    updateTabItems(session, "sbMenu", obrsubtab)
  })
  
  #SWITCH TO HISTORIC INDEX PAGE
  observeEvent(input$historic_switch, {
    historicsubtab <- switch(input$sbMenu,
                      "overview_tab" = "historic_subtab",
                      "historic_subtab" = "overview_tab"
                      )
    updateTabItems(session, "sbMenu", historicsubtab)
  })
  
  #SWITCH TO INFLATION CALCULATOR PAGE
  observeEvent(input$calculator_switch, {
    calculatortab <- switch(input$sbMenu, 
                      "overview_tab" = "calculator_tab",
                      "calculator_tab" = "overview_tab"
                      )
    updateTabItems(session, "sbMenu", calculatortab)
  })
  
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HOMEPAGE LINKS ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #TOOL GUIDANCE 
  
  #INFLATION GUIDANCE
  output$downloadinflationguidance <- downloadHandler(
    filename = function() {
      paste("inflation-guidance - ", Sys.Date(), '.pptx', sep='')
    },
    content = function(con) {
      pptx <- read_pptx("www/inflation-guidance.pptx")
      print(pptx, target = con)
    }
  )
  

  #WELCOME MESSAGE OBR LINK
  obr1 <- a("Economic and Fiscal Outlook March 2023",
           href="https://obr.uk/efo/economic-and-fiscal-outlook-march-2023/")
  output$obr_site1 <- renderUI({
    tagList("", obr1)
  })
  
  #OBR
  obr <- a("OBR Data Website",
           href="https://obr.uk/data/")
  output$obr_site <- renderUI({
    tagList("", obr)
  })
  
  #CONTACT EMAIL
  email <- a("Economics Hub",
             href="mailto:EconomicsHub@justice.gov.uk")
  output$contact_email_home <- renderUI({
    tagList("", email)
  })
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HOMEPAGE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GUIDANCE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #RETURN TO HOMEPAGE BUTTON
  observeEvent(input$guidance_rtn_home, {
    guidance_home <- switch(input$sbMenu, 
                               "guidance_subtab" = "overview_tab",
                               "overview_tab" = "guidance_subtab"
    )
    updateTabItems(session, "sbMenu", guidance_home)
  }) 
  
  
  #APP GUIDANCE PPT
  output$appguidanceppt <- downloadHandler(
    filename = function() {
      paste("indexation-app-guidance - ", Sys.Date(), '.pptx', sep='')
    },
    content = function(con) {
      pptx <- read_pptx("www/indexation-app-guidance.pptx")
      print(pptx, target = con)
    }
  )
  
  
  #INFLATION GUIDANCE
  output$inflationguidanceppt <- downloadHandler(
    filename = function() {
      paste("inflation-guidance - ", Sys.Date(), '.pptx', sep='')
    },
    content = function(con) {
      pptx <- read_pptx("www/inflation-guidance.pptx")
      print(pptx, target = con)
    }
  )
  
  
  
  
  
  #HOMEPAGE TO FAQ BUTTON
  observeEvent(input$guidance_to_faq, {
    guidance_faq <- switch(input$sbMenu, 
                            "guidance_subtab" = "faq_subtab",
                            "faq_subtab" = "guidance_subtab"
    )
    updateTabItems(session, "sbMenu", guidance_faq)
  })
  
  
  
  #NAVIGATE TO OBR DATA PAGE
  observeEvent(input$guidance_obr, {
    guidance_obr <- switch(input$sbMenu, 
                            "guidance_subtab" = "obrdata_subtab",
                            "obrdata_subtab" = "guidance_subtab"
    )
    updateTabItems(session, "sbMenu", guidance_obr)
  }) 
  
  
  #NAVIGATE TO FORECASTS PAGE
  observeEvent(input$guidance_historic, {
    guidance_historic <- switch(input$sbMenu, 
                            "guidance_subtab" = "historic_subtab",
                            "historic_subtab" = "guidance_subtab"
    )
    updateTabItems(session, "sbMenu", guidance_historic)
  }) 
  
  
  #NAVIGATE TO CALCULATOR PAGE
  observeEvent(input$guidance_calculator, {
    guidance_calculator <- switch(input$sbMenu, 
                            "guidance_subtab" = "calculator_tab",
                            "calculator_tab" = "guidance_subtab"
    )
    updateTabItems(session, "sbMenu", guidance_calculator)
  }) 
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GUIDANCE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FAQ | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #RETURN TO HOMEPAGE BUTTON
  observeEvent(input$faq_rtn_home, {
    faq_home <- switch(input$sbMenu, 
                        "faq_subtab" = "overview_tab",
                        "overview_tab" = "faq_subtab"
    )
    updateTabItems(session, "sbMenu", faq_home)
  })
  
  #RETURN TO GUIDANCE BUTTON
  observeEvent(input$faq_to_guidance, {
    faq_guidance <- switch(input$sbMenu, 
                           "faq_subtab" = "guidance_subtab",
                           "guidance_subtab" = "faq_subtab"
    )
    updateTabItems(session, "sbMenu", faq_guidance)
  })
  
  
  #DOWNLOAD INFLATION GUIDANCE
  output$FAQinflationguidance <- downloadHandler(
    filename = function() {
      paste("inflation-guidance - ", Sys.Date(), '.pptx', sep='')
    },
    content = function(con) {
      pptx <- read_pptx("www/inflation-guidance.pptx")
      print(pptx, target = con)
    }
  )
  
  
  #LINK TO BCIS WEBSITE
  BCIS <- a("Further details can be found here.",
           href="https://bcis.co.uk/product/bcis-online/")
  output$BCIS_site <- renderUI({
    tagList("", BCIS)
  })
  
  #LINK TO ONS WEBSITE
  ONS <- a("Further details can be found here.",
            href="https://www.ons.gov.uk/news/statementsandletters/ukstatisticsauthoritystatementonthefutureoftherpi")
  output$ONS_site <- renderUI({
    tagList("", ONS)
  })
  
  
  #LINK TO GOV WEBSITE (GDP DEFLATOR)
  gdp <- a("Further details can be found here.",
           href="https://www.gov.uk/government/publications/gross-domestic-product-gdp-deflators-user-guide/gdp-deflators-user-guide#overview-of-gdp-deflator-series")
  output$gdp_site <- renderUI({
    tagList("", gdp)
  })
  
  
  
  #NAVIGATE TO CALCULATOR PAGE
  observeEvent(input$faq_calculator, {
    faq_calculator <- switch(input$sbMenu, 
                                  "faq_subtab" = "calculator_tab",
                                  "calculator_tab" = "faq_subtab"
    )
    updateTabItems(session, "sbMenu", faq_calculator)
  })
  
  
  #CONTACT EMAIL
  #"email" is brought forward from #CONTACT EMAIL in HOMEPAGE section
  output$contact_email_FAQ <- renderUI({
    tagList("", email)
  })
  

  #FAQ TO TROUBLESHOOT BUTTON
#  observeEvent(input$faq_to_trblsht, {
 #   faq_trblsht <- switch(input$sbMenu, 
  #                         "faq_subtab" = "trblsht_subtab",
   #                        "trblsht_subtab" = "faq_subtab"
    #)
#    updateTabItems(session, "sbMenu", faq_trblsht)
 # })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FAQ | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Troubleshoot | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #RETURN TO HOMEPAGE BUTTON
#  observeEvent(input$trbl_rtn_home, {
 #   trbl_home <- switch(input$sbMenu, 
  #                      "trblsht_subtab" = "overview_tab",
   #                     "overview_tab" = "trblsht_subtab"
    #)
#    updateTabItems(session, "sbMenu", trbl_home)
 # })
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Troubleshoot | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OBR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #RETURN TO HOMEPAGE BUTTON
  observeEvent(input$obr_rtn_home, {
    obr_home <- switch(input$sbMenu, 
                       "obrdata_subtab" = "overview_tab",
                       "overview_tab" = "obrdata_subtab"
    )
    updateTabItems(session, "sbMenu", obr_home)
  })
  
  #ACCESS INFLATION TABLES PAGE BUTTON
  observeEvent(input$obr_to_historic, {
    obr_historic <- switch(input$sbMenu, 
                        "obrdata_subtab" = "historic_subtab",
                        "historic_subtab" = "obrdata_subtab"
    )
    updateTabItems(session, "sbMenu", obr_historic)
  })
  #~~~~DOWNLOAD OBR DATA WEBLINKS~~~~~~~~
  
  #Historical Official Forecasts Database 
  obr_forecasts <- a("Historical Official Forecasts Database",
                      href="https://obr.uk/download/historical-official-forecasts-database/")
  
  output$obr_download_forecasts <- renderUI({
    tagList("", obr_forecasts)
  })
  
  #Longterm Economic Determinants
  obr_determinants <- a("Long-term Economic Determinants",
                      href="https://obr.uk/download/long-term-economic-determinants-march-2023-economic-and-fiscal-outlook/?tmstv=1686928506")
  
  output$obr_download_determinants <- renderUI({
    tagList("", obr_determinants)
  })
  
  #March 2022 Economic and Fiscal Outlook
  obr_outlook <- a("Economic & Fiscal Outlook Supplementary Tables",
             href="https://obr.uk/download/march-2023-economic-and-fiscal-outlook-supplementary-economy-tables/?tmstv=1686928506")
  
  
  output$obr_download_outlook <- renderUI({
    tagList("", obr_outlook)
  })
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OBR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HISTORIC INDICES | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #RETURN TO HOMEPAGE BUTTON
  observeEvent(input$historic_rtn_home, {
    hist_home <- switch(input$sbMenu, 
                       "historic_subtab" = "overview_tab",
                       "overview_tab" = "historic_subtab"
    )
    updateTabItems(session, "sbMenu", hist_home)
  })
  
  #ACCESS OBR DATA PAGE BUTTON
  observeEvent(input$historic_to_obr, {
    historic_obr <- switch(input$sbMenu, 
                           "historic_subtab" = "obrdata_subtab",
                           "obrdata_subtab" = "historic_subtab"
    )
    updateTabItems(session, "sbMenu", historic_obr)
  })
 
  
 #~~~~~~~~~~~~~~~ HELP BUTTONS  ~~~~~~~~~~~~~~~~
  
  # INDEX DEFINITIONS
   observeEvent(input$index_help_box, {
     showModal(modalDialog(Title = "Index Definitions",
                           tags$ul(
                             tags$li(h3("AWE"),
                                     h4("The Average Weekly Earnings Index measures the changes in employees’ average weekly earnings. As well as tracking changes in earnings,
                                        AWE makes an explicit estimate of earnings in pounds. It is useful for adjusting staff costs.")),
                             tags$br(),    
                             tags$li(h3("CPI"),
                                     h4("The Consumer Price Index is the price of a weighted average market basket of consumer goods and services purchased by households.
                                        It is especially useful for commercial costs.")),
                             tags$br(),    
                             tags$li(h3("GDP Deflator"),
                                     h4("GDP deflator is a measure of the level of prices of all new, domestically produced, final goods and services in an economy in a year.
                                        Predominantly used to adjust prices of non-market goods and services.")),
                             tags$br(),
                             tags$li(h3("RPI*"),
                                     h4("The Retail Price Index measures the change in the cost of a representative sample of retail goods and services.
                                        It differs from CPI based on the formulae used to construct the indices. ")),
                             tags$br(),    
                             tags$li(h3("RPIX*"),
                                     h4("RPIX is equivalent to all the RPI items excluding mortgage interest payments.")),
                             ),
                           tags$br(),
                           div(style="color:red",
                               h4("* RPI and RPIX are not recommended to be used. RPI has not been considered a National Statistic since 2013 and is likely to overstate
                                  inflation. If you or a colleague needs to use either of these indices, please contact the Economic Appraisal Hub for advice.")),
                           footer = modalButton("Dismiss"),
                           size = "l",
                           easyClose = T,
                           fade = T))
     })
   
   # INDEX vs YoY% HELP
   observeEvent(input$index_yoy_help_box, {
     showModal(modalDialog(Title = "Year-on-Year % Growth vs. Index",
                           tags$ul(
                             tags$li(h3("Year on Year (YoY) % Growth:"),
                                     h4("The price change compared to the same period a year prior.")),
                             tags$br(),    
                             tags$li(h3("Index:"),
                                     h4("The price level in any given year, relative to the base year.")),
                             tags$br(),
                             h3("Both are interchangeable if used correctly, so it often comes down to ease and purpose, plus personal preference. Understanding the relationship
                                between the two, and how to use them both, is important."),
                             h3("E.g., a 5% year-on-year inflation rate represents a movement in an index from 100 to 105, or from 200 to 210. The index can be calculated as
                                100 × (1 + 5%) = 105; or the inflation rate can be calculated as (105 ÷ 100) - 1 = 5%."),
                             h3("It may be easier for some to understand what 5% inflation means, rather than comparing index values of 100 and 105. On the other hand, it may
                                be easier to understand the change over long periods of time using index values.")
                             ),
                           footer = modalButton("Dismiss"),
                           size = "l",
                           easyClose = T,
                           fade = T))
   })
   
   # INDEX BASE YEARS HELP
   observeEvent(input$base_year_help_box, {
     showModal(modalDialog(Title = "Indices Base Years",
                           tags$ul(
                             tags$li(h3("AWE: 2008Q1 = 100")),
                             tags$br(),    
                             tags$li(h3("CPI: 2015 = 100")),
                             tags$br(),    
                             tags$li(h3("GDP Deflator: 2016 = 100")),
                             tags$br(),
                             tags$li(h3("RPI: January 1987 = 100")),
                             tags$br(),    
                             tags$li(h3("RPIX: January 1987 = 100"))
                             ),
                           footer = modalButton("Dismiss"),
                           size = "l",
                           easyClose = T,
                           fade = T))
   })
   
   # PERIOD PICKER HELP
   observeEvent(input$period_help_box, {
     showModal(modalDialog(Title = "Period Definitions",
                           tags$ul(
                             tags$li(h3("Calendar Year:"),
                                     h4("A one-year period that begins on January 1st and ends on December 31st.")),
                             tags$br(),    
                             tags$li(h3("Financial Year:"),
                                     h4("A 12-month period that spans across two calendar years, starting on April 6th of the first year and ending April 5th of the second year.")),
                             tags$br(),    
                             tags$li(h3("Quarterly:"),
                                     h4("Disaggregates the calendar year into 3-month periods. Q1 referring to Jan-Mar, Q2 Apr-Jun, Q3 Jul-Sep, Q4 Oct-Dec.")),
                             tags$br(),
                             h3("The standard approach is to use Financial Year (FY) periods for all purposes."),
                             h3("Deviations are acceptable under specific circumstances when inflating values – e.g. in commercial services wherein prices are indexed
                                at atypical points in time. Furthermore, it may be appropriate to apply inflation on a lagged basis in certain circumstances – e.g.
                                in commercial services where prices are indexed based on historical inflation rates.")
                           ),
                           footer = modalButton("Dismiss"),
                           size = "l",
                           easyClose = T,
                           fade = T))
   })
   
   
  #~~~~~~~~~~~~~~~~~~~~ HISTORIC INDEX DATATABLES ~~~~~~~~~~~~~~~~~~~~~~~ 
  
  #READ IN CUSTOM EXCEL FILE FROM S3 WHICH CONTAINS INFLATION FIGURES FROM OBR
   
  ######## YOY = GROWTH FIGURES, INDEX = INDEX. 
  ######## IF ADDING EXTRA INDICES TO THE TABLE, MAKE SURE TO CHANGE THE SELECTED RANGE
  df_yoy <- botor::s3_read("s3://alpha-app-inflation-tool/DA_inflation_tool_indexation_table_June_2023.xlsx", readxl::read_excel, sheet = "YOY", range = "A1:F150")
  df_index <- botor::s3_read("s3://alpha-app-inflation-tool/DA_inflation_tool_indexation_table_June_2023.xlsx", readxl::read_excel, sheet = "INDEX", range = "A1:F150")
  
  
  #~~~~~~~~~~~~~~~~~~~ FILTER INDEX DATA BASED ON USER SELECTION ~~~~~~~~~~
  
 #~~~~~~~~~~~~ FILTER BASED ON PERIOD ~~~~~~~~~~~
  
  #~~~~~~~~~ YOY TABLE ~~~~~~~~~~~
  df_yoy_filtered <- reactive({
    if({
      input$period_picker == "Quarterly"
    })
      df_yoy %>%
      filter(grepl('Q|Onwards', Period))
    
    else if({
      input$period_picker == "Financial Year"
    })
      df_yoy %>%
      filter(grepl('-|Onwards', Period))
    
    else if({
      input$period_picker == "Calendar Year"
    })
      df_yoy %>%
      filter(!grepl('Q|-', Period))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~ INDEX TABLE ~~~~~~~~~~~
  df_index_filtered <- reactive({
    if({
      input$period_picker == "Quarterly"
    })
      df_index %>%
      filter(grepl('Q', Period))
    
    else if({
      input$period_picker == "Financial Year"
    })
      df_index %>%
      filter(grepl('-', Period))
    
    else if({
      input$period_picker == "Calendar Year"
    })
      df_index %>%
      filter(!grepl('Q|-', Period))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #RENDER YOY DATA TABLE
  output$yoy_table <- renderDataTable({
    
    DT::datatable(df_yoy_filtered(),
                  extensions = c("Buttons", "Scroller", "FixedHeader"),
                  options = list(
                    dom = "Bit",
                    #COPY DATA AND DOWNLOAD TO EXCEL/CSV BUTTONS                  
                    buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download")),
                    #TABLE SCROLL FUNCTIONALITY
                    deferRender = T,
                    scrollY = 600,
                    scroller = T,
                    fixedHeader = T
                  ),
                  
                  rownames = F,
                  class = "display cell-border") %>%
      #IF ADDING INDICES, NEED TO UPDATE CHOSEN COLUMNS
      formatRound(columns = c(2,3,4,5,6) , digits = 2)
  })
  
  #RENDER INDEX DATA TABLE
  output$index_table <- renderDataTable({
    
    DT::datatable(df_index_filtered(),
                  extensions = c("Buttons", "Scroller", "FixedHeader"),
                  options = list(
                    dom = "Bit",
                    #COPY DATA AND DOWNLOAD TO EXCEL/CSV BUTTONS                  
                    buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download")),
                    #TABLE SCROLL FUNCTIONALITY
                    deferRender = T,
                    scrollY = 600,
                    scroller = T,
                    fixedHeader = T
                    ),
                  
                  rownames = F,
                  class = "display cell-border") %>%
      #IF ADDING INDICES, NEED TO UPDATE CHOSEN COLUMNS
      formatRound(columns = c(2,3,4,5,6) , digits = 1)
    
    
    #download tables - change file name to include date and when/where data came from
    
    
    })
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HISTORIC INDICES | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDEXATION CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #RETURN TO HOMEPAGE BUTTON
  observeEvent(input$calculator_rtn_home, {
    calculator_home <- switch(input$sbMenu, 
                       "calculator_tab" = "overview_tab",
                       "overview_tab" = "calculator_tab"
    )
    updateTabItems(session, "sbMenu", calculator_home)
  })
  
  
  output$dl_excel_calc <- downloadHandler(
    filename = function() {
      paste0("indexation_tool_excel -", Sys.Date(), ".xlsx", sep='')
      
    },
    content = function(con) {
      xlsx <- read_xlsx('www/indexation_tool_excel.xlsx') #read_xlsx must be "officer" package
      print(xlsx, target = con)
    }
  )
#For future, change excel tool to xlsm version - this requires different package to "officer". Must also update the guidance                                          

  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INDEXATION CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
})
