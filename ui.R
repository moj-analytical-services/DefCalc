#Install botor using reticulate::py_install("boto3"), followed by renv::install(botor)
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
library(shinyGovstyle)
library(shinycssloaders)

shinyUI(dashboardPage(
  dashboardHeader(
    title = "Indexation Tool"
    
    ),
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SIDEBAR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  
  sidebar = dashboardSidebar(useShinyjs(),
                             width = "350px",
                             collapsed = T,
                             sidebarMenu(id = "sbMenu",
                                         
                                         #Guidance
                                         menuItem("Overview", tabName = "overview_tab", icon = icon("house-chimney")
                                                  ),
                                                             
                                         menuItem("Guidance", tabName = "guidance_tab", icon =  icon("dice-d6"),
                                                  menuSubItem("Guidance", tabName = "guidance_subtab", icon = icon("circle-info")), 
                                                  menuSubItem("FAQ", tabName = "faq_subtab", icon = icon("comments"))#,
                                              #    menuSubItem("Troubleshooting", tabName = "trblsht_subtab", icon = icon("circle-question"))
                                                  ),
                                         
                                         #Data Repository
                                         menuItem("Index Repository", tabName = "repository_tab", icon = icon("database"),
                                                  menuSubItem("OBR Data", tabName = "obrdata_subtab", icon = icon("file-export")),
                                                  menuSubItem("OBR Forecasts", tabName = "historic_subtab", icon = icon("chart-simple"))
                                                  ),
                                                             
                                         #Indices tab
                                         menuItem("Inflation Calculator", tabName = "calculator_tab", icon = icon("calculator")
                                                  )
                                         )

                             ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SIDEBAR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

#~~~~~~~~~~~~~~~~ BODY CONTENT START ~~~~~~~~~~~~~~~~~~
dashboardBody(
  tabItems(
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HOMEPAGE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    tabItem(tabName = "overview_tab",
            fluidRow(
              tags$div(htmlTemplate("www/title.html", height = "10px"))
              ),
            
            # WELCOME BOX                               
            fluidRow(
              box(background = "navy", width = 12, h1("Welcome"),
                  tags$hr(),
                  tags$ul(
                    tags$li(h4("The Data and Analysis Indexation Tool allows users to view inflation forecasts, access historic economic data and calculate custom inflation indices.")),
                    tags$li(h4("Version 1.0.0 last updated 01/07/2023 using data published by the Office for Budget Responsibility (OBR) in the following publication:",
                               uiOutput("obr_site1")))
                  )
                  
                  
                  #version number,last updated, and message. latest obr data date
                  )
              ), 
            
            # ACCESS GUIDANCE/FAQ BOX
            fluidRow(
              box(background =  "purple", height = 240,
                  div(style="display: inline-block;vertical-align:top; width: 40px;", h2(icon("dice-d6"))),
                  div(style="display: inline-block;vertical-align:top; width: 380px;", h2("Guidance and FAQ")),
                  h3("For guidance on using this app:", 
                     actionBttn("guidance_switch", "Click Here", style = "unite", size = "md", color = "default")
                     ),
              #    tags$br(),
                  h3("For frequently asked questions:",
                     actionBttn("faq_switch", "Click Here", style = "unite", size = "md", color = "default")
                     ),
              #ADD IN TROUBLESHOOTING LATER IN DEVELOPMENT
                 # h3("For troubleshooting:",
                  #   actionBttn("trblsht_switch", "Click Here", style = "unite", size = "md", color = "default")
                  #)
                  ),
              
              # ACCESS OBR DATA/ HISTORIC INDICES BOX
              box(background = "olive", height = 240,
                  div(style="display: inline-block;vertical-align:top; width: 40px;", h2(icon("database"))),
                  div(style="display: inline-block;vertical-align:top; width: 380px;", h2("Index Repository")),
                  h3("To access the OBR datasets used in this app:",
                     actionBttn("obrdata_switch", "Click Here", style = "unite", color = "default")),
                  tags$br(),
                  h3("To view the latest OBR inflation forecasts:",
                    actionBttn("historic_switch", "Click Here", style = "unite", color = "default"))
                  )
              ),
            
            fluidRow(
              # ACCESS CALCULATOR BOX
              box(background = "light-blue", height = 240,
                  div(style="display: inline-block;vertical-align:top; width: 40px;", h2(icon("calculator"))),
                  div(style="display: inline-block;vertical-align:top; width: 380px;", h2("Inflation Calculator")),
                  h3("To access the inflation calculator tool:",
                     actionBttn("calculator_switch", "Click Here", style = "unite", size = "md", color = "default")),
                  tags$br(),
                  tags$br()
                  ),
              
              # ACCESS HELPFUL LINKS BOX
              box(background = "yellow", height = 240,
                  div(style="display: inline-block;vertical-align:top; width: 40px;", h2(icon("link"))),
                  div(style="display: inline-block;vertical-align:top; width: 480px;", h2("Helpful Links and Documents")),
                  tags$ul(
                    tags$li(h3(downloadLink("downloadinflationguidance", label = "General Inflation Guidance"))
                            ),
                    tags$li(
                      h3(withSpinner(uiOutput("obr_site"), proxy.height = "30px"))
                            ),
                    tags$li(div(style="display: inline-block;vertical-align:middle; width: 300px;",
                                h3("For Further Queries, Contact:")),
                            div(style="display: inline-block;vertical-align:middle; width: 260px;",
                                h3(uiOutput("contact_email_home")))
                            )
                    )
                  )
              )
            ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HOMEPAGE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
 

   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GUIDANCE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    tabItem(tabName = "guidance_subtab",
            # RETURN TO HOMEPAGE BUTTON
            actionBttn("guidance_rtn_home", "Return to Homepage", style = "unite", color = "primary", block = F),
            # SWITCH TO FAQ PAGE BUTTON
            actionBttn("guidance_to_faq", "Click for FAQ", style = "unite", color = "royal", block = F),
            
            h1("Guidance"),
            fluidRow(
              box(width = 12, height = 200,
                  h4("Supporting guidance can be found here:"),
                  tags$ul(
                    tags$li(h3(downloadLink("appguidanceppt", label = "App Guidance"))),
                    tags$li(h3(downloadLink("inflationguidanceppt", label = "Inflation Guidance")))
                    )
                  )
              ),
            
            column(width = 4,
                   box(width = NULL,
                       height = 400,
                       h2("OBR Data"),
                       tags$ul(
                         tags$li(h4("This section contains download links to various OBR data sets that are used across government, including the inflation figures used in this app's Forecast and Calculator pages.")),
                         tags$li(h4("Users can download raw data and underlying figures behind various economic and public sector financial forecasts.")),
                         tags$li(h4("As well as inflation, these files contain forecasts and historic outturn data for fiscal aggregates and GDP components"))
                         ),
                       tags$hr(),
                       div(style="display: inline;vertical-align:bottom; position: absolute; bottom: 0px",
                       h3(actionBttn("guidance_obr", "Click Here", style = "stretch", color = "success"),
                          "to see OBR Data"))
                       )
                   ),
            
            column(width = 4,
                   box(width = NULL,
                       height = 400,
                       h2("Inflation Forecasts"),
                       tags$ul(
                         tags$li(h4("This page contains OBR inflation forecasts and historic outturn data.")),
                         tags$li(h4("Users can download both year-on-year growth percentages as well as indexing figures."))
                         ),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$hr(),
                       div(style="display: inline;vertical-align:bottom; position: absolute; bottom: 0px",
                       h3(actionBttn("guidance_historic", "Click Here", style = "stretch", color = "success"),
                          "to see Inflation Forecasts"))
                       )
                   ),
            
            column(width = 4,
                   box(width = NULL,
                       height = 400,
                       h2("Index Calculator"),
                       tags$ul(
                         tags$li(h4("This page contains a calculator to allow users to add their own custom indices.")),
                         tags$li(h4("Users can inflate and deflate financial costs from one time period to another, in line with HMT Green Book guidance.")),
                         ),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$hr(),
                       div(style="display: inline;vertical-align:bottom; position: absolute; bottom: 0px",
                       h3(actionBttn("guidance_calculator", "Click Here", style = "stretch", color = "success"),
                          "to use the Calculator"))
                       )
                   ),
            
            fluidRow(
              box(width = 12,
                  h3("Note:"),
                  tags$ul(
                    tags$li(h4("Data for before 2008 is not available as anything prior to this date is not appropriate for data analysis - should the user wish to access this historic data, it can be downloaded via the OBR Data page."))
                    )
                  )
              )
            
            ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GUIDANCE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FAQ | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

    tabItem(tabName = "faq_subtab",
            
            # RETURN TO HOMEPAGE BUTTON
            actionBttn("faq_rtn_home", "Return to Homepage", style = "unite", color = "primary", block = F),
            
            #RETURN TO GUIDANCE
            actionBttn("faq_to_guidance", "Return to Guidance", style = "unite", color = "royal", block = F),
            
            h1("Frequently Asked Questions"),
            
            #FAQ ACCORDION
            fluidRow(
              box(title = h3("What Is Inflation?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("Inflation can be described as a measure of price changes over time. This can be either positive (i.e. increasing prices) or negative (i.e. decreasing prices). There are several different types of inflation,
                     all which measure the price changes of different goods and services."),
                  tags$br(),
                  div(style="display: inline-block;vertical-align:top; width: 300px;", h4("Further information can be found here:")),
                      div(style="display: inline-block;vertical-align:top; width: 150px;", h4(downloadLink("FAQinflationguidance", label = "Inflation Guidance"))),
              ),
              
              box(title = h3("What Are The Common Inflation Measures?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("The most common inflation measures are:"),
                  tags$ul(                  
                    tags$li(h4(div(style = "font-weight:bold", "Consumer Price Index (CPI):"),
                               "measures the price changes in a ‘typical basket of goods and services’ which people can be expected to purchase. CPIH extends this ‘basket’ and includes housing costs as well.")),
                    
                    tags$li(h4(div(style = "font-weight:bold", "Average Weekly Earnings (AWE):"),
                               "measures price changes in employee wages and salaries.")),
                    
                    tags$li(h4(div(style = "font-weight:bold", "Building Cost Information Service (BCIS):"),
                               "measures price changes in building and construction-related goods and services.")),
                    
                    tags$li(h4(div(style = "font-weight:bold", "GDP Deflator:"),
                               "measures general inflation of the whole domestic economy – unlike other indices, this includes investment activities, government expenditure, and exports (but excludes imports)."))
                  ),
                  tags$br(),
                  h4("All of these indices are general measures and consist of multiple subsets of indices for specific of goods and services. For example, CPI can be broken down into CPI - Food, or even further into CPI – Cake; and AWE
                     can be broken down into AWE – Private Sector, or AWE – Retail Services. However, these breakdowns are not necessarily available as future forecasts, but are published as historic records by the Office for National Statistics (ONS).")
              ),
            ),
            
            fluidRow(
              box(title = h3("How Do I Know Which Inflation Measure To Use?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("It is important to involve many different stakeholders when deciding which measure is most appropriate to inflate different goods and services. At a minimum, this should include: analytical, commercial, and finance expertise."),
                  tags$br(),
                  h4("The most common inflation measures are:"),
                  tags$ul(    
                    tags$li(h4(div(style = "font-weight:bold", "Consumer Price Index (CPI):"),
                               "CPI, or CPIH, and their subsets are the defaults for many inflation-linked price changes within the department, particularly for general goods and services.")),
                    
                    tags$li(h4(div(style = "font-weight:bold", "Average Weekly Earnings (AWE):"),
                               "AWE is most appropriate to use for staff-related earnings and payments (e.g. wages and salaries).")),
                    
                    tags$li(p(div(style = "font-weight:bold", "Building Cost Information Service (BCIS):"),
                               "BCIS is a useful measure for construction and maintenance related expenditure (e.g. building new physical infrastructure, and facilities management).
                               However, the data is privately produced and not necessarily representative of the sector."), uiOutput("BCIS_site")),
                    
                    tags$li(h4(div(style = "font-weight:bold", "GDP Deflator:"),
                               "this is predominantly used for non-market goods and services (e.g. carbon costs), and for converting prices into real prices. Advice from economists should be sought before using it for other purposes."))
                  ),
                  tags$br(),
                  h4("It is also advised to use specific subsets of indices, should suitable alternatives to the broad and high-level indices be identified. These subsets are able to more accurately reflect the prices of those goods and services
                     and have been evidenced across government to improve price certainty and generate savings."),
                  tags$br(),
                  h4("Note that the Retail Price Index (RPI) has intentionally been excluded from this list. Its use is strongly discouraged except where strictly necessary because its ability to accurately estimate inflation is problematic,
                     and is set to be discontinued.", uiOutput("ONS_site"))
              ),
              
              box(title = h3("What Is The GDP Deflator?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("The GDP Deflator allows for the effect of general economy-wide inflation to be removed from prices. This is because it can measure the change in the volume of goods and services in an economy. Consequently,
                     it can be used to demonstrate actual, or ‘real’, growth.", uiOutput("gdp_site"))        
              )
            ),
            
            fluidRow(
              box(title = h3("What Is Discounting?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("Discounting is based on the economic concept of time preference, which is that generally people prefer value now rather than later. Discounting converts costs and benefits into present values by allowing 
                     for society’s preference for now compared with the future. It is used to allow comparison of future values in terms of their value in the present."),
                  tags$br(),
                  h4("Note that this is a distinct concept to inflation, but it is an important concept and is often confused with indexation. Further details can be found in the HM Treasury’s Green Book (Annex 6).")
              ),
              
              box(title = h3("What Are Nominal Prices?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("Nominal prices are those which have been adjusted for inflation and are equal to what something actually costs (i.e. what you would have to pay to purchase the good or service).For example, if a cup of coffee cost £2
                     in 2020 and inflation in 2021 is 10%, the nominal price (i.e. cost) in 2021 would be £2.20. This calculation is as follows: £2.00 × (1 + 10%) = £2.20.")
              )
            ),
            
            fluidRow(
              box(title = h3("What Are Real/Present Prices?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h3(div(style = "font-weight:bold", "Real Prices")),
                  h4("Real prices are those which have had the impact of general inflation removed, relative to a specific ‘price base year’, via the standard method of using GDP Deflators. This is not the same as simply not adjusting prices for inflation
                     – thus, only nominal prices can be converted to real prices. The distinction is important as it enables a true comparison of prices in economic appraisals. Furthermore, this comparison requires setting a ‘price base year’."),
                  tags$br(),
                  h4("For example, if a cup of coffee cost £2.00 in 2020, or £2.20 in 2021, the inflation rate is 10%. If the GDP Deflator for this period was 5%, then the cup of coffee’s nominal price of £2.20 in 2021 would be almost equivalent
                     to a ≈£2.10 real price (rebased to 2020), rather it’s £2.00 nominal price."),
                  tags$hr(),
                  h3(div(style = "font-weight:bold", "Present Prices")),
                  h4("Present prices are future prices which have been discounted to a specific period, which is typically the current financial year. Only real prices can be discounted and converted to present prices.")
              ),
              
              box(title = h3("What Is A Price Base Year?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("A Price Base Year refers to the period in which prices are compared against. For most indices, the price base year is represented by a value of 100. A value above 100 represents a price increase (i.e. positive inflation),
                     and a value below 100 represents a price decrease (i.e. negative inflation)."),
                  tags$br(),
                  h4("For real prices, this is applied to show the relative price of specific periods against the current period. This means the index first has to be rebased so that the current period becomes the base period. Rebasing requires
                     dividing the index value for each year against the new base year and multiplying the output by 100. For example, an index with values of 200 in 2020 and 220 in 2021 can be rebased as follows to make 2020 the base year:
                     (200 ÷ 200) × 100 = 100, and (220 ÷ 200) × 100 = 110.")
              )
            ),
            
            fluidRow(
              box(title = h3("How Do I Apply These Inflation Rates/Indices?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("This data can be used to apply inflation and produce nominal prices, and then subsequently convert them to real prices. For example, if a cup of coffee cost £2 in 2020 and inflation in 2021 is 10%, the price in 2021 would be £2.20.
                     This calculation is as follows: £2.00 × (1 + 10%) = £2.20."),
                  tags$br(),
                  h4("Furthermore, it can be converted to real prices. Given a 5% GDP Deflator rate in 2021, the 2021 nominal price is almost equivalent to ≈£2.10 if rebased to 2020 prices. This calculation is as follows: £2.20 ÷ (1 + 5%) ≈ £2.10."),
                  tags$br(),
                  h4("Whilst these examples use inflation rates, the same effect can be achieved using the index values as well. Note that the Excel Tool can be used to automatically perform these calculations, as well as some extra features.
                     Click below to access."),
                  #ACCESS CALCULATOR
                  actionBttn("faq_calculator", "Inflation Tool", style = "stretch", color = "warning", block = F),
                  
              ),
              
              box(title = h3("When Are Updated Indices Published?"),
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  solidHeader = T,
                  h4("Historic indices are published by the Office for National Statistics (ONS). The ONS publish these forecasts on a monthly basis, with a slight time-lag (e.g. one or two months), and occasionally revise estimates in light of new data.
                     The publications go into details of subsets of indices."),
                  tags$br(),
                  h4("Forecasts are primarily published by the Office for Budget Responsibility (OBR). These forecasts only cover high-level indices such as CPI, but not any subsets. The OBR typically publish new forecasts for key indices in line with major
                     policy announcements, such as the Autumn & Spring Statements. Other publications may be available at different points in time, although the standard approach is to use OBR forecasts unless alternatives are more suitable.")
              )
            ),
      
            
            #CONTACT EMAIL
            h3(icon("address-card"), "Contact"),
            div(style="display: inline-block;vertical-align:top; width: 455px;", h4('If further information or assistance is required, please email:')),
            div(style="display: inline-block;vertical-align:top; width: 250px;", h4(uiOutput("contact_email_FAQ"))),
            
            
        #ADD TROUBLESHOOTING
      #        div(style="display: inline-block;vertical-align: top; position: absolute; right: 30px",
       #           actionBttn("faq_to_trblsht", "Click for Troubleshooting", style = "unite", color = "royal", block = F)
        #          )
              
            ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FAQ | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Troubleshoot | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

#tabItem(tabName = "trblsht_subtab",
        
        # RETURN TO HOMEPAGE BUTTON
 #       actionBttn("trbl_rtn_home", "Return to Homepage", style = "unite", color = "success", block = F),
 
#),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Troubleshoot | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OBR DATA | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

tabItem(tabName = "obrdata_subtab",
        
        # RETURN TO HOMEPAGE BUTTON
        actionBttn("obr_rtn_home", "Return to Homepage", style = "unite", color = "primary", block = F),
        
        #INFLATION TABLES BUTTON
        actionBttn("obr_to_historic", "Click to See Inflation Tables", style = "unite", color = "success", block = F),
        
        #INFO
        h1("OBR Data"),
        h4("This page contains links to the various published OBR data sources that this app uses. To access, simply click the dataset titles below."),
        tags$hr(),
        
        fluidRow(
          column(width = 4,
                 box(width = NULL,
                     height = 500,
                     div(style="display: inline-block;vertical-align:bottom; width: 60px;",
                         actionBttn("test", icon = icon("file-excel"), style = "pill", size = "lg", color = "success")),
                     div(style="display: inline-block;vertical-align:top; width: 480px;", h3(uiOutput("obr_download_forecasts"))),
                     tags$hr(),
                     tags$ul(                  
                       tags$li(h4("This Excel file contains the OBR's historical forecast database, updated at every fiscal event.")),
                       tags$li(h4("It includes forecasts and recent outturn data for many variables from OBR since 2010 – including fiscal aggregates and GDP components.")),
                       tags$li(h4("The database also includes forecasts published by the Treasury prior to 2010 and in some cases as far back as 1970.")),
                       tags$li(h4("Regarding inflation, users can find both outturn data and year-on-year percentage figures for CPI, RPI, GDP Deflator, and Average Earnings (AWE) on 
                                  the contents page under \"Economy forecasts\"."))
                       ),
                     )
                 ),
          
          column(width = 4,
                 box(width = NULL,
                     height = 500,
                     div(style="display: inline-block;vertical-align:bottom; width: 60px;",
                         actionBttn("test", icon = icon("file-excel"), style = "pill", size = "lg", color = "success")
                         ),
                     div(style="display: inline-block;vertical-align:top; width: 480px;", h3(uiOutput("obr_download_determinants"))),
                     tags$hr(),
                     tags$ul(
                       tags$li(h4("This Excel file contains the OBR's long-term economic determinants that underpin their long-term fiscal projections, updated at least once a year.")),
                       tags$li(h4("This spreadsheet can be used by teams to inform their own long-term modelling.")),
                       tags$li(h4("Users can find medium-term and long-term forecasts on CPI, RPI*, RPIX*, GDP Deflator and AWE (listed as Average earnings)." )),
                       tags$br(),
                       tags$li(style="color:red",
                               h4("*Please note: RPI and RPIX are not recommended to be used. RPI has not been considered a National Statistic since 2013 and is likely to overstate
                                  inflation."))
                       )
                     )
                 ),
          
          column(width = 4,
                 box(width = NULL,
                     height = 500,
                     div(style="display: inline-block;vertical-align:bottom; width: 60px;",
                         actionBttn("test", icon = icon("file-zipper"), style = "pill", size = "lg", color = "success")
                         ),
                     div(style="display: inline-block;vertical-align:top; width: 550px;", h3(uiOutput("obr_download_outlook"))),
                     tags$hr(),
                     tags$ul(
                       tags$li(h4("This file contains a collection of supplementary databases published by the OBR that provides more detail on aspects of their forecasts and analysis,
                                  updated with every fiscal event.")),
                       tags$li(h4("See Table 1.6 for Average Weekly Earnings and other Labour Market data")),
                       tags$li(h4("See Table 1.7 for other inflation figures")),
                       tags$li(h4("See Tables 1.20 and 1.21 for CPI category inflation and weighting"))
                       )
                     )
                 )
          )
        ),

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OBR DATA | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HISTORIC INDICES | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
tabItem(tabName = "historic_subtab",
        
        # RETURN TO HOMEPAGE BUTTON
        actionBttn("historic_rtn_home", "Return to Homepage", style = "unite", color = "primary", block = F),
        
        #RETURN TO OBR DATA PAGE BUTTON
        actionBttn("historic_to_obr", "Access OBR Data", style = "unite", color = "success", block = F),
        
        h1("OBR Inflation Forecasts"),
        br(),
        
        #~~~~~~~~~~~~~~~~ INDEX TABLE FILTERS ~~~~~~~~~~~~~~~~~ 
        fluidRow(
          
          #~~~~~~ HELP BOX ~~~~~~~~~~
          box(width = 7,
              h4("Help"),
              p("This page displays the latest inflation forecast figures from the OBR. The forecast figures take two forms: Year-on-Year growth as a percentage,
                and in Index form. To view your preferred form, simply click the tab buttons at the top of the box below."),
              # POP-UP HELP BOXES
              actionButton("index_help_box", "Which Inflation Measure Should I Use?"),
              actionButton("index_yoy_help_box", "Should I use YoY% or Index?"),
              actionButton("base_year_help_box", "Index Base Years")
          ),
          
          #~~~~~~ PERIOD-TYPE FILTER ~~~~~~~~~~
          box(width = 5,
              p("The forecasts displayed in the tables default to financial years; to view quarterly or calendar year forecasts instead, please use the drop box below."),
           
            div(style="display: inline-block; width: 300px",
                pickerInput(inputId = "period_picker",
                        label = "Select period type", 
                        choices = c("Financial Year", "Calendar Year", "Quarterly"),
                        multiple = FALSE
                        )),
            div(style="display: inline-block; vertical-align:middle ",
                actionButton("period_help_box", "Which Period Should I Use?"))
            ),
          ),
        
        #~~~~~~~~~~~~ INDEX TABLE OUTPUT ~~~~~~~~~~~~~~~~
        fluidRow(   
          tabBox(width = 12,
            tabPanel(DT::dataTableOutput("yoy_table"), title = "1) Year-on-Year Growth", width = 12, solidHeader = T, status = "primary"),
            tabPanel(DT::dataTableOutput("index_table"), title = "2) Inflation Indices", width = 12, solidHeader = T, status = "primary"))
          )
        
        ),


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HISTORIC | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CALCULATOR | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

    tabItem(tabName = "calculator_tab",
            
            # RETURN TO HOMEPAGE BUTTON
            actionBttn("calculator_rtn_home", "Return to Homepage", style = "unite", color = "primary", block = F),
            
            h1("Inflation Calculator"),
            
            tags$ul(
              tags$li(h4("This tab contains outputs for creating your own indices.")),
              tags$li(h4("*Note that some of the outputs of this tool are based on estimated/forecasted indices, which are subject to change. Consequently, 
                         any outputs produced by this tool should be treated as an approximation based on the best currently available information - they are 
                         not a definitive prediction of future prices."))
              ),

            fluidRow(
              box(
                h3("Excel Calculator"),
                h4(downloadLink("dl_excel_calc", label = "Click Here to Download")) 
                )
              )
          )


#### ALTERNATIVE SOLUTION: LINK TO SHARED FOLDER WITH ACCESS TO ALL DOCUMENTS/TOOLS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CALCULATOR | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  



  )
)
#~~~~~~~~~~~~~~~~ BODY CONTENT END ~~~~~~~~~~~~~~~~~~
                      
))