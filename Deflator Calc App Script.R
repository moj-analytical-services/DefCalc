# A mock up version done by Thomas 
# The first time you run this, you will need to run install.packages("rhandsontable")

library(rhandsontable)
library(dplyr)
library(shiny)
library(readr)

indexes <- read.csv("Background data indexes FORMATTED.csv")
GDP_June_2019_R <- indexes$GDP_June_2019/100

run_calculations <- function(input_table){
  # This function would be where you put the calculations, which transforms the input table into the output table..
  # In this example, this just doubles one of the columns.
  input_table %>%
    mutate(GDP_June_2019 = GDP_June_2019 * GDP_June_2019_R)
  
}


ui <- shinyUI(fluidPage(
  titlePanel("Handsontable"),
  sidebarLayout(
    sidebarPanel(
      # Ignore this bit - doesn't do anything. I just copied this from the rhandsontable example.
      # The sidebarpanel is where you can put interfacy things, if you're so inclined.
      helpText("Select the index that you want to apply to your figures from the drop-down list below"),
      selectInput("column", "Index", names(indexes)),
      selectInput("base", "Base Year", c("2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20"))
    ),
    mainPanel(
      fluidRow(
        column(6,rHandsontableOutput("hot_input")),
        column(6,rHandsontableOutput("hot_output"))
      )
      
    )
  )
))

server <- function(input, output, session) {
  
  # Trying to set it so the dataset changes to show column selected. 
  selectedData <- reactive({
    indexes[, c(input$column)]
  })
  
  # This creates a reactive dataset that the output table can use.
  # It runs the run_calculations() function on the input table, and updates whenever the input table changes.
  output_data = reactive({
    if (!is.null(input$hot_input)) {
      DF = hot_to_r(input$hot_input) %>% run_calculations()
    } else {
      DF = indexes
    }
    
    return(DF)
  })
  
  output$hot_input <- renderRHandsontable({
    # mtcars is a builtin dataset. You will want to have a separate function to create a template dataset.
    # Probably would be easiest to make the template in Excel and read that in as a CSV
    rhandsontable(indexes)
  })
  
  output$hot_output <- renderRHandsontable({
    DF = output_data()
    if (!is.null(DF))
      rhandsontable(DF)
  })
}


shinyApp(ui, server)