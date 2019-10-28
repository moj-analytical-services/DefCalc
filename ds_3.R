#Necessary packages
library(httr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(s3tools)

#Import online Excel OBR datafile into R, through an indirect URL
obr_url = GET("https://obr.uk/download/march-2019-economic-and-fiscal-outlook-supplementary-economy-tables/") #update url when new database available
temp_obr_xlsx = tempfile(fileext = ".xlsx")
download.file(obr_url$url, destfile = temp_obr_xlsx,mode = "wb")
obr_xlsx = read_excel(temp_obr_xlsx, sheet = "1.7") #1.7 is current tab name for Inflation tab

#Clean data to prepare for app
#removes empty columns based on if they're entirely 'NA'
obr_xlsx <- obr_xlsx[, colSums(is.na(obr_xlsx)) != nrow(obr_xlsx)]
#removes extra rows if they contain any 'NA'; Column 1 assumed to be 'Period', so name is 'NA'
obr_xlsx <- obr_xlsx[complete.cases(obr_xlsx[ , 2:ncol(obr_xlsx)]), ]

#convert first row into column names. Explictly assumes Column 1 is 'Period'.
colnames(obr_xlsx)<- obr_xlsx[1,] #copy Row 1 to Column names
obr_xlsx <- obr_xlsx[-1,] #deletes Row 1
names(obr_xlsx)[1] <- "Period" #creates column name for 'Period'

#renames new column names based on if they are YoY changes, or Indices
colnames(obr_xlsx) <- make.unique(colnames(obr_xlsx))
obr_xlsx_yoy <- obr_xlsx %>% select(-ends_with(".1"))
obr_xlsx_index <- obr_xlsx %>% select(ends_with(".1"))
colnames(obr_xlsx_index) <- paste("index", colnames(obr_xlsx_yoy[2:ncol(obr_xlsx_yoy)]), sep = "_")
colnames(obr_xlsx_yoy) <- paste("yoy", colnames(obr_xlsx_yoy), sep = "_")
obr_xlsx <- cbind.data.frame(obr_xlsx_yoy, obr_xlsx_index)
names(obr_xlsx)[names(obr_xlsx)=="yoy_Period"] <- "Period" #time period reference

#creates dataframes for data by quarter, annual and financial year splits flexibly (i.e. auto-updates with new spreadsheet)
obr_xlsx_qtr <- filter(obr_xlsx, grepl("Q", obr_xlsx$Period)) #quarterly
obr_xlsx_pa <- filter(obr_xlsx, nchar(obr_xlsx$Period, type = "chars") == 4, TRUE) #calendar
obr_xlsx_fy <- filter(obr_xlsx, grepl("/", obr_xlsx$Period))  #financial

#Adds row names to all dataframes (doing this at the start doesn't copy them to new dataframes)
obr_xlsx <- data.frame(obr_xlsx, row.names = 1)
obr_xlsx_qtr <- data.frame(obr_xlsx_qtr, row.names = 1)
obr_xlsx_pa <- data.frame(obr_xlsx_pa, row.names = 1)
obr_xlsx_fy <- data.frame(obr_xlsx_fy, row.names = 1)

#Creates one workbook as .xlsx
obr <- createWorkbook()
addWorksheet(obr, sheet = "all")
addWorksheet(obr, sheet = "qtr")
addWorksheet(obr, sheet = "pa")
addWorksheet(obr, sheet = "fy")
writeData(obr, sheet = "all", obr_xlsx, colNames = TRUE, rowNames = TRUE)
writeData(obr, sheet = "qtr", obr_xlsx_qtr, colNames = TRUE, rowNames = TRUE)
writeData(obr, sheet = "pa", obr_xlsx_pa, colNames = TRUE, rowNames = TRUE)
writeData(obr, sheet = "fy", obr_xlsx_fy, colNames = TRUE, rowNames = TRUE)
saveWorkbook(obr, "obr.xlsx", overwrite = TRUE)

#Save dataframes as both .xlsx and .csv files (only .csv is used in app)
library(s3tools)
write_file_to_s3("obr.xlsx", "alpha-sandbox/obr.xlsx", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx, "alpha-sandbox/obr_all.csv", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx_qtr, "alpha-sandbox/obr_qtr.csv", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx_pa, "alpha-sandbox/obr_pa.csv", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx_fy, "alpha-sandbox/obr_fy.csv", overwrite = TRUE)

#deletes obr.xlsx from directory
file.remove("obr.xlsx")

