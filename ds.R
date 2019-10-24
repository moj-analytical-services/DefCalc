#Necessary packages
library(httr)
library(readxl)
library(openxlsx)
library(dplyr)
library(s3tools)

#Import online Excel OBR datafile into R, through an indirect URL
obr_url = GET("https://obr.uk/download/march-2019-economic-and-fiscal-outlook-supplementary-economy-tables/") #update url when new database available
temp_obr_xlsx = tempfile(fileext = ".xlsx")
download.file(obr_url$url,destfile = temp_obr_xlsx,mode = "wb")
obr_xlsx = read_excel(temp_obr_xlsx,sheet = "1.7") #1.7 is current tab name for Inflation tab

#Clean data to prepare for app
#removes empty first column and removes all extra rows
obr_xlsx <- obr_xlsx[2:18] #removes column 1 (blank)
obr_xlsx <- obr_xlsx %>% slice(4:100) #removes extra rows
#renames all columns
#year on year (% change)
names(obr_xlsx)[names(obr_xlsx)=="...2"] <- "yoy_Period" #time period reference
names(obr_xlsx)[names(obr_xlsx)=="...3"] <- "yoy_Retail Price Index" #Retail Prices Index
names(obr_xlsx)[names(obr_xlsx)=="...4"] <- "yoy_Retail Price Index (exc MIP) " #RPI excluding mortgage interest payments
names(obr_xlsx)[names(obr_xlsx)=="...5"] <- "yoy_Consumer Price Index" #Consumer Price Index
names(obr_xlsx)[names(obr_xlsx)=="...6"] <- "yoy_Producer Output Prices" #Producer Output Prices
names(obr_xlsx)[names(obr_xlsx)=="...7"] <- "yoy_Mortgage Interest Payments" #Mortgage Interest Payments
names(obr_xlsx)[names(obr_xlsx)=="...8"] <- "yoy_Actual Rents for Housing" #Actual rents for housing
names(obr_xlsx)[names(obr_xlsx)=="...9"] <- "yoy_Consumer Expenditure Deflator" #Consumer expenditure deflator
names(obr_xlsx)[names(obr_xlsx)=="...10"] <- "yoy_Gross Domestic Product Deflator" #Gross Domestic Product deflator
#index figures (currently unused due to how the app works)
names(obr_xlsx)[names(obr_xlsx)=="...11"] <- "index_RPI" #Jan1987=100
names(obr_xlsx)[names(obr_xlsx)=="...12"] <- "index_RPIX" #Jan1987=100
names(obr_xlsx)[names(obr_xlsx)=="...13"] <- "index_CPI" #2015=100
names(obr_xlsx)[names(obr_xlsx)=="...14"] <- "index_POP" #2010=100
names(obr_xlsx)[names(obr_xlsx)=="...15"] <- "index_MIP" #Jan1987=100
names(obr_xlsx)[names(obr_xlsx)=="...16"] <- "index_ARH" #2015=100
names(obr_xlsx)[names(obr_xlsx)=="...17"] <- "index_CExDef" #2016=100
names(obr_xlsx)[names(obr_xlsx)=="...18"] <- "index_GDPdef" #2016=100

#creates datatables for data by quarter, annual, and financial year splits
obr_xlsx_qtr <- obr_xlsx %>% slice(1:65) #quarterly
obr_xlsx_pa <- obr_xlsx %>% slice(66:81) #calendar
obr_xlsx_fy <- obr_xlsx %>% slice(82:97) #financial year

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
