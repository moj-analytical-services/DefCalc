#Necessary packages
library(httr)
library(readxl) 
library(openxlsx) #read in/manipulate Excel files
library(tidyverse) #dplyr & readr & stringr (plus other functionalities)
library(s3tools) #importing data from AWS
library(RCurl) #Get information relating to a URL. 

#Create a list of dates between now and 12 months prior. 
date_today <- as.POSIXlt(Sys.Date())
date_back <- date_today
date_back$mon <- date_back$mon - 12
test <- seq(date_back,date_today,"month")
latest_url <- format(seq(date_back,date_today,"month"),"%B-%Y") %>% as.data.frame(col.names = "Dates")
names(latest_url) <- "Date"

#Gives the structure of the URL, which we'll use and replace 'march-2019' with the series of month-years created above.
ourl <- "https://obr.uk/download/march-2019-economic-and-fiscal-outlook-supplementary-economy-tables/"

#Create URLs and test if they exist, create column of URLS and replace with the months_years from above. Check if each URL exists. 
latest_url$urls <- ourl
sub = function(x,y,z)gsub(x,y,z)
latest_url$urls <- mapply(sub,"march-2019",latest_url$Date,latest_url$urls)
latest_url$exist <- sapply(latest_url$urls,url.exists)
latest_url$filename <- paste('Economy_supplementary_tables',paste(latest_url$Date,"xlsx",sep="."),sep="_")

latest_url$filename <- gsub("-","_",latest_url$filename)

#Test if files are in S3 and if they're not then indicate the URL that needs to be downloaded.  
s3files <- s3tools::list_files_in_buckets('alpha-sandbox')
mypattern = 'Economy_supplementary_tables_(.*).*'
gg = grep(mypattern,s3files$filename) #Find the pattern above in the list of s3 files, this tells me the number of the file in that list.  
econ = s3files$filename[gg] #Create a variable called econ, which is the filename of the file that contains the pattern.  
latest_url$s3 <- ifelse((latest_url$filename == econ),1,0) #Identify which hyperlink / file in the latest_url dataframe corresponds to the file in s3.   
latest_url$latest <- as.Date(paste0(latest_url$Date,"-01"),format ='%B-%Y-%d') #Create new column that formats the month-year column into a date format (e.g. dd/mm/yyyy).
maxi <- max(latest_url$latest[latest_url$exist == TRUE]) #Find the latest date of those URLs that exist. 
latest_url$latest <- ifelse(latest_url$latest == maxi,1,0) #Confirm which is the latest date in the list. 
latest_url$updatereq <- ifelse((latest_url$exist == TRUE & latest_url$s3 == 0 & latest_url$latest == 1),1,0) #If the URL exists, there is no file in s3 and the month is the latest then an udpate is required. 

updateurl <- ifelse(length(latest_url$urls[latest_url$updatereq == 1]) == 0,latest_url$urls[latest_url$s3 == 1],latest_url$urls[latest_url$updatereq == 1])

#Import online Excel OBR datafile into R, through an indirect URL
ourl = updateurl
obr_url = GET(ourl) #update url when new database available
temp_obr_xlsx = basename(obr_url$url)
download.file(obr_url$url,destfile = temp_obr_xlsx,mode = "wb")

#Check which table in the spreadsheet contains the inflation figures. 
obr_contents = read_excel(temp_obr_xlsx,sheet = "Contents")
names(obr_contents) = "contents"
infpat <- 'Table* (.*):.Inflation*'
ginf = grep(infpat,obr_contents$contents)
obrsheet <- obr_contents$contents[ginf]
obrsheet <- gsub(infpat,'\\1',obrsheet) 

#Read the sheet defined by 'obrsheet' in the publication.
obr_xlsx = read_excel(temp_obr_xlsx,sheet = obrsheet) #1.7 is current tab name for Inflation tab

#Clean data to prepare for app
#removes empty first column and removes all extra rows
obr_xlsx <- obr_xlsx[2:18] #removes column 1 (blank)
obr_xlsx <- obr_xlsx %>% slice(4:100) #removes extra rows

#renames all columns
#year on year (% change)
columns <- outer(c("Period","Retail Price Index","Retail Price Index (exc MIP)","Consumer Price Index","Producer Output Prices","Mortgage Interest Payments","Actual Rents for Housing","Consumer Expenditure Deflator","Gross Domestic Product Deflator"),c("_yoy","_index"),FUN = paste0)
dim(columns) <- NULL
columns <- columns[1:17]
names(obr_xlsx) = columns
                 
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
saveWorkbook(obr, temp_obr_xlsx, overwrite = TRUE)

#Save dataframes as both .xlsx and .csv files (only .csv is used in app)
library(s3tools)
write_file_to_s3("obr.xlsx", paste0("alpha-sandbox/",temp_obr_xlsx), overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx, "alpha-sandbox/obr_all.csv", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx_qtr, "alpha-sandbox/obr_qtr.csv", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx_pa, "alpha-sandbox/obr_pa.csv", overwrite = TRUE)
write_df_to_csv_in_s3(obr_xlsx_fy, "alpha-sandbox/obr_fy.csv", overwrite = TRUE)

#deletes obr.xlsx from directory
file.remove("Economy_supplementary_tables_March_2019.xlsx")
