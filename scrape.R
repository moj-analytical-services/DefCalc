# Necessary packages
library(httr)
library(readxl) 
library(openxlsx) # read in/manipulate Excel files
library(tidyverse) # dplyr & readr & stringr (plus other functionalities)
library(s3tools) # importing data from AWS
library(RCurl) # get information relating to a URL
library(xml2) # required for rvest
library(rvest) # get information relating to a URL
library(lubridate) # to create the list of dates to check against for URLs

library(reticulate) # required for auto-email updates

print("Print Statement: Packages loaded")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WEB SCRAPING FOR FILE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a list of dates between now and 12 months prior.
date_end <- floor_date(as.POSIXlt(Sys.Date()), "months")
date_start <- date_end
date_start$mon <- date_end$mon - 12
date_length <- interval(date_start, date_end)/months(1)
date_list <- as.data.frame(date_start + months(0:(date_length)))
latest_url <- format(date_list, "%B-%Y")
names(latest_url) <- "Date"

print(latest_url["Date"])
print("Print Statement: URL dataframe structure created")

# Gives the structure of the URL, which we'll use and replace 'march-2019' with the series of month-years created above.
ourl <- "https://obr.uk/download/march-2019-economic-and-fiscal-outlook-supplementary-economy-tables/"
osearch <- "https://obr.uk/efo/economic-and-fiscal-outlook-march-2019/"
obr <- "https://obr.uk/publications"

print("Print Statement: URL structure provided")

# Create URLs and test if they exist, create column of URLS and replace with the months_years from above. Check if each URL exists. 
latest_url$urls <- ourl
sub = function(x,y,z)gsub(x,y,z)
latest_url$urls <- mapply(sub, "march-2019", latest_url$Date, latest_url$urls)
latest_url$filename <- paste(paste('Economy_supplementary_tables', latest_url$Date, "EFO", sep = "_"),  "xlsx", sep = ".")
latest_url$filename <- gsub("-", "_", latest_url$filename)

latest_url$search <- osearch
latest_url$search <- mapply(sub, "march-2019", latest_url$Date, latest_url$search)

print("Print Statement: URL list created")

# Scrape 'obr' to find list of hrefs to match against 'ourl'
obr_html <- read_html(obr)
obr_hrefs <- html_attr(html_nodes(obr_html, "a"), "href")

print("Print Statement: HTML download links identified")

# Match obr_hrefs against latest_url$urls
for (i in 1:nrow(latest_url)) {
  
  if (any(grep(latest_url[i, "search"], obr_hrefs, ignore.case = TRUE) != 0)) {
    
    latest_url[i, "exist"] <- TRUE
    }
    else { 
    latest_url[i, "exist"] <- FALSE
    }
  
}

print("Print Statement: Latest EFO version identified")

# Test if files are in S3 and if they're not then indicate the URL that needs to be downloaded.
s3files <- s3tools::list_files_in_buckets('alpha-app-defcalc')
mypattern = 'Economy_supplementary_tables_(.*).*'

# Find the pattern above in the list of s3 files, this tells you the number of the file in that list.
gg = grep(mypattern, s3files$filename)  
# Create a variable called econ, which is the filename of the file that contains the pattern.
econ = s3files$filename[gg]  
# Identify which hyperlink / file in the latest_url dataframe corresponds to the file in s3.
latest_url$s3 <- ifelse((latest_url$filename %in% econ), 1, 0)   
# Create new column that formats the month-year column into a date format (e.g. dd/mm/yyyy).
latest_url$latest <- as.Date(paste0(latest_url$Date, "-01"), format ='%B-%Y-%d')
# Find the latest date of those URLs that exist, and confirm which is the latest date in the list
maxi <- max(latest_url$latest[latest_url$exist == TRUE]) 
latest_url$latest <- ifelse(latest_url$latest == maxi, 1, 0) 
# If the URL exists, there is no file in s3 and the month is the latest then an udpate is required.
latest_url$updatereq <- ifelse((latest_url$exist == TRUE & latest_url$s3 == 0 & latest_url$latest == 1), 1, 0)
latest_url$weblink <- paste('https://obr.uk/efo/economic-and-fiscal-outlook', latest_url$Date,sep = "-")
latest_filename <- latest_url$filename[which(latest_url$latest == 1)]

print("Print Statement: Check latest URL against S3 buckets complete")

# Pick out, of the latest_url dataframe, the url that needs to be downloaded and the corresponding filename.
updateurl <- ifelse(length(latest_url$urls[latest_url$updatereq == 1]) == 0, latest_url$urls[latest_url$s3 == 1 & latest_url$latest == 1], latest_url$urls[latest_url$updatereq == 1])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WEB SCRAPING FOR FILE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE IMPORT/IDENTIFY SHEET | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only run rest of code if updatereq has a non-zero value
if (is_empty(which(latest_url$updatereq == 1) == 0) == FALSE) {
  
  print("Print Statement: Update required: code running...")
  
  ### Auto-email code
  #R wrapper for send_contributor_email_py
  send_contributor_email = function(contrib_email, QA_table){
    
    # Read in python email function
    reticulate::source_python("./gov_notify.py")
    
    # Get api key
    api_key_raw = s3tools::read_using(
      read.delim,
      s3_path = 'alpha-app-defcalc/indexationtool_api.txt',
      header = FALSE,
      sep = "",
      quote = ""
    )
    
    api_key = as.character(api_key_raw[[1]])
    
    return(send_contributor_email_py(contrib_email,
                                     "",
                                     api_key))
    
  }
  
  jason_table = jsonlite::toJSON("")
  
  #Loop created to cycle through all emails (not ideal, but could not generate a list in below function)
  email_list <- c("daniel.white@justice.gov.uk",
                  "james.leicester@justice.gov.uk",
                  "michelle.tenwick@justice.gov.uk",
                  "megan.chambers@justice.gov.uk")
  
  # Must be added as members of the team on 'gov.uk/notify'
  for (i in 1:length(email_list)) {
    
  send_contributor_email(email_list[i],
                          jason_table)
  }
  
  
  # Import online Excel OBR datafile into R, through an indirect URL
  ourl = updateurl
  obr_url = GET(ourl)
  temp_obr_xlsx = basename(obr_url$url)
  download.file(obr_url$url, destfile = temp_obr_xlsx, mode = "wb")
  
  # Check which table in the spreadsheet contains the inflation figures. 
  obr_contents = read_excel(temp_obr_xlsx, sheet = "Contents")
  names(obr_contents) = "contents"
  
  # Finds the main inflation figures
  path_inflation <- 'Table* (.*):.Inflation*'
  info_inflation = grep(path_inflation, obr_contents$contents)
  obr_inflation <- obr_contents$contents[info_inflation]
  obr_inflation <- gsub(path_inflation, '\\1', obr_inflation)
  
  # Finds the average weekly earnings figures
  path_awe <- 'Table* (.*):.Labour Market*'
  info_awe <- grep(path_awe, obr_contents$contents)
  obr_awe <- obr_contents$contents[info_awe]
  obr_awe <- gsub(path_awe, '\\1', obr_awe)
  
  # Read the sheet defined by the above in the publication.
  obr_xlsx_inf = read_excel(temp_obr_xlsx,sheet = obr_inflation)
  obr_xlsx_inf_unformatted = read_excel(temp_obr_xlsx, sheet = obr_inflation)
  
  obr_xlsx_awe = read_excel(temp_obr_xlsx,sheet = obr_awe)
  obr_xlsx_awe_unformatted = read_excel(temp_obr_xlsx, sheet = obr_awe)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE IMPORT/IDENTIFY SHEET | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE MANIPULATION | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Clean data to prepare for app
  # Removes empty columns based on if they're entirely 'NA'
  obr_xlsx_inf <- obr_xlsx_inf[, colSums(is.na(obr_xlsx_inf)) != nrow(obr_xlsx_inf)]
  obr_xlsx_awe <- obr_xlsx_awe[, colSums(is.na(obr_xlsx_awe)) != nrow(obr_xlsx_awe)]
  obr_xlsx_inf <- obr_xlsx_inf[-c(1,2),] # deletes Row 1 & 2
  obr_xlsx_awe <- obr_xlsx_awe[-1,] # deletes Row 1
  
  # Convert first row into column names. Explictly assumes Column 1 is 'Period'.
  colnames(obr_xlsx_inf)<- obr_xlsx_inf[1,] # copy Row 1 to Column names
  names(obr_xlsx_inf)[1] <- "Period" # creates column name for 'Period'
  
  colnames(obr_xlsx_awe)<- obr_xlsx_awe[1,] # copy Row 1 to Column names
  names(obr_xlsx_awe)[1] <- "Period" # creates column name for 'Period'
  
  # Removes extra rows if they contain any 'NA'; Row 2 exempt
  obr_xlsx_inf <- obr_xlsx_inf[complete.cases(obr_xlsx_inf[ , 1:ncol(obr_xlsx_inf)]), ]
  obr_xlsx_awe <- obr_xlsx_awe[complete.cases(obr_xlsx_awe[ , 1:ncol(obr_xlsx_awe)]), ]
  
  # Removes extra columns in AWE dataset that aren't related to the index of interest
  awe_columns <- grep("average earnings", colnames(obr_xlsx_awe), ignore.case = TRUE)
  obr_xlsx_awe <- obr_xlsx_awe[awe_columns]
  
  awe_column_yoy <- grep("growth", colnames(obr_xlsx_awe), ignore.case = TRUE)
  awe_column_index <- grep("index", colnames(obr_xlsx_awe), ignore.case = TRUE)
  
  # Renames new column names based on if they are YoY changes, or Indices
  colnames(obr_xlsx_inf) <- make.unique(colnames(obr_xlsx_inf))
  obr_xlsx_inf_yoy <- obr_xlsx_inf %>% select(-ends_with(".1"))
  obr_xlsx_inf_index <- obr_xlsx_inf %>% select(ends_with(".1"))
  colnames(obr_xlsx_inf_index) <- paste("index", colnames(obr_xlsx_inf_yoy[2:ncol(obr_xlsx_inf_yoy)]), sep = "_")
  colnames(obr_xlsx_inf_yoy) <- paste("yoy", colnames(obr_xlsx_inf_yoy), sep = "_")
  
  names(obr_xlsx_awe)[awe_column_yoy] <- "yoy_Average weekly earnings"
  names(obr_xlsx_awe)[awe_column_index] <- "index_Average weekly earnings"
  
  obr_xlsx_awe_yoy <- obr_xlsx_awe[awe_column_yoy]
  obr_xlsx_awe_index <- obr_xlsx_awe[awe_column_index]
  
  # Combines Inflation & AWE datasets
  obr_xlsx <- cbind.data.frame(obr_xlsx_inf_yoy, obr_xlsx_awe_yoy, obr_xlsx_inf_index, obr_xlsx_awe_index)
  names(obr_xlsx)[names(obr_xlsx)=="yoy_Period"] <- "Period" #time period reference
  obr_xlsx$Period <- str_replace_all(obr_xlsx$Period, "-", "/")
  
  # Creates dataframes for data by quarter, annual and financial year splits flexibly (i.e. auto-updates with new spreadsheet)
  obr_xlsx_qtr <- filter(obr_xlsx, grepl("Q", obr_xlsx$Period)) #quarterly
  obr_xlsx_pa <- filter(obr_xlsx, nchar(obr_xlsx$Period, type = "chars") == 4, TRUE) #calendar
  obr_xlsx_fy <- filter(obr_xlsx, grepl("/", obr_xlsx$Period))  #financial
  
  # Adds row names to all dataframes (doing this at the start doesn't copy them to new dataframes)
  obr_xlsx <- data.frame(obr_xlsx, row.names = 1)
  obr_xlsx_qtr <- data.frame(obr_xlsx_qtr, row.names = 1)
  obr_xlsx_pa <- data.frame(obr_xlsx_pa, row.names = 1)
  obr_xlsx_fy <- data.frame(obr_xlsx_fy, row.names = 1)
  
  # Creates one workbook as .xlsx
  obr <- createWorkbook()
  addWorksheet(obr, sheet = "all")
  addWorksheet(obr, sheet = "qtr")
  addWorksheet(obr, sheet = "pa")
  addWorksheet(obr, sheet = "fy")
  writeData(obr, sheet = "all", obr_xlsx, colNames = TRUE, rowNames = TRUE)
  writeData(obr, sheet = "qtr", obr_xlsx_qtr, colNames = TRUE, rowNames = TRUE)
  writeData(obr, sheet = "pa", obr_xlsx_pa, colNames = TRUE, rowNames = TRUE)
  writeData(obr, sheet = "fy", obr_xlsx_fy, colNames = TRUE, rowNames = TRUE)
  
  # Creates raw data workbook as .xlsx
  obr_unformatted <- createWorkbook()
  addWorksheet(obr_unformatted, sheet = "Inflation")
  addWorksheet(obr_unformatted, sheet = "Labour Market")
  writeData(obr_unformatted, sheet = "Inflation", obr_xlsx_inf_unformatted)
  writeData(obr_unformatted, sheet = "Labour Market", obr_xlsx_awe_unformatted)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE MANIPULATION | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE SAVE | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Save dataframes as both .xlsx and .csv files (only .csv is used in app)
  # saveWorkbook(obr, temp_obr_xlsx, overwrite = TRUE)
  # write_file_to_s3("obr.xlsx", paste0("alpha-app-defcalc/", temp_obr_xlsx), overwrite = TRUE)
  
  # Save dataframes as both .xlsx and .csv files (only .csv is used in app)
  saveWorkbook(obr, "obr.xlsx", overwrite = TRUE)
  write_file_to_s3("obr.xlsx", "alpha-app-defcalc/obr.xlsx", overwrite = TRUE)
  saveWorkbook(obr_unformatted, "obr_unformatted.xlsx", overwrite = TRUE)
  write_file_to_s3("obr_unformatted.xlsx", "alpha-app-defcalc/obr_raw.xlsx", overwrite = TRUE)
  
  write.csv(obr_xlsx, "obr_all.csv", row.names = TRUE)
  write_file_to_s3("obr_all.csv", "alpha-app-defcalc/obr_all.csv", overwrite = TRUE)
  write.csv(obr_xlsx_fy, "obr_fy.csv", row.names = TRUE)
  write_file_to_s3("obr_fy.csv", "alpha-app-defcalc/obr_fy.csv", overwrite = TRUE)
  write.csv(obr_xlsx_qtr, "obr_qtr.csv", row.names = TRUE)
  write_file_to_s3("obr_qtr.csv", "alpha-app-defcalc/obr_qtr.csv", overwrite = TRUE)
  write.csv(obr_xlsx_pa, "obr_pa.csv", row.names = TRUE)
  write_file_to_s3("obr_pa.csv", "alpha-app-defcalc/obr_pa.csv", overwrite = TRUE)
  
  # Create .csv with just the filename of the latest download in it. 
  write.csv(obr_xlsx, latest_filename)
  write_file_to_s3(latest_filename, paste0("alpha-app-defcalc/", latest_filename), overwrite = TRUE)
  
  # Create .csv with just fhe filename of the latest url in it
  write.csv(latest_url, "latest_url.csv")
  write_file_to_s3("latest_url.csv", "alpha-app-defcalc/latest_url.csv", overwrite = TRUE)
  
  # deletes obr.xlsx from directory
  file.remove(temp_obr_xlsx)
  file.remove("obr.xlsx")
  file.remove("obr_unformatted.xlsx")
  file.remove("obr_all.csv")
  file.remove("obr_fy.csv")
  file.remove("obr_qtr.csv")
  file.remove("obr_pa.csv")
  file.remove("latest_url.csv")
  
  print("Print Statement: Update completed")
  
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE SAVE | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
