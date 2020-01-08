# Necessary packages
library(tidyverse) # dplyr & readr & stringr (plus other functionalities)
library(openxlsx) # read in/manipulate Excel files
library(s3tools) # importing data from AWS
library(DT) # required to produce output table
library(rhandsontable) # required to add tabs to UI

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE IMPORT/CLEAN-UP | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Download cleaned OBR data .xlsx (mostly not currently in use)
#download_file_from_s3("alpha-app-defcalc/obr.xlsx", "obr.xlsx", overwrite = TRUE)
#index_obr_all <- readWorkbook("obr.xlsx", sheet = "all", colNames = TRUE, rowNames = TRUE)
#index_obr_qtr <- readWorkbook("obr.xlsx", sheet = "qtr", colNames = TRUE, rowNames = TRUE)
#index_obr_pa <- readWorkbook("obr.xlsx", sheet = "pa", colNames = TRUE, rowNames = TRUE)
#index_obr_fy <- readWorkbook("obr.xlsx", sheet = "fy", colNames = TRUE, rowNames = TRUE)
download_file_from_s3("alpha-app-defcalc/obr_raw.xlsx", "obr_raw.xlsx", overwrite = TRUE)
index_obr_raw <- readWorkbook("obr_raw.xlsx", sheet = "Raw", colNames = TRUE)
index_obr_raw <- index_obr_raw %>% replace(., is.na(.), "")
names(index_obr_raw)[2:ncol(index_obr_raw)] <- ""

# Download cleaned OBR data .csv, and re-adds rownames
index_obr_all <- s3_path_to_full_df("alpha-app-defcalc/obr_all.csv")
index_obr_all <- data.frame(index_obr_all, row.names = 1)
index_obr_all$yoy_None <- 0
index_obr_all$index_None <- 100
index_obr_qtr <- s3_path_to_full_df("alpha-app-defcalc/obr_qtr.csv")
index_obr_qtr <- data.frame(index_obr_qtr, row.names = 1)
index_obr_qtr$yoy_None <- 0
index_obr_qtr$index_None <- 100
index_obr_pa <- s3_path_to_full_df("alpha-app-defcalc/obr_pa.csv")
index_obr_pa <- data.frame(index_obr_pa, row.names = 1)
index_obr_pa$yoy_None <- 0
index_obr_pa$index_None <- 100
index_obr_fy <- s3_path_to_full_df("alpha-app-defcalc/obr_fy.csv")
index_obr_fy <- data.frame(index_obr_fy, row.names = 1)
index_obr_fy$yoy_None <- 0
index_obr_fy$index_None <- 100
latest_url_csv <- s3_path_to_full_df("alpha-app-defcalc/latest_url.csv")
latest_url_csv <- data.frame(latest_url_csv, row.names = 1)

# Tidy up the filenames in the latest_url_csv df to remove underscores and file extension.
latest_url_csv$filename <- gsub("_"," ",latest_url_csv$filename)
latest_url_csv$filename <- gsub(".xlsx","",latest_url_csv$filename)

# Create variables for UI for the latest filename used and the website link. 
updatefilename <- ifelse(length(latest_url_csv$filename[latest_url_csv$updatereq == 1]) == 0,
                         latest_url_csv$filename[latest_url_csv$s3 == 1 & latest_url_csv$latest == 1],
                         latest_url_csv$filename[latest_url_csv$updatereq == 1])
updateweblink <- ifelse(length(latest_url_csv$weblink[latest_url_csv$updatereq == 1]) == 0,
                        latest_url_csv$weblink[latest_url_csv$s3 == 1 & latest_url_csv$latest == 1],
                        latest_url_csv$weblink[latest_url_csv$updatereq == 1])

# Selects index drop down options needed for UI (i.e. removes duplicates)
index_options <- index_obr_all %>% select(starts_with("yoy_"))
colnames(index_options) <- substring(colnames(index_options), 5)
# Replace '.' with blank space in column names, for UI aesthetic purposes
colnames(index_options) <- str_replace_all(colnames(index_options), "[.]", " ")

# Alters all column names in dataframes; necessary to match drop down with dataframe YoY column names
colnames(index_obr_all) <- colnames(index_obr_all) %>%
  substring(5) %>%
  str_replace_all("[.]", " ")
colnames(index_obr_qtr) <- colnames(index_obr_qtr) %>%
  substring(5) %>%
  str_replace_all("[.]", " ")
colnames(index_obr_pa) <- colnames(index_obr_pa) %>%
  substring(5) %>%
  str_replace_all("[.]", " ")
colnames(index_obr_fy) <- colnames(index_obr_fy) %>%
  substring(5) %>%
  str_replace_all("[.]", " ")

# Date information, to work out which years are forecasts
latest_url_entry <- latest_url_csv %>% filter(latest==1)
year_known <- as.numeric(str_sub(latest_url_entry$Date, -4, -1))-1
year_forecast <- c(paste0("/",as.numeric(str_sub(year_known,-2,-1))+1), 
                   year_known+1, year_known+2, year_known+3, year_known+4, year_known+5, year_known+6)

# Remove imported excel file
file.remove("obr_raw.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE IMPORT/CLEAN-UP | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~