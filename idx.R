#Necessary packages
library(tidyverse) #dplyr & readr & stringr (plus other functionalities)
library(openxlsx) #read in/manipulate Excel files
library(s3tools) #importing data from AWS
library(DT) #required to produce output table
library(rhandsontable) #required to add tabs to UI

#Download cleaned OBR data .xlsx (mostly not currently in use)
#download_file_from_s3("alpha-sandbox/obr.xlsx", "obr.xlsx", overwrite = TRUE)
#index_obr_all <- readWorkbook("obr.xlsx", sheet = "all", colNames = TRUE, rowNames = TRUE)
#index_obr_qtr <- readWorkbook("obr.xlsx", sheet = "qtr", colNames = TRUE, rowNames = TRUE)
#index_obr_pa <- readWorkbook("obr.xlsx", sheet = "pa", colNames = TRUE, rowNames = TRUE)
#index_obr_fy <- readWorkbook("obr.xlsx", sheet = "fy", colNames = TRUE, rowNames = TRUE)
download_file_from_s3("alpha-sandbox/obr_raw.xlsx", "obr_raw.xlsx", overwrite = TRUE)
index_obr_raw <- readWorkbook("obr_raw.xlsx", sheet = "Raw", colNames = TRUE)
index_obr_raw <- index_obr_raw %>% replace(., is.na(.), "")
names(index_obr_raw)[2:ncol(index_obr_raw)] <- ""

#Download cleaned OBR data .csv, and re-adds rownames
index_obr_all <- s3_path_to_full_df("alpha-sandbox/obr_all.csv")
index_obr_all <- data.frame(index_obr_all, row.names = 1)
index_obr_qtr <- s3_path_to_full_df("alpha-sandbox/obr_qtr.csv")
index_obr_qtr <- data.frame(index_obr_qtr, row.names = 1)
index_obr_pa <- s3_path_to_full_df("alpha-sandbox/obr_pa.csv")
index_obr_pa <- data.frame(index_obr_pa, row.names = 1)
index_obr_fy <- s3_path_to_full_df("alpha-sandbox/obr_fy.csv")
index_obr_fy <- data.frame(index_obr_fy, row.names = 1)

#selects index drop down options needed for UI (i.e. removes duplicates)
index_options <- index_obr_all %>% select(starts_with("yoy_"))
colnames(index_options) <- substring(colnames(index_options), 5)
#replace '.' with blank space in column names, for UI aesthetic purposes
colnames(index_options) <- str_replace_all(colnames(index_options), "[.]", " ")

#alters all column names in dataframes; necessary to match drop down with dataframe YoY column names
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

#date information, to work out which years are forecasts
curr_year <- as.numeric(
                substr(Sys.Date(), 1, 4)
                        )
fcst_years <- as.character(
                c(curr_year, curr_year+1, curr_year+2, curr_year+3, curr_year+4, curr_year+5)
                          )
#remove imported excel file
file.remove("obr_raw.xlsx")
