#Necessary packages
library(tidyverse) #dplyr & readr & stringr (plus other functionalities)
#library(openxlsx) #read in/manipulate Excel files
library(DT) #output table; better functionality than shiny's tables
library(s3tools) #importing data from AWS

#Download cleaned OBR data .xlsx (not currently in use)
#download_file_from_s3("alpha-sandbox/obr.xlsx", "obr.xlsx", overwrite = TRUE)
#index_obr_all <- readWorkbook("obr.xlsx", sheet = "all", colNames = TRUE, rowNames = TRUE)
#index_obr_qtr <- readWorkbook("obr.xlsx", sheet = "qtr", colNames = TRUE, rowNames = TRUE)
#index_obr_pa <- readWorkbook("obr.xlsx", sheet = "pa", colNames = TRUE, rowNames = TRUE)
#index_obr_fy <- readWorkbook("obr.xlsx", sheet = "fy", colNames = TRUE, rowNames = TRUE)

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
colnames(index_options) <- substring(colnames(index_options),5)
#replace '.' with " " and ".." with '(' or ')'
str_replace(colnames(index_options, "\\.", " ")
            str_replace(colnames(index_options, "\\.", " ")

#alters all column names in dataframes; necessary to match drop down with dataframe YoY column names
colnames(index_obr_all) <- substring(colnames(index_obr_all),5)
colnames(index_obr_qtr) <- substring(colnames(index_obr_qtr),5)
colnames(index_obr_pa) <- substring(colnames(index_obr_pa),5)
colnames(index_obr_fy) <- substring(colnames(index_obr_fy),5)
