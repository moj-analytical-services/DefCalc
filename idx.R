# Necessary packages
library(tidyverse) # dplyr & readr & stringr (plus other functionalities)
library(openxlsx) # read in/manipulate Excel files
library(s3tools) # importing data from AWS
library(DT) # required to produce output table
library(rhandsontable) # required to add tabs to UI

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE IMPORT/CLEAN-UP | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Links for general documents (will need updating if file name changes unless code is make flexible)
updateappguidance <- download_file_from_s3("alpha-app-defcalc/IndexationToolGuidance_AppVersion_1.0.pptx", "www/Guidance/IndexationToolGuidance.pptx", overwrite = TRUE)

updategeneralguidance <- download_file_from_s3("alpha-app-defcalc/IndexationGuidance_AppVersion_1.0.docx", "www/Guidance/IndexationGuidance.docx", overwrite = TRUE)

updateintranetlink <- "https://intranet.justice.gov.uk/guidance/procurement/analytics/"

# Download cleaned OBR data .xlsx (mostly not currently in use)
#download_file_from_s3("alpha-app-defcalc/obr.xlsx", "obr.xlsx", overwrite = TRUE)
#index_obr_all <- readWorkbook("obr.xlsx", sheet = "all", colNames = TRUE, rowNames = TRUE)
#index_obr_qtr <- readWorkbook("obr.xlsx", sheet = "qtr", colNames = TRUE, rowNames = TRUE)
#index_obr_pa <- readWorkbook("obr.xlsx", sheet = "pa", colNames = TRUE, rowNames = TRUE)
#index_obr_fy <- readWorkbook("obr.xlsx", sheet = "fy", colNames = TRUE, rowNames = TRUE)
download_file_from_s3("alpha-app-defcalc/obr_raw.xlsx", "obr_raw.xlsx", overwrite = TRUE)
index_obr_raw <- readWorkbook("obr_raw.xlsx", sheet = "Inflation", colNames = TRUE)
index_obr_raw <- index_obr_raw %>% replace(., is.na(.), "")
names(index_obr_raw)[2:ncol(index_obr_raw)] <- ""

# Download cleaned OBR data .csv, re-adds rownames, and adds 'do-nothing' index
index_obr_all <- s3_path_to_full_df("alpha-app-defcalc/obr_all.csv")
index_obr_all <- data.frame(index_obr_all, row.names = 1)
index_obr_all <- add_column(index_obr_all, "yoy_None" = 0, .after = ncol(index_obr_all)/2)
index_obr_all <- add_column(index_obr_all, "index_None" = 100, .after = ncol(index_obr_all))

index_obr_qtr <- s3_path_to_full_df("alpha-app-defcalc/obr_qtr.csv")
index_obr_qtr <- data.frame(index_obr_qtr, row.names = 1)
index_obr_qtr <- add_column(index_obr_qtr, "yoy_None" = 0, .after = ncol(index_obr_qtr)/2)
index_obr_qtr <- add_column(index_obr_qtr, "index_None" = 100, .after = ncol(index_obr_qtr))

index_obr_pa <- s3_path_to_full_df("alpha-app-defcalc/obr_pa.csv")
index_obr_pa <- data.frame(index_obr_pa, row.names = 1)
index_obr_pa <- add_column(index_obr_pa, "yoy_None" = 0, .after = ncol(index_obr_pa)/2)
index_obr_pa <- add_column(index_obr_pa, "index_None" = 100, .after = ncol(index_obr_pa))

index_obr_fy <- s3_path_to_full_df("alpha-app-defcalc/obr_fy.csv")
index_obr_fy <- data.frame(index_obr_fy, row.names = 1)
index_obr_fy <- add_column(index_obr_fy, "yoy_None" = as.numeric(0), .after = ncol(index_obr_fy)/2)
index_obr_fy <- add_column(index_obr_fy, "index_None" = as.numeric(100), .after = ncol(index_obr_fy))

latest_url_csv <- s3_path_to_full_df("alpha-app-defcalc/latest_url.csv")
latest_url_csv <- data.frame(latest_url_csv, row.names = 1)

# Tidy up the filenames in the latest_url_csv df to remove underscores and file extension.
latest_url_csv$filename <- gsub("_"," ",latest_url_csv$filename)
latest_url_csv$filename <- gsub(".xlsx","",latest_url_csv$filename)

# Create variables for UI for the latest filename used and the website link. 
updatefilename <- ifelse(length(latest_url_csv$filename[latest_url_csv$updatereq == 1]) == 0,
                         latest_url_csv$filename[latest_url_csv$s3 == 1 & latest_url_csv$latest == 1],
                         latest_url_csv$filename[latest_url_csv$updatereq == 1]
                         )

updateweblink <- ifelse(length(latest_url_csv$weblink[latest_url_csv$updatereq == 1]) == 0,
                        latest_url_csv$weblink[latest_url_csv$s3 == 1 & latest_url_csv$latest == 1],
                        latest_url_csv$weblink[latest_url_csv$updatereq == 1]
                        )


# Selects index drop down options needed for default UI (i.e. removes duplicates)
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

# Creates seperated files for index and year-on-year changes
index_obr_all_yoy <- index_obr_all[, 1:(ncol(index_obr_all)/2)]
index_obr_all_index <- index_obr_all[, ((ncol(index_obr_all)/2)+1):ncol(index_obr_all)]

index_obr_qtr_yoy <- index_obr_qtr[, 1:(ncol(index_obr_qtr)/2)]
index_obr_qtr_index <- index_obr_qtr[, ((ncol(index_obr_qtr)/2)+1):ncol(index_obr_qtr)]

index_obr_pa_yoy <- index_obr_pa[, 1:(ncol(index_obr_pa)/2)]
index_obr_pa_index <- index_obr_pa[, ((ncol(index_obr_pa)/2)+1):ncol(index_obr_pa)]

index_obr_fy_yoy <- index_obr_fy[, 1:(ncol(index_obr_fy)/2)]
index_obr_fy_index <- index_obr_fy[, ((ncol(index_obr_fy)/2)+1):ncol(index_obr_fy)]

# Create list of period references
Period_References <- c("Financial Year", "Calendar Year", "Quarterly")

# Remove imported excel file
file.remove("obr_raw.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE IMPORT/CLEAN-UP | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FORECAST PERIODS | START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Scrapes raw OBR file to find cell containing date information (best available resource)
date_row <- grep(paste(month.name, collapse ="|"), index_obr_raw[, 2])
date_row <- date_row[1]
date_filter <- index_obr_raw[date_row,]
date_cell <- date_filter[, colSums(date_filter != "") != 0]

# Identify the year
date_year <- as.numeric(gsub(".*?([0-9]+).*", "\\1", date_cell))

# Identify which month was most recently included in forecast
for (m in 1:12) {
  date_month <- grepl(month.name[m], date_cell)
  if (date_month == TRUE) {
    date_month <- m
    break
  }
}

# Identify last known full quarter
for (q in 1:4) {
  if (date_month/3 == q) {
    date_qtr <- q
    break
  } else if ((date_month/3 < q) & (q != 1)) {
    date_qtr <- q-1
    break
  } else if ((date_month/3 < q) & (q = 1)) {
    date_qtr <- 4
    break
    }
}

# Generate calendar year forecast periods
known_pa <- date_year -1

fcst_pa <- list()
  for (pa in c(1:6)) {
    fcst_pa[[pa]] <- known_pa + pa
    }

# Generate quarterly forecast periods (a); find last known quarter
known_qtr <- if (date_qtr == 4) {
  paste0(known_pa,
         "Q",
         date_qtr
         )
} else paste0(date_year,
              "Q",
              date_qtr
              )

# Generate quarterly forecast periods (b); finish calendar year for 'a'
fcst_qtr_1 <- list()
  for (qtr_1 in c((as.numeric(str_sub(known_qtr, -1)) + 1):4)) {
    if (str_sub(known_qtr, -1) == 4) {
      break
  } else fcst_qtr_1[[qtr_1]] <- paste0(str_sub(known_qtr,1,-2),
                                       qtr_1
                                       )
}

# Generate quarterly forecast periods (c); find future calendar years
fcst_qtr_2a <- list()
  for (qtr_2a in c(1:6)) {
    fcst_qtr_2a[[qtr_2a]] <- paste0(as.numeric(str_sub(known_qtr, 1, 4)) + qtr_2a)
  }

# Generate quarterly forecast periods (d); convert 'c' into quarters
fcst_qtr_2b <- list()
  for (qtr_2b in c(1:4)) {
    fcst_qtr_2b[[qtr_2b]] <- paste0(fcst_qtr_2a,
                                    "Q",
                                    qtr_2b
                                    )
  }

fcst_qtr <- c(fcst_qtr_1,
              fcst_qtr_2b
              )

# Generate financial year forecasts
known_fy <- paste0(as.numeric(str_sub(known_qtr, 1, 4)) - 1 ,
                   "/",
                   as.numeric(str_sub(known_qtr, 3, 4))
                   )

fcst_fy <- list()
  for (fy in c(1:6)) {
    fcst_fy[[fy]] <- paste0(as.numeric(str_sub(known_fy, 1, 4)) + fy,
                            "/", 
                            as.numeric(str_sub(known_fy, -2, -1)) + fy
                            )
  }

# Combine all forecast periods into one list
year_known <- c(known_pa, known_qtr, known_fy)
year_forecast <- c(fcst_pa, fcst_qtr, fcst_fy)
year_now <- as.integer(format(Sys.Date(), "%Y"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FORECAST PERIODS | END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

