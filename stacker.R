# EPL_STACKER Ver 0.1.0
# written by Jungwon Choi
# eplchoi@kangwon.ac.kr
# SEP 06 2023

# this is stacking patterns measurement software written with R
# before operation, stalagmite's laminae data must be obtained.
# for more details about input datas, see documentation.

# import library
library(tidyverse)
library(readr)
library(data.table)
library(gridExtra)
library(dplyr)
library(fs)

# reset all data
rm(list = ls())

# data read
print("= = = = = = = = = = STACKER VER 0.1.0 = = = = = = = = = = ")
st_name <- readline("# Type specimen name for identification : ")
datatype <- readline("# Type data extention(.csv or .txt only) : ")
data_path <- readline("# Type file directory : ")

print(paste0("Data type is : ", datatype, " . proceed? (y/n) : "))
# break point threshold(default = 0.3)
threshold <- 0.3
# graph limit
data_limit_X <- 50
data_limit_Y <- 30

# data inspect function
data_processing <- function(datatype, data_path) {
  # load datas from input directory
  # two condition : .csv, .txt
  if (datatype == ".csv") {
    print("DATA TYPE : .CSV")
    file_list <- dir_ls(path = data_path, regexp = "\\.csv$")
    data_list <- list()

    for (data_path in file_list) {
      data <- read.csv(data_path)
      data_list[[data_path]] <- data
    }
  }else if (datatype == ".txt") {
    print("DATA TYPE : .TXT")
    file_list <- dir_ls(path = data_path, regexp = "\\.txt$")
    data_list <- list()
    for (data_path in file_list) {
      data <- read_delim(data_path, delim = "\t")
      data_list[[data_path]] <- data
    }
  }else {
    print("invalid extention. please choose .csv / .txt only.")
  }
  # Data preprocess : data combine and calculate
  combined_data  <-  bind_rows(data_list, .id = "file_id")
  combined_data$file_id <- basename(combined_data$file_id)
  combined_data <- rename(combined_data, "X" = "START X", "Y" = "START Y")
  combined_data <- combined_data %>%
    arrange("file_id") %>%
    group_by("file_id") %>%
    mutate(slope = c(NA, diff(Y) / diff(X)))
  return(combined_data)
}

max_min <- function(datainput) {
  calc_data <- datainput %>%
    group_by(file_id)
    slice(which.max(X), which.min(X))
    mutate(length_between_points = dist(cbind(X, Y)))
}
# execute function
data_list  <-  data_processing(datatype, data_path)
calc_data <- max_min(data_list)
filtered_data <- subset(data_list, slope >= -threshold &
                          slope <= threshold)

