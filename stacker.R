# EPL_STACKER Ver 0.1.1
# written by Jungwon Choi
# eplchoi@kangwon.ac.kr
# OCT 03 2023

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
  # 1. collect data wit file_id
  combined_data  <-  bind_rows(data_list, .id = "file_id")
  # 2. fetch data name (not full path)
  combined_data$file_id <- basename(combined_data$file_id)
  # 3. changing column name in START X and START Y into X Y
  combined_data <- rename(combined_data, "X" = "START.X", "Y" = "START.Y")
  combined_data <- combined_data %>%
    arrange(file_id) %>%
    group_by(file_id) %>%
    mutate(slope = c(NA, diff(Y) / diff(X)))
  return(combined_data)
}
# Calculate max-min value
max_min <- function(datainput) {
  threshold_data <- datainput %>%
    group_by(file_id) %>%
    slice(which.max(X), which.min(X)) %>%
    mutate(length_between_points = dist(cbind(X, Y))) %>%
    return (threshold_data)
}
# data process for Growth Line
modify_to_odd <- function(x) {
  if (length(x) %% 2 == 0) {
    # For even-length groups, modify the data into odd number
    x <- x[-length(x) %/% 2]
  }
  return(x)
}

axis_calc <- function(dat) {
  axis_dat <- dat %>%
    group_by(file_id) %>%
    summarize(median = median(modify_to_odd(X))) %>%
    left_join(dat, axis_dat, by = "file_id") %>%
    filter(near(X, median))
  return(axis_dat)
}
# execute function
data_list  <-  data_processing(datatype, data_path)

# create "flat" boundary
filtered_data <- subset(data_list, slope >= -threshold &
                          slope <= threshold)
# extract max point and min point
break_point <- max_min(filtered_data)
# calculate length between two points
length_val <- break_point %>%
  distinct(length_between_points, .keep_all = TRUE)
# calculate axis
axis_val <- axis_calc(filtered_data)

# PLOT DATA
print("data calculate complete. plot data...")
# main plot
data_limit_X <- max(length_val$X)
data_limit_Y <- max(length_val$Y) + 0.2

stal <- ggplot(data_list, aes(x = X, y = Y, )) +
  geom_path(aes(color = file_id)) +
  # add axial plot
  geom_path(data = axis_val,
            mapping =
              aes(x = X, y = Y),
            color = "blue") +
  # top line plotting
  geom_point(data = filtered_data, # apex line
             aes(x = X, y = Y),
             size = 0.5,
             shape = 2,
             color = "red") +
  # Apex Point plot
  geom_point(data = break_point, # BP
             aes(x = X, y = Y),
             color = "black",
             size = 1) +
  labs(title = paste0(st_name, " STACKER REPORT"),
       subtitle = threshold,
       x = "X-axis Label",
       y = "Y-axis Label") +
  theme_minimal() +
  ylim(0, data_limit_Y) +
  coord_fixed(ratio = 1)

print("data plot ready.")
print(stal)
# save your files 
d_select <- readline("save calculated data into .csv? (y/n) : ")
if (d_select == "y") {
  write.csv(length_val, paste0(st_name, ".csv"), row.names = FALSE)
} else if (p_select == "n") {
  break
}

p_select <- readline("would you want to save graph into .PDF? (y/n)")
if (p_select == "y") {
  ggsave(paste0(st_name, ".pdf"), plot = stal)
} else if (p_select == "n") {
  break
}
