# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(tidyverse) # data manipulation
library(lubridate) # date manipulation
library(googledrive) # google drive access
library(zoo) # time-series data
library(fs) # file system operations
#library(future.apply) # parallelism

# Data prep
# reading in data directly from Google Drive (with authentication)
malt_data_url <- "https://drive.google.com/file/d/18T4Lp605LCgqxp9bk6lwEJvAgKUZfCbt/view?usp=sharing"
malt_data <- malt_data_url %>% as_id() %>% drive_download(overwrite = TRUE) %>%
  pull(local_path) %>% read_csv() %>%
  select(date = month, count = num_of_forms, app_id, worker_id =user_id) %>%
  nest_by(app_id) %>% ungroup()
one_app <- malt_data %>%
  mutate(workers = map_int(data, ~ length(unique(.x$worker_id)))) %>%
  top_n(50, workers) %>% sample_n(1)

app_id <- one_app %>% pull(app_id)
one_app_data <- one_app %>% pull(data) %>% pluck(1)

worker_analysis(one_app_data, app_id = app_id)

hello <- function() {
  print("Hello, world!")
}
