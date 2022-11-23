library(tidyverse)
library(mongolite)

db <- "indication_list"

mongoUrl <- "mongodb://root:sempre813!@192.168.0.91:27017/admin"
ena_list <- read_delim(file = "ena_sra-run_20221019-0625.tsv", delim = "\t", col_names = T) %>%
  # bind_rows(., read_delim(file = "R-Selenium/list/new_normal/normal2.tsv", delim = "\t", col_names = T)) %>%
  # bind_rows(., read_delim(file = "R-Selenium/list/new_normal/normal3.tsv", delim = "\t", col_names = T)) %>%
  # bind_rows(., read_delim(file = "R-Selenium/list/new_normal/normal4.tsv", delim = "\t", col_names = T)) %>%
  # bind_rows(., read_delim(file = "MiaPaca2_5.tsv", delim = "\t", col_names = T)) %>%
  # bind_rows(., read_delim(file = "MiaPaca2_6.tsv", delim = "\t", col_names = T)) %>%
  unique()

collection_name <- "TEMP"
m <- mongo(collection = collection_name, 
           db = db, 
           url = mongoUrl,
           verbose = TRUE, 
           options = ssl_options())
m$insert(ena_list)

