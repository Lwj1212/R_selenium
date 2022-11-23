library(mongolite)
library(tidyverse)

collection_to_DF_context <- function(db, collection_name, url) {
  m <- mongo(collection = collection_name, 
             db = db, 
             url = url,
             verbose = TRUE, 
             options = ssl_options())
  m$find() %>% as_tibble() %>% return()
}

collection_to_DF_context(db = "indication_center_name_paper", collection_name = "NEW_NORMAL_0701", url = mongoUrl) %>%
  write_delim(file = "NEW_NORMAL_0701_RAW.csv", delim = ",")