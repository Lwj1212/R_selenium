library(RSelenium)
library(tidyverse)
library(mongolite)

# function
collection_to_DF <- function(collection_name, url) {
  m <- mongo(collection = collection_name, 
             db = "indication", 
             url = url,
             verbose = TRUE, 
             options = ssl_options())
  m$find() %>% as_tibble() %>% return()
}
run_parse <- function(remDr, ena_url, id, collection_name, start, end){
  ena_parse <- function(remDr, ena_url, id, sleep_cnt = 3){
    
    remDr$navigate(paste0(ena_url, id))
    Sys.sleep(sleep_cnt)
    
    tryCatch(
      expr = {
        title <- remDr$findElement(using = "xpath", '//*[@id="view-content-col"]/div[2]')
        title <- title$getElementText() %>% unlist()
      },
      error = function(e){
        title <<- " "
      }
    )
    

    
    tryCatch(
      expr = {
        study_accession <- remDr$findElement(using = "xpath", '//*[@id="view-content-col"]/div[4]/div/div[2]/app-read-file-links/div/div[3]/table/tbody/tr/td[1]/div/span/a')
        study_accession <- study_accession$getElementText() %>% unlist()
      },
      error = function(e){
        tibble(title = title,
               study_accession = " ", 
               organism = " ", 
               sample_accession = " ",
               instrument_model = " ", 
               read_count = " ", 
               base_count = " ", 
               library_layout = " ",
               library_strategy = " ", 
               library_source = " ") %>% 
          return()
      }
    )
    

    
    
    tryCatch(
      expr = {
        organism <- remDr$findElement(using = "xpath", "//div[contains(text(),'Organism')]/../div[2]")
        organism <- organism$getElementText() %>% unlist()
      },
      error = function(e){
        organism <<- " "
      })
    
    tryCatch(
      expr = {
        sample_accession <- remDr$findElement(using = "xpath", "//div[contains(text(),'Sample Accession')]/../div[2]")
        sample_accession <- sample_accession$getElementText() %>% unlist()
      },
      error = function(e){
        sample_accession <<- " "
      })
    
    tryCatch(
      expr = {
        instrument_model <- remDr$findElement(using = "xpath", "//div[contains(text(),'Instrument Model')]/../div[2]")
        instrument_model <- instrument_model$getElementText() %>% unlist()
      },
      error = function(e){
        instrument_model <<- " "
      })
    
    tryCatch(
      expr = {
        read_count <- remDr$findElement(using = "xpath", "//div[contains(text(),'Read')]/../div[2]")
        read_count <- read_count$getElementText() %>% unlist()
      },
      error = function(e){
        read_count <<- " "
      })
    
    tryCatch(
      expr = {
        base_count <- remDr$findElement(using = "xpath", "//div[contains(text(),'Base')]/../div[2]")
        base_count <- base_count$getElementText() %>% unlist()
      },
      error = function(e){
        base_count <<- " "
      })
    
    tryCatch(
      expr = {
        library_layout <- remDr$findElement(using = "xpath", "//div[contains(text(),'Library Layout')]/../div[2]")
        library_layout <- library_layout$getElementText() %>% unlist()
      },
      error = function(e){
        library_layout <<- " "
      })
    
    tryCatch(
      expr = {
        library_strategy <- remDr$findElement(using = "xpath", "//div[contains(text(),'Library Strategy')]/../div[2]")
        library_strategy <- library_strategy$getElementText() %>% unlist()
      },
      error = function(e){
        library_strategy <<- " "
      })

    tryCatch(
      expr = {
        library_source <- remDr$findElement(using = "xpath", "//div[contains(text(),'Library Source')]/../div[2]")
        library_source <- library_source$getElementText() %>% unlist()
      },
      error = function(e){
        library_source <<- " "
      })
    
    tibble(title = title,
           study_accession = study_accession, 
           organism = organism, 
           sample_accession = sample_accession,
           instrument_model = instrument_model, 
           read_count = read_count, 
           base_count = base_count, 
           library_layout = library_layout,
           library_strategy = library_strategy, 
           library_source = library_source) %>% 
      return()
  }
  remDr$open() 
  remDr$navigate("https://www.ebi.ac.uk/ena/browser/home")
  Sys.sleep(10)
  cnt <- start
  while(cnt <= end){
    print(paste0(run_id[cnt], " #", cnt))
    re <- FALSE
    tryCatch(
      expr = {
        m <- mongo(collection = collection_name, 
                     db = "indication", 
                     url = mongoUrl,
                     verbose = TRUE, 
                     options = ssl_options())
        df <- bind_cols(tibble(index = cnt, run_accession = run_id[cnt]), 
                       ena_parse(remDr = remDr, 
                                 ena_url = ena_url, id = run_id[cnt]))
  
        m$insert(df)
        
      },
      error = function(e) {
        re <<- TRUE
      }
    )
    
    if(re){
      print(paste0(run_id[cnt], " re-tried"))
      try(remDr$close())
      remDr$open()
      remDr$navigate(paste0(ena_url, run_id[cnt]))
      Sys.sleep(10)
      
      next
    } else {
      cnt <- cnt + 1
    }
  }
}

# Selenium server
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4444)  

ena_url <- "https://www.ebi.ac.uk/ena/browser/view/"
mongoUrl <- "mongodb://root:sempre813!@192.168.0.91:27017/admin"

# run_id <- read_delim(file = "colon1.tsv", delim = "\t", col_names = T) %>% pull(1)
run_id <- 'SRR14580535'

# run selenium
run_parse(remDr = remDr, ena_url = ena_url, id = run_id, collection_name = "test", start = 1, end = length(run_id))
