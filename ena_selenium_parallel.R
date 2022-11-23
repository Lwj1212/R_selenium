library(RSelenium)
library(tidyverse)
library(parallel)
library(mongolite)

# selenium
# xvfb-run java -Dwebdriver.chrome.driver=/usr/bin/chromedriver -jar selenium-server-standalone-3.141.59.jar 

# function
min_max_chunk <- function(ena_list, C){
  chunk <- function(x, n) split(x, sort(rank(x) %% n))
  
  ena_list <- 1:ena_list
  chunk(ena_list, C) %>% lapply(X = ., function(value){
    return(c(min(value), max(value)))
  }) %>% unname() %>% return()
}
collection_list <- function(db, url){
  m <- mongo(db = db, 
             url = mongoUrl,
             verbose = TRUE, 
             options = ssl_options())
  
  mongo_list <- m$run('{"listCollections":1}')
  mongo_list <- mongo_list$cursor$firstBatch$name
  
  return(mongo_list)
}
# collection_list(db = "cellline_list", url = mongoUrl)
collection_to_DF <- function(db, collection_name, url) {
  m <- mongo(collection = collection_name, 
             db = db, 
             url = url,
             verbose = TRUE, 
             options = ssl_options())
  m$find() %>% as_tibble() %>% return()
}
collection_to_DF_context <- function(db, collection_name, url) {
  m <- mongo(collection = collection_name, 
             db = db, 
             url = url,
             verbose = TRUE, 
             options = ssl_options())
  m$find() %>% as_tibble() %>% return()
}
run_id_setdiff <- function(run_id, db, collection_name, url){
  DF <- collection_to_DF_context(db = db, collection_name = collection_name, url = mongoUrl)  
  if(nrow(DF) == 0){
    setdiff(run_id, NULL) %>% 
      sort() %>% 
      return()
  } else {
    DF %>% 
      pull(run_accession) %>% 
      setdiff(run_id, .) %>% 
      sort() %>% 
      return()
  }
  
    
}
run_parse <- function(remDr, ena_url, id, db, collection_name, start, end){
  ena_parse <- function(remDr, ena_url, id, sleep_cnt = 8){
    
    remDr$navigate(paste0(ena_url, id))
    Sys.sleep(sleep_cnt)
    
    tryCatch(
      expr = {
        title_ <- remDr$findElement(using = "xpath", '//*[@id="view-content-col"]/div[2]')
        title_ <- title_$getElementText() %>% unlist()
      },
      error = function(e){
        title_ <<- " "
      }
    )
    
    # error check
    tryCatch(
      expr = {
        study_accession <- remDr$findElement(using = "xpath", '//*[@id="view-content-col"]/div[4]/div/div[2]/app-read-file-links/div/div[3]/table/tbody/tr/td[1]/div/span/a')
        study_accession <- study_accession$getElementText() %>% unlist()
      },
      error = function(e){
        study_accession <<- " "
      })
    
    # find element
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
    

    tryCatch(
      expr = {
        # to Study description
        remDr$navigate(paste0(ena_url, str_trim(study_accession), "?show=xrefs"))
        Sys.sleep(5)
        study_description <- remDr$findElement(using = "xpath", "//div[contains(@class, 'record-description')]")
        study_description <- study_description$getElementText() %>% unlist()
      },
      error = function(e){
        study_description <<- " "
      })
    
    # to Study Center Name
    tryCatch(
      expr = {
        study_center_name <- remDr$findElement(using = "xpath", "//div[contains(text(),'Center Name')]/../div[2]")
        study_center_name <- study_center_name$getElementText() %>% unlist()
      },
      error = function(e){
        study_center_name <<- " "
      })
    
    # to Study Public
    tryCatch(
      expr = {
        Pubmed <- remDr$findElement(using = "xpath", "//li[contains(text(),'PubMed')]/div[1]")
        Pubmed <- Pubmed$getElementText() %>% unlist() %>% str_remove_all(pattern = "\\(Click here to see all ENA records for this PubMed ID\\)") %>% 
          strsplit(split = ",") %>% unlist() %>% 
          lapply(X = ., FUN = function(value){
            str_extract_all(value, pattern = "[:alnum:]") %>% 
              unlist() %>% 
              paste0(collapse = "") %>% return()}) %>% 
          paste(collapse = ",") 
      },
      error = function(e){
        Pubmed <<- " "
      })
    
    tryCatch(
      expr = {
        EuropePMC <- remDr$findElement(using = "xpath", "//li[contains(text(),'EuropePMC')]/div[1]")
        EuropePMC <- EuropePMC$getElementText() %>% unlist() %>% str_remove_all(pattern = "\\(Click here to see all ENA records for this EuropePMC ID\\)") %>% 
          strsplit(split = ",") %>% unlist() %>% 
          lapply(X = ., FUN = function(value){
            str_extract_all(value, pattern = "[:alnum:]") %>% 
              unlist() %>% 
              paste0(collapse = "") %>% return()}) %>% 
          paste(collapse = ",") 
      },
      error = function(e){
        EuropePMC <<- " "
      })
    
    # return DF
    tibble(title = title_,
           study_accession = study_accession, 
           study_description = study_description,
           study_center_name = study_center_name,
           Pubmed = Pubmed,
           EuropePMC = EuropePMC, 
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
                   db = db, 
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
  remDr$close()
}

# variable
ena_url <- "https://www.ebi.ac.uk/ena/browser/view/"
mongoUrl <- "mongodb://root:sempre813!@192.168.0.91:27017/admin"
db_list <- "indication_list"
db_save <- "indication_center_name_paper"
# db_list <- "cellline_list"
# db_save <- "cellline"

# setdiff 
col_list <- collection_list(db = db_list, url = mongoUrl)
exist_list <- collection_list(db = db_save, url = mongoUrl)
col_list <- setdiff(col_list, exist_list) %>% sort()

for(collection_name in col_list){
  # variable
  cores <- 13
  cl <- makeCluster(cores)
  
  run_id <- collection_to_DF(db = db_list, collection_name = collection_name, url = mongoUrl) %>%
    pull(1) %>% 
    run_id_setdiff(run_id = ., db = db_save, collection_name = collection_name, url = mongoUrl)
  
  if(length(run_id) == 0){
    next
  }
  
  # STAR_END
  start_end_list <- min_max_chunk(ena_list = length(run_id), cores)
  
  # Cluster define
  clusterExport(cl, varlist=c("collection_name", "start_end_list", "run_parse", "ena_url", "mongoUrl", "run_id",
                              "db_list", "db_save"), envir=environment())
  clusterEvalQ(cl, {
    library(RSelenium)
    library(tidyverse)
    library(parallel)
    library(mongolite)
  })
  
  # run selenium
  print(collection_name)
  parLapply(cl = cl,
            X = start_end_list,
            fun = function(se_list) {
              print(se_list)
              remDr_ <- remoteDriver(remoteServerAddr = "localhost",
                                     port = 4444)
              run_parse(remDr = remDr_, ena_url = ena_url, 
                        id = run_id, db = db_save, collection_name = collection_name,
                        start =  se_list[1], end = se_list[2])
            })
  stopCluster(cl)
  print("Done!!")
}
