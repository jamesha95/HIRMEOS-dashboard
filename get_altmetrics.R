# Here, we test accessing the Altmetrics API, and investigate the data it returns

##---- Set up ----

# Add code to check for existing altmetric data. We will eventually need to build a separate dashboard
# for the publisher where they can easily add and retrieve data.

library(tidyverse) # for manipulating data easily
library(httr) # for accessing APIs within R
library(jsonlite) # for translating the JSON results into something readable

# Here's a quick function to access passwords without needing to put them in the script
my_secrets <- function() {
  path <- "/Users/jaha/Desktop/key.csv" #either jaha or jamesha depending on device
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  read_file(path)
}


##---- Getting a token ----

url <- "https://altmetrics.ubiquity.press"

path <- "api/get_token"

raw.token <- GET(url = url, 
                 path = path, 
                 authenticate(user = "jamesha.v2.0@gmail.com", password = my_secrets(), type = "basic"))

JWT <- content(x = raw.token, as = "text")

##---- Calling the API multiple times ----

# Here's some books for which we have readership metrics but want alt-metrics
metrics_data <- read_csv("data/metrics.csv") %>% 
  as_tibble()

dois_of_interest <- metrics_data %>%
  pull(work_uri) %>% 
  unique()

retrieve_altmetrics <- function(uris, key){
  calls_list <- vector("list", length(uris))
  for(i in 1:length(uris)){
    uri <- uris[i]
    raw.result <- GET(url = "https://metrics.ubiquity.press",
                      path = "metrics", 
                      add_headers(Authorization = key),
                      query = list(uri = uri))
    
    if(raw.result$status_code != 200){
      message(paste0(raw.result$status_code," "), appendLF = FALSE)
      warning(paste0(uri, " did not return valid results. Status code: ", raw.result$status_code), 
              call. = FALSE)
      next()
    }
    this.content <- raw.result$content %>% 
      rawToChar() %>% 
      fromJSON()
    
    metrics <- this.content[[3]] %>% 
      as_tibble()
    calls_list[[i]] <- metrics # don't forget the double [[]] when indexing a list, not a vector
    message("ok ", appendLF = FALSE) # this prints dots on the same line
    Sys.sleep(time = 0) # this would be polite to the metrics server, but we want the data asap
  }
  
  combined_data <- bind_rows(calls_list)
  return(combined_data)
}

my_altmetrics <- retrieve_altmetrics(dois_of_interest, JWT)

glimpse(my_altmetrics) # a check

write_csv(my_altmetrics, "data/altmetrics.csv")
