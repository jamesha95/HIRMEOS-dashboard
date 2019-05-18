#This script allows you to collect metadata on the DOIs for which we have readership metrics stored locally.

library(tidyverse)
library(rcrossref)

metrics_data <- read_csv("data/metrics.csv") %>% 
  as_tibble()

# Call CrossRef and metrics API as required - this is SLOW.
dois_of_interest <- metrics_data %>% 
  # extract(work_uri, into = c("uri_type", "uri"), REGEX EXPR
  mutate(uri = substr(work_uri, start = 10, stop = nchar(work_uri))) %>%
  pull() %>% 
  unique()

external_metadata <- cr_works(dois = dois_of_interest, 
                              .progress = "text") 

titles <- external_metadata$data %>% 
  as_tibble() %>% 
  select(doi, title, type, publisher) # Can add other variables of interest, such as author

write_csv(titles, "data/metadata.csv")