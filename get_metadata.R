#This script allows you to collect metadata on the DOIs for which we have readership metrics stored locally.

library(tidyverse)
library(rcrossref)
library(jsonlite)

# Need to include a check for existing meta_data

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


if (file.exists("data/all_crossref_data")){
  warning("all_crossref_data already exists. Overwriting it.")
}
write_json(external_metadata$data, "data/all_crossref_data") # just saving this for later, since it took so long to call
# we save it as a JSON file because they're flexible and can handle the nested lists in this data

if (file.exists("data/metadata.csv")){
  warning("metadata.csv already exists. Overwriting it.")
}
write_csv(titles, "data/metadata.csv")


