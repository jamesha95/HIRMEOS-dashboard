# This script does everything that's needed to import/clean the data. 

#We disable R's feature that automatically reads in strings as factors. In this work, strings are generally just strings

options(stringsAsFactors = FALSE)

# and let's define some important colours
hirmeos_orange <- "#F79C49"
hirmeos_blue <- "#0C5EA3"
operas_purple <- "#682661"
eu_blue <- '#094E97'
sidebar_blue <- "#3A80A7"
header_blue <- "#408EBA"

# Import local data and tidy------------------------------------------------------------------------------

if (!file.exists("data/metrics.csv")){
  warning("metrics.csv does not exist. Using demo data instead.")
  metrics_path <- "data/demo_data/demo_metrics_small.csv"
} else{metrics_path <- "data/metrics.csv"}

if (!file.exists("data/metadata.csv")){
  warning("metadata.csv does not exist. Using demo data instead.")
  metadata_path <- "data/demo_data/demo_metadata_small.csv"
} else{metadata_path <- "data/metadata.csv"}

if (!file.exists("data/altmetrics.csv")){
  warning("altmetrics.csv does not exist. Using demo data instead.")
  altmetrics_path <- "data/demo_data/demo_altmetrics.csv"
} else{altmetrics_path <- "data/altmetrics.csv"}

metrics_data <- read_csv(metrics_path) %>% 
  as_tibble()

meta_data <- read_csv(metadata_path) %>% 
  as_tibble() %>%
  mutate(work_uri = paste0("info:doi:", doi))

altmetrics_data <- read_csv(altmetrics_path) %>% 
  as_tibble()



# OPERASurl <- "https://metrics.operas-eu.org/measures"
#  measures <- OPERASurl %>%
#    read_html() %>%
#  html_nodes(xpath = '//*[@id="root"]/div/section/section/main/div/div[2]/div/div/div/div/div/div[2]/div/div/div/div/div/table') %>%
# html_table()
# The OPERAS website has the table formatted using ant-table rather than table, which is hard to extract... 

country_geodata <- read_csv("data/country_centroids.csv") %>%
  select(country_name = admin,
         country_code =iso_a2, 
         longitude = Longitude, 
         latitude = Latitude) %>% # source: https://worldmap.harvard.edu/data/geonode:country_centroids_az8
  mutate(longitude = case_when(country_name == "France" ~ 2.61, # there are a few countries which we need to manually edit
                               country_name == "Kiribati" ~ 173.00,
                               country_name == "Fiji" ~ 178.2,
                               TRUE ~ longitude), 
         latitude = case_when(country_name == "France" ~ 46.46, 
                              country_name == "Kiribati" ~ 1.44,
                              country_name == "Fiji" ~ -17.76,
                              TRUE ~ latitude))




all_data <- metrics_data %>%
  left_join(meta_data, by = c("work_uri" = "work_uri")) %>% 
  # left_join(altmetrics_data, by = c("work_uri" = "URI")) %>%  # We should keep altmetrics separate
  
  # for now, we will focus on book & monograph data. If we allow chapters,
  # we'll need to generalise the search/filters in the UI to select for books/monograph/chapters
  filter(type %in% c("monograph", "book")) %>%  
  
  select(work_uri,
         measure_id, # this tells us what sort of metric we're looking at
         value, # the value of the metric   
         timestamp, # this tells us what date
         country_uri, # this tells us what country
         title, # this is the title
         publisher) %>%  # together, these are the data we'll analyse for now
  # later we will include events (altmetrics)
  mutate(country_code = ifelse(is.na(country_uri), 
                               "missing",
                               substr(country_uri, start = 21, stop = 22))) %>%
  left_join(country_geodata, by = "country_code") %>%
  mutate(country_name = ifelse(is.na(country_name), 
                               "No info", 
                               country_name)) %>% 
  separate(col = measure_id, into = c("junk", "junk2", "junk3","platform", "measure", "version"), sep = "/") %>%
  select(-c(junk, junk2, junk3)) %>%  
  mutate(platform = str_replace(platform, "-", " ")) %>%
  mutate(platform_measure = paste0(platform, ": ", measure)) %>% 
  
  
  # We add year-quarters for the readership dates
  mutate(yq = as.yearqtr(timestamp)) %>%  
  
  # Some titles are outrageously verbose; we tidy those here
  mutate(title_abbr = ifelse(nchar(title) > 100,
                             paste0(substr(title, start = 1, stop = 97), "..."),
                             title)) 



event_data <- wrangle_event_data(altmetrics_data)


# Pre-processed data (non-interactive)-------------------------------------------------------------------------------
# Have some data pre-processed and ready to go here, so that the entire datafile doesn't need uploading
# each time someone uses the app. 

# Number of titles and names of titles in the dataset
titles <- all_data %>%
  filter(!is.na(title_abbr)) %>%
  pull(title_abbr) %>%
  unique() %>% 
  sort()

no_titles <- prettyNum(length(titles), big.mark = ",")

# Number of countries and readership by country for the dataset, and a barplot for top 10 countries

countries <- all_data %>% 
  filter(!is.na(value)) %>% 
  select(country_name, value) %>%
  group_by(country_name) %>% 
  summarise(country_access = sum(value))

no_countries_reached_static <- countries %>%
  pull(country_name) %>% 
  unique() %>% 
  length() %>% 
  prettyNum(big.mark = ",")

countries_top_10 <- countries %>%
  arrange(desc(country_access)) %>%
  top_n(10, wt = country_access)

countries_top_10 <- countries_top_10 %>%
  mutate(country_name = factor(x = pull(countries_top_10, country_name), 
                               levels = pull(countries_top_10, country_name),
                               ordered = T),
         country_percent = percent(country_access/sum(country_access), 1))



p2 <- top_10_countries(countries_top_10)


no_platforms_static <- all_data %>%
  filter(!is.na(platform_measure)) %>%
  pull(platform) %>%
  unique() %>%
  length() %>%
  prettyNum(big.mark = ",")

# Grouping metrics by measure and date, and creating a column plot over time
# This metric has been removed and replaced with number of platforms.
# total_access_static <- static_metrics_data %>% 
#   pull(value) %>% 
#   sum() %>% 
#   prettyNum(big.mark = ",")

static_metrics_data <- all_data %>%
  filter(!is.na(platform_measure)) %>%
  select(platform_measure, value, yq) %>%
  group_by(platform_measure, yq) %>% 
  summarise(value = sum(value))


measures <- static_metrics_data %>%
  pull(platform_measure) %>% 
  unique()



p1 <- quarterly_plot(static_metrics_data)