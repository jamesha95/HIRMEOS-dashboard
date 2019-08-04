# This script details the packages that need to be installed to make the shiny app run.

list.of.packages <- c(
  "tidyverse", # tidyverse should already be part of the Docker image
  "shinydashboard", # for styling the shiny app as a dashboard easily
  "shinyWidgets", # extends the number of shiny input options
  "shinyjs", # allows more functions, like disabling an input button until a password has been entered
  "rcrossref", # the crossref API: for easily getting metadata like title, authorship from DOI
  "httr", # for calling the altmetrics API
  "jsonlite", # for handling JSON data
  "rsconnect", # for publishing your shiny app to shinyapps.io
  "RJSONIO", # also for publishing your shiny app to shinyapps.io
  "PKI", # for publishing your shiny app to shinyapps.io
  "scales", # for formatting certain data (such as percentages) nicely
  "zoo", # for formatting dates as year-quarters
  "lubridate", # for working with dates
  "RColorBrewer", # for making nice colour palettes 
  "leaflet") # for the interactive map


# This code checks for these packages and installs them if they need installing

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

