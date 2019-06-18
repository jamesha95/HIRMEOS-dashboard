# This script produces the shiny app that will be used to display HIRMEOS metrics

#---- Set up ------------------------------------------------------------------------------

# This code will install the necessary packages if you're running the script outside of Docker.
# source("DockerConfig/requirements.R")

# Load packages

#library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(rcrossref)
#library(plotly)
#library(shinyWidgets)
#library(countrycode)
library(scales)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(leaflet)
library(rsconnect)
#library(rvest)


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
 # left_join(altmetrics_data, by = c("work_uri" = "URI")) %>% We should keep altmetrics separate
  
  filter(type %in% c("monograph", "book")) %>%  # for now, we will focus on book & monograph data. If we allow chapters,
  # we'll need to generalise the search/filters in the UI to select for books/monograph/chapters
  
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
  
  # allocating readership to quarters
  
  mutate(yq = as.yearqtr(timestamp)) %>%  # We add year-quarters for the readership dates
  mutate(title_abbr = ifelse(nchar(title) > 100,
                             paste0(substr(title, start = 1, stop = 97), "..."),
                             title)) # Some titles are outrageously verbose; we tidy those here


  
wrangle_event_data <- function(data){
  data %>% 
  mutate(work_uri = paste0("info:doi:", work_uri)) %>%
  left_join(meta_data, by = c("work_uri" = "work_uri")) %>%
  separate(col = measure_id, into = c("junk", "junk2", "junk3","platform", "measure", "version"), sep = "/") %>%
  select(-c(junk, junk2, junk3)) %>% 
    mutate(platform = str_replace(platform, "-", " ")) %>%
  mutate(platform_measure = paste0(platform, ": ", measure)) %>% 
  mutate(title_abbr = ifelse(nchar(title) > 100,
                             paste0(substr(title, start = 1, stop = 97), "..."),
                             title))
}

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

top_10_countries <- function(data){
  p2 <- ggplot(data, mapping = aes(x = country_name, y = country_access))
  p2 <- p2 + geom_bar(stat = "identity", 
                      fill = hirmeos_blue)
  p2 <- p2 + geom_text(aes(label = paste0(prettyNum(country_access, big.mark = ","),
                                          "  (",
                                          country_percent,
                                          ")")),
                       hjust = -0.1)
  p2 <- p2 + coord_flip()
  p2 <- p2 + theme_void()
  p2 <- p2 + theme(axis.text.y = element_text(), 
                   title = element_blank())
  p2 <- p2 + scale_x_discrete(limits = rev(levels(pull(data,country_name))))
  p2 <- p2 + scale_y_continuous(limits = c(0, 1.5*max(pull(data, country_access))))
  return(p2)
}

p2 <- top_10_countries(countries_top_10)


# Grouping metrics by measure and date, and creating a column plot over time

static_metrics_data <- all_data %>%
  filter(!is.na(platform_measure)) %>%
  select(platform_measure, value, yq) %>%
  group_by(platform_measure, yq) %>% 
  summarise(value = sum(value))

total_access_static <- static_metrics_data %>% 
  pull(value) %>% 
  sum() %>% 
  prettyNum(big.mark = ",")

measures <- static_metrics_data %>%
  pull(platform_measure) %>% 
  unique()

quarterly_plot <- function(data, event_data = NULL){
  p1 <- ggplot(data, mapping = aes(x = yq, y = value))
  p1 <- p1 + geom_col(aes(fill = platform_measure))
  if(!is.null(event_data)){
    last_q <- max(event_data)
    first_q <- min(event_data)
  p1 <- p1 + scale_x_continuous(expand = c(min(first_q, data$yq), Sys.yearqtr()))
  } # this is to ensure that the x axis contains the same dates as the event plot beneath
  p1 <- p1 + theme_minimal()
  p1 <- p1 + scale_color_brewer(palette = "RdYlBu", aesthetics = "fill")
  p1 <- p1 + theme(legend.position = "top",
                   axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
                   axis.text.y = element_text(size = 12))
  # p1 <- p1 + scale_fill_discrete(palette = brewer.pal(9, "YlGnBu"))
  # p1 <- p1 + scale_fill_manual(values = c("downloads" = brewer.pal(8, "YlGnBu")[5],
  #                                         "sessions" = brewer.pal(8, "YlGnBu")[6],
  #                                         "users" = brewer.pal(8, "YlGnBu")[7], 
  #                                         "views" = brewer.pal(8, "YlGnBu")[8]))
  p1 <- p1 + scale_y_continuous(labels = comma)
  p1 <- p1 + ylab("")
  p1 <- p1 + xlab("")
  p1 <- p1 + scale_x_yearqtr(format = "%Y-Q%q")
  #p1 <- p1 + theme(legend.title = element_blank())
  
  return(p1)
}

p1 <- quarterly_plot(static_metrics_data)


#---- The User Interface: Header and sidebar----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(title = "Readership metrics"
  ),
  
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
      menuItem("Metrics by title", tabName = "Metrics_by_title", icon = icon("book")),
      menuItem("Global reach", tabName = "Metrics_by_country", icon = icon("globe")),
      
      # We add a horizontal line, followed by the HIRMEOS logo, OPERAS logo and EU logo
      hr(),
      
      box(
        a(href = 'https://www.hirmeos.eu/',
          img(src = 'HIRMEOS_LOGO_rect.png', 
              width = "100%")
        ), 
        width = 12, 
        solidHeader = TRUE
      ),
      

      
      box(
        a(href = 'https://operas.hypotheses.org/',
          img(src = 'https://operas.hypotheses.org/files/2017/04/Logo-OPERAS.png', 
              width = "100%")
        ), 
        width = 12, 
        solidHeader = TRUE
        
        
      ),
      
      
      
      box(
        img(src = 'https://hirmeos.eu/wp-content/uploads/2017/03/logo-ce-horizontal-en-pantone-lr.png', 
            width = "100%"),
        width = 12, 
        solidHeader = TRUE
        
      )
    )
  ),
  
  #---- The User Interface: Body----------------------------------------------------------------------------------                    
  
  body = dashboardBody(
    
    #Set a theme if desired - either with the HTML below or using the shinythemes package
    
    # tags$head(
    #   tags$style(HTML("
    #                   .content-wrapper {
    #                   background-color: lightgrey !important;
    #                   }
    #                   .main-sidebar {
    #                   background-color: white !important;
    #                   }
    #                   "))
    #   ),
    
    tabItems(
      
      #---- Tab 1: Summary----------------------------------------------------------------------------------                              
      
      tabItem(tabName = "Summary",
              
              
              
              fluidRow(
                # A static valueBox for number of titles in database
                valueBox(value = no_titles, 
                         subtitle = "Titles published", 
                         icon = icon("book"),
                         color = "light-blue", 
                         width = 4),
                
                # A static valueBox for total access metrics
                valueBox(value = total_access_static, 
                         subtitle = "Total Access", 
                         icon = icon("book-reader"),
                         color = "teal",
                         width = 4),
                
                # A static valueBox for total number of countries reached
                valueBox(value = no_countries_reached_static, 
                         subtitle = "Countries reached", 
                         icon = icon("flag"),
                         color = "light-blue",
                         width = 4)
              ),
              
              
              fluidRow( 
                
                # adjacent to the table is the top 10 countries chart
                
                box(plotOutput("countries_barplot_all", height = "300px"), 
                    width = 8, 
                    title = "Top 10 countries by readership",
                    br(),
                    p("Note: Not all platforms provide country-level data.")
                ),
                
                #table of access by different metrics
                
                box(
                  tableOutput("metrics_table_all"),
                  br(),
                  p("Descriptions of each measure can be found ",
                    a(href = "https://metrics.operas-eu.org/measures", "here"),
                    "."),
                  
                  # Here we include the possibility of filtering by measure, but that would require reactivity
                  
                  # checkboxGroupInput(inputId = "metric1",choices = measures,
                  #                    selected = measures, label = "Select measure"
                  #                     ),
                  
                  width = 4)
              ), 
              
              # At the bottom, we have the readership by month chart
              
              fluidRow(box(plotOutput("monthly_access_all"), 
                           width = 12, 
                           title = "Quarterly readership"))
      ),
      
      #---- Tab 2: Metrics by title---------------------------------------------------------------------------------- 
      
      tabItem(tabName = "Metrics_by_title",
              fluidRow(
                column(6,
                       # First interactive content: choosing book by title
                       wellPanel(pickerInput(inputId = "title", 
                                             label = "Choose a title or select all", 
                                             selected = titles[1],
                                             choices = titles,
                                             options = pickerOptions(actionsBox = TRUE,
                                                                     liveSearch = TRUE,
                                                                     virtualScroll = TRUE,
                                                                     mobile = FALSE), # setting mobile = TRUE breaks the picker on computers
                                             
                                             multiple = TRUE)
                       ),
                       offset = 0
                )
              ),
              
              fluidRow(
                
                # Dynamic valueBoxes
                valueBoxOutput(outputId = "no_titles_selected", width = 4),
                
                valueBoxOutput(outputId = "total_access", width = 4),
                
                valueBoxOutput(outputId = "no_countries_reached", width = 4)
              ),
              
              fluidRow(
                
                # adjacent to the table is the top 10 countries chart 
                box(plotOutput("countries_barplot", height = 300), 
                    title = "Top 10 countries by readership",
                    width = 8
                ),
                
                #table of access by different metrics
                
                box(
                  tableOutput("metrics_table"),
                  width = 4,
                  br(),
                  p("Descriptions of each measure can be found ",
                    a(href = "https://metrics.operas-eu.org/measures", "here"),
                    ".")
                )
              ),
              
              fluidRow(
                
                box(
                  
                  checkboxGroupInput(inputId = "metric2",choices =  measures,
                                     selected = measures, label = "Select measure"
                  ),
                  plotOutput("monthly_access"),
                  width = 12,
                  plotOutput("eventsplot", height = "200px")
                  
                )
              )
      ),
      
      #---- Tab 3: Global reach------------------------------------------------------------------------------
      
      tabItem(tabName = "Metrics_by_country",
              wellPanel(pickerInput(inputId = "title2", 
                                    label = "Choose a title or select all", 
                                    # selected = titles,  # start with all or none selected
# none is the better option, because dots don't load by default until someone interacts with the map
                                    choices = titles,
                                    options = pickerOptions(actionsBox = TRUE,
                                                            liveSearch = TRUE,
                                                            virtualScroll = TRUE),
                                    
                                    multiple = TRUE)
              ),
              fluidRow(
                box(leafletOutput("map"),
                    width = 12),
                p()
              )
              
              
              
              #global dot map
              #views per month for selected title, split by platform/metric 
              
      )
      # end of the tabbed items
      
    ) # end of tabItems()
  ) # end of dashboardBody
) # end of UI


##---- The Server -------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #---- A feature to allow specific urls to take you straight to the relevant book---------------------
  
  # This should work if after the dashboard url, you query with the following structure:
  # .../?doi=foo
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$doi)) {
      doi <-query$doi
      updateTabItems(session, 'tabs', "Metrics_by_title")
      updatePickerInput(session, 
                        'title', 
                        selected = {
                          linked_title <- all_data %>% 
                            filter(work_uri == paste0("info:doi:", doi)) %>% 
                            pull(title_abbr) %>%
                            unique()
                          if(is.null(linked_title)){
                            titles[1]
                            }else{linked_title}
                        })
    }
  })
  
  
  
  #---- Summary tab: static features---------------------------------------------------------------
  output$metrics_table_all <- renderTable({static_metrics_data %>% 
      group_by(platform_measure) %>% 
      summarise(Value = prettyNum(sum(value), big.mark = ",")) %>% 
      rename(Measure = platform_measure)},
      digits = 0, 
      align = "cr"
  )
  
  output$countries_barplot_all <- renderPlot(p2)
  
  output$monthly_access_all <- renderPlot(p1)
  
  
  
  
  
  
  
  #---- Metrics by title tab-----------------------------------------------------------------
  
  # instead of filtering our dataset many times (costly), we create a reactive expression
  # that builds a new dataset with each update of input$title
  title_data <- reactive({
    all_data %>%
      filter(title_abbr %in% input$title, # this filters for the chosen title
             !is.na(value)) # this removes any measure with a missing value
  }) # note that title_data() is an expression that retrieves a dataset, so must be called like a function 
  
  title_altimetrics <- reactive({
    event_data %>%  
      filter(title_abbr %in% input$title) # this filters for the chosen title
            
  }) 
  
  
  
  
  output$no_titles_selected <- renderValueBox({
    title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
      pull(title) %>% 
      unique() %>%
      length() %>%
      prettyNum(big.mark = ",") %>%
      valueBox( 
        subtitle = "Titles selected", 
        icon = icon("book"),
        color = "light-blue")
  })
  
  output$total_access <- renderValueBox({
    title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
      pull(value) %>% 
      sum() %>% 
      prettyNum(big.mark = ",") %>%
      valueBox( 
        subtitle = "Total Access", 
        icon = icon("book-reader"),
        color = "teal")
  })
  
  
  
  output$no_countries_reached <- renderValueBox({
    title_data() %>%  # note that title_data() is an expression that retrieves a dataset, hence the () 
      pull(country_name) %>% 
      unique() %>% 
      length() %>% 
      valueBox(
        icon = icon("flag"),
        subtitle = "Countries reached",
        color = "light-blue")
  })
  
  output$metrics_table <- renderTable({
    title_data() %>%   
      group_by(platform_measure)%>%
      summarise(value = prettyNum(sum(value),
                                  big.mark = ",")) %>%
      rename(metric = platform_measure)
  }, 
  align = "cr", digits = 0
  )
  
  output$monthly_access <- renderPlot({
    this_data <- title_data() %>% 
      filter(platform_measure %in% input$metric2) %>% 
      group_by(platform_measure, yq)%>%
      summarise(value = sum(value)) %>%
      arrange(yq, platform_measure) 
    p <- ""
    if(dim(this_data)[1] > 0){
      
      times <- title_altimetrics() %>% # I want the x axis to be as wide as the events chart below
        mutate(yq = as.yearqtr(timestamp)) %>% 
        pull(yq)
      
      p <- quarterly_plot(this_data, times)}
    
    return(p) #consider return(ggplotly(p)) for interactivity, or ggvis
  })
  
  # Top country bar chart
  output$countries_barplot <- renderPlot({
    chart_data <- title_data() %>% 
      select(country_name, value) %>%
      group_by(country_name) %>% 
      summarise(country_access = sum(value)) %>% 
      arrange(desc(country_access)) %>%
      top_n(10, wt = country_access)
    
    chart_data <- chart_data %>%
      mutate(country_name = factor(x = pull(chart_data, country_name), 
                                   levels = pull(chart_data, country_name),
                                   ordered = T),
             country_percent = percent(country_access/sum(country_access),accuracy = 1))
    
    if(dim(chart_data)[1] > 0){
      top_10_countries(chart_data)
    }
  })
  
  # this is a pretty rudimentary attempt at an event timeline
  output$eventsplot <- renderPlot({
    if(dim(title_altimetrics())[1] >0){
      times <- title_data() %>% # I want the x axis to be as wide as the chart above
        pull(timestamp)
      
    title_altimetrics() %>% 
      ggplot() +
      geom_segment(
        aes(x = min(c(min(times), min(timestamp))) - 10   # I want the x axis to contain all the events
            , xend = as.POSIXct(Sys.Date()) + 20
            , y = 0
            , yend = 0)
            , colour = "black"
            , size = 0.5
      ) +
      geom_linerange(mapping = aes(x = timestamp
                                   , ymin = -1
                                   , ymax = 1
                                   , colour = platform_measure)) +  # change this once we name the measures 
 #))+
     # we will need to pick a wider palette eventually, as YlGnBu can only handle up to 9 groups
       scale_colour_brewer(palette = "Dark2", aesthetics = "colour") +
      
      xlab("Event date") +
      theme_minimal() +
      theme(axis.text.y = element_blank()
            , axis.title.y = element_blank()
            , panel.grid.major.y = element_blank()
            , panel.grid.minor.y = element_blank()
            , legend.position = "bottom"
            
      ) }
    
  })
  
  
  
  
  ##---- Leaflet tab --------------------------------------------------------------------
  
  # Map of the world
  map_data <- reactive({
    all_data %>%
      filter(title_abbr %in% input$title2, # this filters for the chosen title
             !is.na(value),
             !is.na(longitude))%>%
      group_by(country_name) %>%
      summarise(total_access = sum(value),
                longitude = max(longitude),
                latitude = max(latitude)) # all longitude/latitude values should be the same for a given country
  }) 
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1, 
                                     worldCopyJump = TRUE # This allows continuous panning left-right
    )
    ) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(nowrap = FALSE)
      ) %>% 
      fitBounds(-180, -70, 180, 75)
    
  })
  
  observe({
    map_data <- map_data()
    
    leafletProxy("map", data = map_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~log(total_access) + 3, # The +3 helps to make small circles clickable
                       weight = 1, 
                       color = hirmeos_blue,
                       fillColor = hirmeos_blue, 
                       fillOpacity = 0.7, 
                       popup = ~paste0(country_name, ": ", prettyNum(total_access, big.mark = ","))
      )
  })
  
  # Line chart by platform
  
}



##---- The final command ----
shinyApp(ui = ui, server = server)
