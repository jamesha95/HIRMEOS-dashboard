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

source("helper.R")
source("set_up.R")




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
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------















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
