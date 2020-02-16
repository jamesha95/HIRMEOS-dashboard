# This script produces the shiny app that will be used to display HIRMEOS metrics

#---- Set up ------------------------------------------------------------------------------

# This code will install the necessary packages if you're running the script outside of Docker.
# source("DockerConfig/requirements.R")

# Load packages

#library(shiny)
library(shinydashboard)
library(shinyjs) # For the "disable" feature until a password is entered
library(shinyWidgets)
library(tidyverse)
#library(ggfittext) # not needed now, but may be helpful to fit text into the bar chart..?
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
  header = dashboardHeader(title = "Readership metrics"),
  
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
      menuItem("Metrics by title", tabName = "Metrics_by_title", icon = icon("book")),
      menuItem("Global reach", tabName = "Metrics_by_country", icon = icon("globe")),
      menuItem("Admin", tabName = "Admin", icon = icon("key")),
      
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
    useShinyjs(),
    
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
                
                # After some discussion, the "Total Access" metric has been removed and replaced with number of platforms
                
                # A static valueBox for total access metrics
                # valueBox(value = total_access_static, 
                #          subtitle = "Total Access", 
                #          icon = icon("book-reader"),
                #          color = "teal",
                #          width = 4),
                
                # A static valueBox for total number of countries reached
                valueBox(value = no_countries_reached_static, 
                         subtitle = "Countries reached", 
                         icon = icon("flag"),
                         color = "teal",
                         width = 4),
                
                # A static valueBox for total access metrics
                valueBox(value = no_platforms_static, 
                         subtitle = "Total platforms", 
                         icon = icon("window-restore"),
                         color = "light-blue",
                         width = 4)
              ),
              
              
              
              fluidRow( 
                
                
                
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
                  
                  width = 4),
                
                # adjacent to the table is the top 10 countries chart
                
                box(plotOutput("countries_barplot_all", height = "300px"), 
                    width = 8, 
                    title = "Top 10 countries by readership",
                    br(),
                    p("Note: Not all platforms provide country-level data.")
                )
              ), 
              
              # At the bottom, we have the readership by month chart
              
              fluidRow(box(plotOutput("access_over_time_all"), 
                           width = 12, 
                           title = "Quarterly readership"))
      ),
      
      #---- Tab 2: Metrics by title---------------------------------------------------------------------------------- 
      
      tabItem(tabName = "Metrics_by_title",
              fluidRow(
                column(6,
                       # First interactive content: choosing book by title
                       wellPanel(pickerInput(inputId = "title", 
                                             label = "Choose up to 20 titles", 
                                             selected = titles[1],
                                             choices = titles,
                                             options = pickerOptions(actionsBox = FALSE,
                                                                     liveSearch = TRUE,
                                                                     virtualScroll = TRUE,
                                                                     maxOptions = 20,
                                                                     maxOptionsText = "Limit of 20 titles",
                                                                     mobile = FALSE), # setting mobile = TRUE breaks the picker on computers
                                             
                                             multiple = TRUE)
                       ),
                       offset = 0
                )
              ),
              
              fluidRow(
                
                # Dynamic valueBoxes
                valueBoxOutput(outputId = "no_titles_selected", width = 4),
                
                # After some discussion, the "Total Access" metric has been removed and replaced with number of platforms
                #valueBoxOutput(outputId = "total_access", width = 4),
                valueBoxOutput(outputId = "no_countries_reached", width = 4),
                
                valueBoxOutput(outputId = "no_platforms", width = 4)
              ),
              
              fluidRow(
                
                #table of access by different metrics
                
                box(
                  tableOutput("metrics_table"),
                  width = 4,
                  br(),
                  p("Descriptions of each measure can be found ",
                    a(href = "https://metrics.operas-eu.org/measures", "here"),
                    ".")
                ),
                
                # adjacent to the table is the top 10 countries chart 
                box(plotOutput("countries_barplot", height = 300), 
                    title = "Top 10 countries by readership",
                    width = 8
                )
              ),
              
              fluidRow(
                
                box(
                  
                  h4("Click and drag to highlight a section of the horizontal axis. Double-clicking will then zoom to those dates. Double-click anywhere to return to full view."),
                  
                  checkboxGroupInput(inputId = "metric2", 
                                     choices =  measures,
                                     selected = measures, 
                                     label = "Select measure"
                  ),
                  plotOutput("access_over_time",
                             height = "400px",
                             dblclick = "hist1_dblclick",
                             brush = brushOpts(
                               direction = "x",
                               id = "hist1_brush",
                               resetOnNew = TRUE
                             )),
                  width = 12,
                  
                  plotOutput("eventsplot", 
                             # dblclick = "timeline1_dblclick",
                             # brush = brushOpts(
                             #   direction = "x",
                             #   id = "timeline1_brush",
                             #   resetOnNew = TRUE
                             # ),
                             height = "200px"),
                  
                  downloadLink('download_metrics_data', 'Download metrics data'),
                  br(),
                  downloadLink('download_event_data', 'Download event data')
                  
                  
                )
              )
      ),
      
      #---- Tab 3: Global reach------------------------------------------------------------------------------
      
      tabItem(tabName = "Metrics_by_country",
              wellPanel(pickerInput(inputId = "title2", 
                                    label = "Choose up to 20 titles", 
                                    # selected = titles,  # start with all or none selected
                                    # none is the better option, because dots don't load by default until someone interacts with the map
                                    choices = titles,
                                    options = pickerOptions(actionsBox = FALSE,
                                                            maxOptions = 20,
                                                            maxOptionsText = "Limit of 20 titles",
                                                            liveSearch = TRUE,
                                                            virtualScroll = TRUE),
                                    
                                    multiple = TRUE)
              ),
              fluidRow(
                box(
                  
                  checkboxGroupInput(inputId = "metric3", 
                                     choices =  measures,
                                     selected = measures, 
                                     label = "Select measure"
                  ),
                  
                  leafletOutput("map"),
                  width = 12),
                p()
              )
              
              
              
              #global dot map
              #views per month for selected title, split by platform/metric 
              
      ),
      
      #----- Tab 4: Admin ----------------------------------------------------------------------------
      
      tabItem(tabName = "Admin",
              wellPanel("Enter the password to manage the dashboard's data, then press 'Go'",
                        
                        passwordInput(inputId = "admin_password",
                                      label = "",
                                      value = ""),
                        actionButton("go", "Go"),
                        
                        verbatimTextOutput("password_reponse"),
                        
                        br(), 
                        
                        disabled(downloadButton(outputId = "download_all_data",
                                                label =  "Download the current combined dataset"),
                                 downloadButton(outputId = "download_all_metrics_data",
                                                label =  "Download the current metrics data"),
                                 downloadButton(outputId = "download_all_meta_data",
                                                label =  "Download the current metadata"),
                                 downloadButton(outputId = "download_all_altmetrics_data",
                                                label =  "Download the current altmetrics data")
                                 
                                 
                        ))  #,   For now, we disable the ability to upload data.
              
              # wellPanel(h4("Upload new data here"),
              #           
              #           pickerInput(inputId = "file_type",
              #                       label = "Type of file",
              #                       choices = c("Metrics", "Metadata", "Altmetrics"),
              #                       selected = "Metrics",
              #                       multiple = FALSE),
              #           
              #           disabled(fileInput(inputId = "upload",
              #                              label =  "Choose CSV File",
              #                              accept = c(
              #                                "text/csv",
              #                                "text/comma-separated-values",
              #                                "text/plain",
              #                                ".csv"))
              #           ),
              #           
              #           
              #           tableOutput(outputId = "uploaded_file_details"),
              #           
              #           checkboxInput(inputId = "head",
              #                         label =  "Preview the uploaded data?",
              #                         value =  TRUE),
              #           
              #           tableOutput(outputId = "uploaded_file_head"),
              #           
              #           verbatimTextOutput("response_to_new_data"),
              #           
              #           br(),
              #           "Check 2: should the existing data be overwritten, or appended?",
              #           
              #           disabled(radioButtons(inputId = "replace_or_append",
              #                                 label = "Replace or append the data?", 
              #                                 choices = c("Replace", "Append"), 
              #                                 selected = "Append"),
              #                    
              #                    actionButton(inputId = "complete_upload", 
              #                                 label = "Complete")),
              #           
              #           
              #           br(),
              #           
              #           verbatimTextOutput("outcome"),
              #           
              #           br(),
              #           "Check 4: Perhaps something to do with version control of their data..?"
              #          
              #          
              #          
              # ) # end of well panel        
              
      )     # end of the tabbed items
      
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
  
  
  
  #---- Tab 1: static features---------------------------------------------------------------
  #all_data <- reactiveValues() #here, we consider making the underlying data reactive too
  
  output$metrics_table_all <- renderTable({static_metrics_data %>%
      group_by(platform_measure) %>% 
      summarise(Value = prettyNum(sum(value), big.mark = ",")) %>% 
      rename(Measure = platform_measure)},
      digits = 0, 
      align = "cr"
  )
  
  output$countries_barplot_all <- renderPlot(p2)
  
  output$access_over_time_all <- renderPlot(p1)
  
  
  
  
  
  
  
  #---- Tab 2: Metrics by title tab-----------------------------------------------------------------
  
  
  
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
  
  # here we make the filtered data available for download as a .csv
  output$download_metrics_data <- downloadHandler(
    filename = function() {
      paste('metrics-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(title_data(), con)
    }
  )
  
  output$download_event_data <- downloadHandler(
    filename = function() {
      paste('event-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(title_altimetrics(), con)
    }
  )
  
  
  
  
  output$no_titles_selected <- renderValueBox({
    title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
      n_unique("title") %>% #the n_unique function needs the column name entered as a charater string
      valueBox( 
        subtitle = "Titles selected", 
        icon = icon("book"),
        color = "light-blue")
  })
  
  # output$total_access <- renderValueBox({
  #   title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
  #     pull(value) %>% 
  #     sum() %>% 
  #     prettyNum(big.mark = ",") %>%
  #     valueBox( 
  #       subtitle = "Total Access", 
  #       icon = icon("book-reader"),
  #       color = "teal")
  # })
  
  output$no_platforms <- renderValueBox({
    title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
      n_unique("platform") %>% 
      valueBox( 
        subtitle = "Platforms", 
        icon = icon("window-restore"),
        color = "light-blue")
  })
  
  
  
  output$no_countries_reached <- renderValueBox({
    title_data() %>%  # note that title_data() is an expression that retrieves a dataset, hence the () 
      n_unique("country_name") %>% 
      valueBox(
        icon = icon("flag"),
        subtitle = "Countries reached",
        color = "teal")
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
  
  x_range <- reactiveValues(x = NULL) # this reactive value is the range of dates that will be shown in the histogram
  
  output$access_over_time <- renderPlot({
    this_data <- title_data() %>% 
      filter(platform_measure %in% input$metric2)
    p <- ""
    if(dim(this_data)[1] > 0){
      # 
      # times <- title_altimetrics() %>% # I want the x axis to be as wide as the events chart below
      #   mutate(yq = as.yearqtr(timestamp)) %>% 
      #   pull(yq)
      
      p <- histogram_timeline(this_data)}
    
    if(!is.null(x_range$x)){p <- p + xlim(x_range$x)} # this code is to allow the user to zoom in on a time frame
    
    return(p) #consider return(ggplotly(p)) for interactivity, or ggvis
  })
  
  observeEvent(input$hist1_dblclick, {
    brush <- input$hist1_brush
    if (!is.null(brush)){
      selected_data <- brushedPoints(df = {title_data() %>% filter(platform_measure %in% input$metric2)}, 
                                     brush = input$hist1_brush, 
                                     xvar = "date", 
                                     yvar = "value"
      )
      if(dim(selected_data)[1] < 1){x_range$x <- NULL} else{
        x_range$x <- c(min(selected_data$date), max(selected_data$date))}
      # this line prevents the chart from crashing if the brush doesn't actually cover any data
    } else {
      x_range$x <- NULL
    }
  })
  
  
  # Top country bar chart
  output$countries_barplot <- renderPlot({
    chart_data <- top_10_bar_chart_data(title_data()) 
    
    if(dim(chart_data)[1] > 0){
      top_10_countries(chart_data)
    }
  })
  
  # this is a pretty rudimentary attempt at an event timeline
  output$eventsplot <- renderPlot({
    p <- NULL
    if(dim(title_altimetrics())[1] > 0){
      min_metric_date <- title_data() %>% # I want the x axis to be as wide as the chart above
        pull(date) %>%
        min()
      min_altmetric_date <- title_altimetrics() %>%
        pull(date) %>%
        min() 
      
      p <- title_altimetrics() %>% 
        ggplot() +
        geom_segment(
          aes(x = min(min_altmetric_date, min_metric_date) - 10   # I want the x axis to contain all the events
              , xend = Sys.Date() + 10
              , y = 0
              , yend = 0)
          , colour = "black"
          , size = 0.5
        ) +
        geom_linerange(mapping = aes(x = date
                                     , ymin = -1
                                     , ymax = 1
                                     , colour = platform_measure)) +  # change this once we name the measures 
        
        scale_fill_viridis_d(aesthetics = "colour", option = "B", end = 0.9) + #the inferno palette. Ending at 0.9 avoids the lightest yellows
        
        xlab("Event date") +
        ylab("One line per event") +
        theme_minimal() +
        theme(axis.text.y = element_blank()
              , axis.title.y = element_text(size = 14, margin = margin(r = 10, l = 10))
              , panel.grid.major.y = element_blank()
              , panel.grid.minor.y = element_blank()
              , legend.position = "bottom"
              , text = element_text(size = 12)
              , legend.text = element_text(size = 12)
              , legend.title = element_blank()
        ) +
        guides(fill=guide_legend(ncol = 4))
      if(!is.null(x_range$x)){p <- p + xlim(x_range$x)} # this code zooms the range of this plot to match the one above
    }
    return(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##---- Tab 3: Leaflet tab --------------------------------------------------------------------
  
  # Map of the world
  map_data <- reactive({
    all_data %>%
      filter(title_abbr %in% input$title2, # this filters for the chosen title
             platform_measure %in% input$metric3,
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
  
  ##----- Tab 4: The admin tab -------------------------------------------------------------------
  
  # This allows files up to 100MB to be uploaded, instead of maxing out at 5MB.
  options(shiny.maxRequestSize=100*1024^2) 
  
  # password protection
  observeEvent(input$go, {
    req(input$admin_password)
    pw <- isolate(input$admin_password)
    if(pw == my_secrets("admin_password.csv")){
      enable("download_all_data")
      enable("upload")
      enable("download_all_metrics_data")
      enable("download_all_meta_data")
      enable("download_all_altmetrics_data")
    }
  })
  
  output$password_reponse <- renderText({
    req(input$go)
    
    pw <- isolate(req(input$admin_password))
    if(pw == my_secrets("admin_password.csv")){
      return("Correct")
    } else {return("Incorrect password")}
  })
  
  
  
  # here we make all data available for download as a .csv
  output$download_all_data <- downloadHandler(
    filename = function() {
      paste('all-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(all_data, con)
    }
  )
  
  output$download_all_metrics_data <- downloadHandler(
    filename = function() {
      paste('all-metrics-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(metrics_data, con)
    }
  )
  
  output$download_all_meta_data <- downloadHandler(
    filename = function() {
      paste('all-meta-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(meta_data, con)
    }
  )
  
  output$download_all_altmetrics_data <- downloadHandler(
    filename = function() {
      paste('all-altmetrics-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(altmetrics_data, con)
    }
  )
  
  
  # Here we deal with the incoming data from the user
  uploaded_data <- reactive({
    req(input$upload)
    req(input$file_type)
    upload <- input$upload
    read_csv(upload$datapath)
  })
  
  response_to_new_data <- reactive({
    req(input$upload)
    col_names <- uploaded_data() %>% names()
    switch(input$file_type,
           Metrics = ifelse(all(c("measure_uri", "timestamp", "work_uri", "country_uri", "value") %in% col_names), 
                            yes = "Looks okay!", 
                            no = paste("Error! Missing a required column(s). Required columns are:","measure_uri", "timestamp", "work_uri", "country_uri", "value")),
           Metadata = ifelse(all(c("doi", "title", "type") %in% col_names), 
                             yes = "Looks okay!", 
                             no = paste("Error! Missing a required column(s). Required columns are:","doi", "title", "type")),
           Altmetrics = ifelse(all(c("measure_uri", "timestamp", "work_uri", "country_uri", "event_uri", "value") %in% col_names), 
                               yes = "Looks okay!", 
                               no = paste("Error! Missing a required column(s). Required columns are:","measure_uri", "timestamp", "work_uri", "country_uri", "event_uri", "value"))
    ) # end of switch
  })
  
  output$response_to_new_data <- renderText({
    response_to_new_data()
  })
  
  
  # This shows the file name, size and type
  output$uploaded_file_details <- renderTable({
    if (is.null(uploaded_data()))
      return(NULL)
    input$upload %>% select(-datapath)
    #ifelse(input$head, head(uploaded_data()), as.data.frame(input$upload))
  })
  
  # This shows a preview of the uploaded data
  output$uploaded_file_head <- renderTable({
    if (is.null(uploaded_data()))
      return(NULL)
    req(input$head)
    head(uploaded_data())
  })
  
  # Should we replace, or append the existing data?
  
  observeEvent(input$upload, {
    req(input$upload)
    enable("replace_or_append")
    enable("complete_upload")
  })
  
  
  # A message to say it's been successful
  outcome <- reactiveValues(text = NULL)
  
  observeEvent(input$complete_upload, {
    req(input$upload)
    req(input$replace_or_append)
    if(response_to_new_data() == "Looks okay!"){
      disable("complete_upload")
      write_csv(x = uploaded_data(), 
                path = paste0("data/", 
                              str_to_lower(input$file_type),
                              ".csv"), 
                append = (input$replace_or_append == "Append"))
      source("set_up.R") 
      ## Does EVERY dataset need to be reactive?
      ## I think it might... Otherwise the static charts don't know to update
      ## In particular, all_data is non-reactive.
      outcome$text <- "Upload complete!"
      
    }else{outcome$text <- "Upload failed! Is your data in the right format?"}
  })
  
  
  
  output$outcome <- renderText({
    if(is.null(outcome$text)) return()
    outcome$text
  })
  
  
  
  
  # A message about version control
  
  
  
  
}



##---- The final command ----
shinyApp(ui = ui, server = server)
