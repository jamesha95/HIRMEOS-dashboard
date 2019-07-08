# This script includes the helper functions that we use in the shiny app, mostly for making similar charts

quarterly_plot <- function(data, event_data = NULL){
  p1 <- ggplot(data, mapping = aes(x = yq, y = value))
  p1 <- p1 + geom_col(aes(fill = platform_measure))
  if(!is.null(event_data)){
    last_q <- max(event_data)
    first_q <- min(event_data)
    p1 <- p1 + scale_x_continuous(expand = c(min(first_q, data$yq), Sys.yearqtr()))
  } # this is to ensure that the x axis contains the same dates as the event plot beneath
  p1 <- p1 + theme_minimal()
  #p1 <- p1 + scale_color_brewer(palette = "RdYlBu", aesthetics = "fill")
  p1 <- p1 + scale_fill_viridis_d(aesthetics = "fill")
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