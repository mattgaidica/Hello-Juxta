library(shiny)
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(rclipboard)

# Set option to display warnings in the browser console
options(shiny.sanitize.errors = FALSE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Change the font of the input textarea */
      .form-control {
        font-family: Courier, sans-serif;
        font-size: 0.85REM;
        resize: none;
      }
      textarea.form-control {
        height: 400px;
      }
    "))
  ),
  titlePanel("Hello, Juxta"),
  
  tabsetPanel(
    tabPanel("Log Data",
        br(),
       sidebarLayout(
         sidebarPanel(
           textAreaInput("log_input_text", "Enter log data:", value = paste(readLines("log_sample.txt"), collapse = "\n")),
           actionButton("clear_log", "Clear"),
           width = 5
         ),
         mainPanel(
           helpText("Social Network", align = "center"),
           plotOutput("social_plot"),
           helpText("RSSI vs. MAC", align = "center"),
           plotOutput("rssi_plot"),
           width = 7
         )
       )
    ),
    tabPanel("Meta Data",
      br(),
      sidebarLayout(
        sidebarPanel(
          textAreaInput("meta_input_text", "Enter meta data:", value = paste(readLines("meta_sample.txt"), collapse = "\n")),
          actionButton("clear_meta", "Clear"),
          width = 5
        ),
        mainPanel(
          helpText("Temperature (°C)", align = "center"),
          plotOutput("deg_c_plot", height = "200px"),
          br(),
          helpText("Battery (V)", align = "center"),
          plotOutput("vbatt_plot", height = "200px"),
          br(),
          helpText("Movement over Time (binned)", align = "center"),
          plotOutput("xl_time", height = "200px"),
          br(),
          helpText("Movement by Hour of Day", align = "center"),
          plotOutput("xl_hist", height = "200px"),
          width = 7
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$clear_log, {
    updateTextAreaInput(session, "log_input_text", value = "")
  })
  
  observeEvent(input$clear_meta, {
    updateTextAreaInput(session, "meta_input_text", value = "")
  })
  
  # Function to create graph from input text
  createGraph <- function(text) {
    # Split text by rows and columns
    rows <- strsplit(text, "\n")[[1]]
    cols <- strsplit(rows, ",")
    # Remove header row
    cols <- cols[-1]
    # Select first two columns
    cols <- lapply(cols, function(x) x[1:2])
    # Check that cols has the same length
    if (length(cols[[1]]) != length(cols[[2]])) {
      stop("Invalid input: column lengths do not match")
    }
    # Convert MAC addresses to vertices
    vertices <- unique(unlist(cols))
    # Create graph and add edges
    edges <- data.frame(from = cols[[1]], to = cols[[2]])
    edges$weight <- 1
    for (i in 2:length(cols)) {
      from <- cols[[i]][1]
      to <- cols[[i]][2]
      # Find the corresponding edge and increment the weight
      idx <- which(edges$from == from & edges$to == to)
      if (length(idx) == 0) {
        edges <- rbind(edges, data.frame(from = from, to = to, weight = 1))
      } else {
        edges$weight[idx] <- edges$weight[idx] + 1
      }
    }
    graph <- graph_from_data_frame(edges, directed = FALSE, vertices = vertices)
    graph <- simplify(graph, edge.attr.comb = "sum")
    return(graph)
  }
  
  observeEvent(input$log_input_text, {
    # Render graph plot
    output$social_plot <- renderPlot({
      # Get input text and create graph
      log_input_text <- input$log_input_text
      graph <- createGraph(log_input_text)
      # make max weight = * pts
      max_weight <- max(E(graph)$weight)
      E(graph)$scaled_weight <- E(graph)$weight / max_weight * 15
      E(graph)$width <- E(graph)$scaled_weight
      # Plot graph
      plot(graph, layout = layout_with_fr, vertex.label.cex = 1.25, vertex.label.color = "black",
           vertex.color = "lightblue", edge.color = "gray", vertex.size = 20, asp = 1,
           vertex.label.family="Courier", edge.label = E(graph)$weight)
    })
    
    output$rssi_plot <- renderPlot({
      data <- read.table(text = input$log_input_text, sep = ",", header = TRUE)
      data <- data[,-c(1,4)]
      data$rssi <- as.numeric(data$rssi)
      # agg_df <- aggregate(data[, 3], list(data[, 2]), FUN = function(x) c(mean = mean(x), std = sd(x)))
      gather_df <- gather(data, key = "Stat", value = "rssi", -their_mac)
      agg_df <- group_by(gather_df, their_mac, Stat) %>%
        summarize(mean = mean(rssi), sd = sd(rssi))
      
      ggplot(agg_df, aes(x = their_mac, y = mean, fill = Stat)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.9), width = 0.2) +
        theme(panel.background = element_rect(fill = "white"))
    })
  })
  
  # META
  # output$myText <- renderText({
  #   "This is my text."
  # })
  
  observeEvent(input$meta_input_text, {
    req(input$meta_input_text)
    input_text <- input$meta_input_text
    input_text <- gsub("\r", "", input_text)
    input_text <- strsplit(input_text, "\n")
    input_text <- unlist(input_text)
    input_text <- input_text[-1] # remove the header row
    input_text <- strsplit(input_text, ",")
    input_df <- data.frame(do.call(rbind, input_text))
    colnames(input_df) <- c("subject", "data_type", "data_value", "local_time")
    # input_df$local_time <- as.POSIXct(input_df$local_time, format = "%s", origin = "1970-01-01", tzone = "ET")
    input_df$local_time <- with_tz(as.POSIXct(input_df$local_time, format = "%s", origin = "1970-01-01"), tzone = "GMT") %>%
      with_tz(tzone = "America/New_York")
    
    output$deg_c_plot <- renderPlot({
      # Subset the input data to only include "deg_c" data type
      subset_data <- input_df %>%
        filter(data_type == "deg_c")
      
      # Create the plot for deg_c
      ggplot(subset_data, aes(x = local_time, y = as.numeric(data_value))) +
        geom_line(color = "blue") +
        ylab("deg_c") +
        scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
        # scale_y_continuous(limits = c(15, 35), expand = expand_scale(mult = c(0, 0.1))) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(color = "blue"),
              axis.text.y = element_text(color = "blue")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$vbatt_plot <- renderPlot({
      # Subset the input data to only include "vbatt" data type
      subset_data <- input_df %>%
        filter(data_type == "vbatt")
      
      # Create the plot for vbatt
      ggplot(subset_data, aes(x = local_time, y = as.numeric(data_value))) +
        geom_line(color = "red") +
        ylab("vbatt") +
        scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
        scale_y_continuous(limits = c(2, 5), expand = expand_scale(mult = c(0, 0.1))) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(color = "red"),
              axis.text.y = element_text(color = "red")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    })
    
    output$xl_time <- renderPlot({
      # Filter input_df to only include "xl" events and their local_time values
      # subset_data <- subset(input_df, data_type == "xl", select = c("local_time"))
      subset_data <- input_df %>%
        filter(data_type == "xl")
      
      # Calculate start and end times for plot
      start_time <- min(input_df$local_time)
      end_time <- max(input_df$local_time)
      
      # Create sequence of hourly time intervals
      time_seq <- seq(start_time, end_time, by = "hour")
      if (length(time_seq) == 1) {
        message("Not enough data to create plot.")
        return()
      }
      
      # Bin time values into hourly intervals and count "xl" events
      xl_events_per_hour <- table(cut(subset_data$local_time, time_seq))
      
      # Convert results to data frame
      xl_events_df <- data.frame(hour = as.POSIXct(names(xl_events_per_hour)), 
                                 count = as.numeric(xl_events_per_hour))
      
      # Create bar plot using ggplot2
      ggplot(xl_events_df, aes(x = hour, y = count)) +
        geom_bar(stat = "identity") +
        scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
        labs(x = "Time", y = "XL per hour") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$xl_hist <- renderPlot({
      # Subset the input data to only include "xl" data type
      subset_data <- input_df %>%
        filter(data_type == "xl")
      
      # Extract hour of day from local_time
      subset_data$hour_of_day <- hour(subset_data$local_time)
      
      # Create a histogram of the number of "xl" occurrences by hour
      ggplot(subset_data, aes(x = hour_of_day, fill = as.factor(hour_of_day))) +
        geom_histogram(binwidth = 1) +
        xlab("Hour of Day") +
        ylab("Number of Occurrences") +
        scale_fill_discrete(name = "Hour of Day", labels = function(x) format(strptime(x, format = "%H"), "%I %p")) +
        scale_x_continuous(breaks = c(0, 6, 12, 18, 23), limits = c(0, 23)) +
        theme_minimal() +
        guides(fill = FALSE) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
  })
  
}

shinyApp(ui = ui, server = server)