library(shiny)
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

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
           textAreaInput("log_input_text", "Enter log data:", value="my_mac,their_mac,rssi,local_time
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-86,1680043326
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-86,615148862
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-67,1680043386
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-67,615345530
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-57,1680043446
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-57,615148982
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-78,1680043506
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-64,615345650
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-102,615345650
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:92,-100,615345699
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:44,-88,615345699
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:44,-72,615349699"),
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
          textAreaInput("meta_input_text", "Enter meta data:", value="subject,data_type,data_value,local_time
HUX8768,conn,0.00,1679780520
HUX8768,conn,0.00,1679780577
HUX8768,mg,0.00,1679780631
HUX8768,deg_c,18.00,1679780691
HUX8768,vbatt,3.34,1679780691
HUX8768,conn,0.00,1679780719
HUX8768,deg_c,18.75,1679780871
HUX8768,vbatt,3.34,1679780871
HUX8768,conn,0.00,1679780958
HUX8768,conn,0.00,1679780994
HUX8768,xl,0.00,1679781028
HUX8768,mg,0.00,1679781029
HUX8768,xl,0.00,1679781036
HUX8768,deg_c,17.75,1679781051
HUX8768,vbatt,3.33,1679781051
HUX8768,deg_c,17.75,1679781231
HUX8768,vbatt,3.33,1679781231
HUX8768,deg_c,17.75,1679781411
HUX8768,vbatt,3.33,1679781411
HUX8768,deg_c,17.75,1679781591
HUX8768,vbatt,3.33,1679781591
HUX8768,deg_c,17.75,1679781771
HUX8768,vbatt,3.33,1679781771
HUX8768,deg_c,17.75,1679781951
HUX8768,vbatt,3.33,1679781951
HUX8768,deg_c,17.75,1679782131
HUX8768,vbatt,3.33,1679782131
HUX8768,deg_c,17.75,1679782311
HUX8768,vbatt,3.33,1679782311
HUX8768,deg_c,17.75,1679782491
HUX8768,vbatt,3.33,1679782491
HUX8768,deg_c,17.75,1679782671
HUX8768,vbatt,3.33,1679782671
HUX8768,deg_c,17.75,1679782851"),
          width = 5
        ),
        mainPanel(
          helpText("Temperature (°C)", align = "center"),
          plotOutput("deg_c_plot", height = "200px"),
          helpText("Battery (V)", align = "center"),
          plotOutput("vbatt_plot", height = "200px"),
          helpText("Movement", align = "center"),
          plotOutput("xl_hist", height = "200px"),
          width = 7
        )
      )
    )
  )
)

server <- function(input, output, session) {
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
  output$myText <- renderText({
    "This is my text."
  })
  
  meta_input_data <- reactive({
    req(input$meta_input_text)
    input_text <- input$meta_input_text
    input_text <- gsub("\r", "", input_text)
    input_text <- strsplit(input_text, "\n")
    input_text <- unlist(input_text)
    input_text <- input_text[-1] # remove the header row
    input_text <- strsplit(input_text, ",")
    input_df <- data.frame(do.call(rbind, input_text))
    colnames(input_df) <- c("subject", "data_type", "data_value", "local_time")
    input_df$local_time <- as.POSIXct(input_df$local_time, format = "%s", origin = "1970-01-01")
    input_df
  })
  
  output$deg_c_plot <- renderPlot({
    # Subset the input data to only include "deg_c" data type
    subset_data <- meta_input_data() %>%
      filter(data_type == "deg_c")
    
    # Convert local_time to a datetime format
    subset_data$local_time <- as.POSIXct(subset_data$local_time, origin = "1970-01-01")
    
    # Create the plot for deg_c
    ggplot(subset_data, aes(x = local_time, y = as.numeric(data_value))) +
      geom_line(color = "blue") +
      ylab("deg_c") +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
      scale_y_continuous(limits = c(15, 30), expand = expand_scale(mult = c(0, 0.1))) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(color = "blue"),
            axis.text.y = element_text(color = "blue"))
  })
  
  output$vbatt_plot <- renderPlot({
    # Subset the input data to only include "vbatt" data type
    subset_data <- meta_input_data() %>%
      filter(data_type == "vbatt")
    
    # Convert local_time to a datetime format
    subset_data$local_time <- as.POSIXct(subset_data$local_time, origin = "1970-01-01")
    
    # Create the plot for vbatt
    ggplot(subset_data, aes(x = local_time, y = as.numeric(data_value))) +
      geom_line(color = "red") +
      ylab("vbatt") +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
      scale_y_continuous(limits = c(2, 5), expand = expand_scale(mult = c(0, 0.1))) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(color = "red"),
            axis.text.y = element_text(color = "red"))
  })
  
  output$xl_hist <- renderPlot({
    # Subset the input data to only include "xl" data type
    subset_data <- meta_input_data() %>%
      filter(data_type == "xl")
    
    # Convert local_time to a datetime format
    subset_data$local_time <- as.POSIXct(subset_data$local_time, origin = "1970-01-01")
    
    # Extract hour of day from local_time
    subset_data$hour_of_day <- hour(subset_data$local_time)
    
    # Create a histogram of the number of "xl" occurrences by hour
    ggplot(subset_data, aes(x = hour_of_day, fill = as.factor(hour_of_day))) +
      geom_histogram() +
      xlab("Hour of Day") +
      ylab("Number of Occurrences") +
      scale_fill_discrete(name = "Hour of Day", labels = function(x) format(strptime(x, format = "%H"), "%I %p")) +
      scale_x_continuous(breaks = 0:23, limits = c(0, 23)) +
      theme_minimal() +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
}

shinyApp(ui = ui, server = server)