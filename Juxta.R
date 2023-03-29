library(shiny)
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)
# library(shinyjs)

ui <- fluidPage(
  # useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Change the font of the input textarea */
      .form-control {
        font-family: Courier, sans-serif;
        font-size: 1REM;
        resize: none;
    "))
  ),
  titlePanel("Hello, Juxta"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("input_text", "Enter data:", rows=40, value="my_mac,their_mac,rssi,local_time
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-86,1680043326
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-86,615148862
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-67,1680043386
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-67,615345530
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-57,1680043446
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-57,615148982
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-78,1680043506
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-78,615345650
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-78,615345650
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-96,1680043566
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-96,615345710
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AA,-96,615345710
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-92,1680043626
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-92,613576298
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-92,613576298
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:92,-87,1680043686
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:92,-87,615149222
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-94,1680043746
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-94,613576418
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-94,613576418
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:92,-94,1680043806
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:92,-94,615345950
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:92,-94,615345950
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-82,1680043865
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-82,615149401
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-82,615149401
6C:B2:FD:CE:24:B2,6C:B2:FD:CE:24:AD,-94,1680043926"),
      # radioButtons("option_selector", "Select an option:",
      #              choices = c("Weight by Frequency")),
      # actionButton("submitBtn", "Submit"),
      width = 5
    ),
    mainPanel(
      helpText("Social Network", align = "center"),
      plotOutput("graph_plot"),
      helpText("RSSI vs. MAC", align = "center"),
      plotOutput("box_plot"),
      width = 7
    )
  )
)

server <- function(input, output, session) {
  # populate the textarea with sample data when the app loads
  observe({
    # simulate click on load
    # runjs("$('#submitBtn').click();")
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
  
  observeEvent(input$input_text, {
    # Render graph plot
    output$graph_plot <- renderPlot({
      # Get input text and create graph
      input_text <- input$input_text
      graph <- createGraph(input_text)
      # make max weight = * pts
      max_weight <- max(E(graph)$weight)
      E(graph)$scaled_weight <- E(graph)$weight / max_weight * 15
      E(graph)$width <- E(graph)$scaled_weight
      # Plot graph
      plot(graph, layout = layout_with_fr, vertex.label.cex = 1.25, vertex.label.color = "black",
           vertex.color = "lightblue", edge.color = "gray", vertex.size = 20, asp = 1,
           vertex.label.family="Courier", edge.label = E(graph)$weight)
    })
    
    output$box_plot <- renderPlot({
      data <- read.table(text = input$input_text, sep = ",", header = TRUE)
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
}

shinyApp(ui, server)