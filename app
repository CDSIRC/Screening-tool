library(timevis)
library(ggplot2)
library(dplyr)
library(ggraph)
library(tidygraph)
library(shiny)
library(leaflet)
library(htmltools)
library(imager)

load("saved_dfs.Rda")
load("saved_networks.Rda")


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CDSIRC Screening App"),
  
  # Input: Select a file ----
  selectInput("case_number", "Case number:",
              c("Demonstration" = 1),
              selectize = FALSE),
  
  # Output: Data file ----
  tabsetPanel(type = "tabs",
              tabPanel("Timeline",
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           h4("Click on a baloon to the right for more information. Drag and zoom the timeline to see more events."), 
                           div("Date of event:", textOutput("selected_date", inline = TRUE)),
                           div("Aged (years):", textOutput("selected_age", inline = TRUE)),
                           div("Description:", textOutput("selected_text", inline = TRUE))#
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: ----
                           timevisOutput("timeline")
                         )
                       )
              ),
              
              tabPanel("Significant other network",
                       #  Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         
                         #Sidebar panel for inputs ----
                           sidebarPanel(
                             
                             radioButtons("selected", label = h3("Select a factor to colour nodes by"), 
                                                choices = list("Alert", "Sex", "Relationship"),
                                                selected = "Alert")
                             
                           ),
                         
                         #Main panel for displaying outputs ----
                         mainPanel(
                           # Output: ----
                           plotOutput("network", width = "100%")
                         )
                       )
              ),
              tabPanel("Residential locations", 
                       #  Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         
                         #Sidebar panel for inputs ----
                         sidebarPanel(
                           
                           h3("Hover on a location for more inforamtion. Drag and zoom the map for more detail.")
                           
                         ),
                         
                         #Main panel for displaying outputs ----
                         mainPanel(
                       leafletOutput("mymap")
                       
                         )
                       )
              )
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  filtered_df <- reactive({
    
    dfs[[as.numeric(input$case_number)]] # %>% 
     # filter(group %in% input$checkGroup) %>%
     # arrange(as.Date(start)) %>%
     # mutate(id = row_number())
    
  })
  
  
  
  output$timeline <- renderTimevis(
    
    timevis(filtered_df(), 
            groups = data.frame(id = unique(filtered_df()$group),
                                content = unique(filtered_df()$group)),
            showZoom = TRUE)
  )
  
  output$selected_text <- renderText(
    
    as.character(input$timeline_data$text[as.numeric(input$timeline_selected)])
    
  )
  
  output$selected_date <- renderText(
    
    as.character(input$timeline_data$date_as_text[as.numeric(input$timeline_selected)])
    
  )
  
  output$selected_age <- renderText(
    
    as.character(round(as.numeric(difftime(input$timeline_data$start[as.numeric(input$timeline_selected)], input$timeline_data$start[which(input$timeline_data$content == "Birth")])/365), digits = 1))
    
  )
  
  this_network <- reactive({

    networks[[as.numeric(input$case_number)]]

  })
  
  selected_factor <- reactive({
    
    input$selected
    
  })

  output$network <- renderPlot({

    ggraph(this_network()) +
      geom_edge_diagonal() +
      geom_node_point(alpha = 0.2, size = 10, aes(colour = get(selected_factor()))) +
      geom_node_text(aes(label = paste(paste0(4, substr(ID, 6, 8), substr(ID, 2, 5)), "\n", Relationship))) + 
      theme_graph() + 
      labs(colour = "")

  }, height = 800, width = 800)
  
  content <- c("Residence with mother")
  
  points <- eventReactive(input$recalc, {
    cbind(c(138.538024), c(-35.994730))
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points()) %>%
      addPopups(cbind(c(138.538024), c(-35.994730)), content)
  })
  
  Name <- c("Mother's house", 
            "Boyfriend's house", 
            "Muggy's accomodation", 
            "Housing SA accomodation")
  Long <- c(138.5721, 144.9612, 138.6740, 138.601)
  Lat <- c(-35.0255, -37.7964, -34.9103, -34.923915)
  
  df <- data.frame(Name, Long, Lat)
  
  output$mymap <- renderLeaflet({
    leaflet(df) %>% 
      addTiles() %>% #addProviderTiles(Stamen.Watercolor) %>%
      addMarkers(~Long, ~Lat, label = ~htmlEscape(Name))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
