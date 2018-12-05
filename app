library(timevis)
#library(ggplot2)
library(dplyr)
#library(ggraph)
#library(tidygraph)
library(shiny)
#library(leaflet)
#library(htmltools)
#library(imager)

timelines_list <- readRDS("saved_timelines.Rda")
cases_with_timelines <- names(timelines_list)

#load("saved_networks.Rda")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CDSIRC Screening App"),
  
  # Input: Select a file ----
  
  uiOutput("validated"),
  
  # Output: Data file ----
  tabsetPanel(type = "tabs",
              tabPanel("Timeline",
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           uiOutput("select_groups"), #this comes from the server
                           
                           hr(),
                           
                           div(textOutput("selected_content")),
                           
                           hr(),
                           
                           
                           # div("Date of event:", textOutput("selected_date", inline = TRUE)),
                           div(textOutput("selected_age", inline = TRUE))
                           # div("Description:", textOutput("selected_text", inline = TRUE))
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: ----
                           textOutput("timeline_instructions"),
                           timevisOutput("mytime")
                         )
                       )
              )#,
              
              # tabPanel("Significant other network",
              #          #  Sidebar layout with input and output definitions ----
              #          sidebarLayout(
              # 
              #            #Sidebar panel for inputs ----
              #              sidebarPanel(
              # 
              #                radioButtons("selected", label = h3("Select a factor to colour nodes by"),
              #                                   choices = list("Alert", "Sex", "Relationship"),
              #                                   selected = "Alert")
              # 
              #              ),
              # 
              #            #Main panel for displaying outputs ----
              #            mainPanel(
              #              # Output: ----
              #              plotOutput("network", width = "100%")
              #            )
              #          )
              )#,
              # tabPanel("Residential locations", 
              #          #  Sidebar layout with input and output definitions ----
              #          sidebarLayout(
              #            
              #            #Sidebar panel for inputs ----
              #            sidebarPanel(
              #              
              #              h3("Hover on a location for more inforamtion. Drag and zoom the map for more detail.")
              #              
              #            ),
              #            
              #            #Main panel for displaying outputs ----
              #            mainPanel(
              #          leafletOutput("mymap")
              #          
              #            )
              #          )
              # )

)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$validated <- renderUI({
    
      selectInput("case_number", "Case number:",
                  cases_with_timelines,
                  selectize = FALSE)
    
    })
  
    case_order <- reactive({
    
    which(cases_with_timelines == input$case_number)
    
  })
  
  filtered_df <- reactive({
    
    timelines_list[[as.numeric(case_order())]] %>%
      filter(group %in% input$group_choice) %>%
      arrange(start) %>%
      mutate(id = row_number())
    
  })
  
  var <- reactive({
    
    unique(timelines_list[[as.numeric(case_order())]]$group) #This is needed so that select_groups isn't defined in terms of filtered_df which is defined in terms of select_groups etc (it created a loop of lookin gup each other that created an error)
    
  })
  
  output$select_groups <- renderUI({
    
    checkboxGroupInput("group_choice", "Select categories of events to show more or less in the timeline", choices = var(), selected = var())
    
  })
  
  
  output$mytime <- renderTimevis(
    
    timevis(filtered_df(), 
            groups = data.frame(id = unique(filtered_df()$group),
                                content = unique(filtered_df()$group)),
            showZoom = TRUE)
  )
  
  timeline_instructions_text <- reactive({
    
    "Click on a text balloon for more information and drag and zoom the timeline for more events"
    
  })
  
  output$timeline_instructions <- renderText(
    
    timeline_instructions_text()
    
  )
  
  date_of_birth <- reactive ({
    
    min(timelines_list[[as.numeric(case_order())]]$start)
    
  })
  
  date_of_event <- reactive({
  
    filtered_df()$start[as.numeric(input$mytime_selected)]
    
  })
  
  output$selected_age <- renderText(
    
    paste0("Aged at event (years): ", as.character(round(as.numeric(difftime(date_of_event(), date_of_birth(), "days"))/365.25), digits = 1))
    
  )
  
  
  
  
  
  # output$selected_text <- renderText(
  #   
  #   as.character(input$timeline_data$text[as.numeric(input$mytime_selected)])
  #   
  # )
  
  # output$selected_date <- renderText(
  #   
  #   as.character(input$timeline_data$date_as_text[as.numeric(input$mytime_selected)])
  #   
  # )
  
  # output$selected_age <- renderText(
  #   
  #   as.character(round(as.numeric(difftime(input$timeline_data$start[as.numeric(input$timeline_selected)], input$timeline_data$start[which(input$timeline_data$content == "Birth")])/365), digits = 1))
  #   
  # )
  
  output$selected_content <-  renderText(
    
    #as.character(input$mytime_data$content[as.numeric(input$mytime_selected)])
    #as.character(input$mytime_selected)
    paste0("Additional information: ", as.character(filtered_df()$text[as.numeric(input$mytime_selected)]))

  )
  
  # this_network <- reactive({
  # 
  #   networks[[as.numeric(input$case_number)]]
  # 
  # })
  # 
  # selected_factor <- reactive({
  #   
  #   input$selected
  #   
  # })
  # 
  # output$network <- renderPlot({
  # 
  #   ggraph(this_network()) +
  #     geom_edge_diagonal() +
  #     geom_node_point(alpha = 0.2, size = 10, aes(colour = get(selected_factor()))) +
  #     geom_node_text(aes(label = paste(paste0(4, substr(ID, 6, 8), substr(ID, 2, 5)), "\n", Relationship))) + 
  #     theme_graph() + 
  #     labs(colour = "")
  # 
  # }, height = 800, width = 800)
  # 
  # content <- c("Residence with mother")
  # 
  # points <- eventReactive(input$recalc, {
  #   cbind(c(138.538024), c(-35.994730))
  # }, ignoreNULL = FALSE)
  # 
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles(providers$Stamen.TonerLite,
  #                      options = providerTileOptions(noWrap = TRUE)
  #     ) %>%
  #     addMarkers(data = points()) %>%
  #     addPopups(cbind(c(138.538024), c(-35.994730)), content)
  # })
  # 
  # Name <- c("Mother's house", 
  #           "Boyfriend's house", 
  #           "Muggy's accomodation", 
  #           "Housing SA accomodation")
  # Long <- c(138.5721, 144.9612, 138.6740, 138.601)
  # Lat <- c(-35.0255, -37.7964, -34.9103, -34.923915)
  # 
  # df <- data.frame(Name, Long, Lat)
  # 
  # output$mymap <- renderLeaflet({
  #   leaflet(df) %>% 
  #     addTiles() %>% #addProviderTiles(Stamen.Watercolor) %>%
  #     addMarkers(~Long, ~Lat, label = ~htmlEscape(Name))
  # })
  
}

# Create Shiny app ----
shinyApp(ui, server)
