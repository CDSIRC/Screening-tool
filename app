library(timevis)
load("saved_dfs.Rda")

# Define UI for data upload app ----
ui <- fluidPage(
  

  
  # Input: Select a file ----
  selectInput("case_number", "Case number:",
              c("####" = 1,
                "####" = 2, 
                "####" = 3, 
                "####" = 4, 
                "####" = 5, 
                "####" = 6, 
                "####" = 7, 
                "####" = 8), 
              selectize = FALSE),
  
  # Output: Data file ----
  timevisOutput("timeline"), 
  div("Date of event:", textOutput("selected_date", inline = TRUE)),
  div("Aged (years):", textOutput("selected_age", inline = TRUE)),
  div("Description:", textOutput("selected_text", inline = TRUE))
  
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$timeline <- renderTimevis(
    
    timevis(dfs[[as.numeric(input$case_number)]], 
            groups = data.frame(id = unique(dfs[[as.numeric(input$case_number)]]$group),
                                        content = unique(dfs[[as.numeric(input$case_number)]]$group)),
            showZoom = FALSE)
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
  
}

# Create Shiny app ----
shinyApp(ui, server)
