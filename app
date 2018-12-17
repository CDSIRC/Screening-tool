library(timevis)
library(dplyr)
library(shiny)

timelines_list <- readRDS("saved_timelines.Rda")
cases_with_timelines <- names(timelines_list)

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
              )
              
  )
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
  
  output$selected_content <-  renderText(
    
    #as.character(input$mytime_data$content[as.numeric(input$mytime_selected)])
    #as.character(input$mytime_selected)
    paste0("Additional information: ", as.character(filtered_df()$text[as.numeric(input$mytime_selected)]))
    
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)
