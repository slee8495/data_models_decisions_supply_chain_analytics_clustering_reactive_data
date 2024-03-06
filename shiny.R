library(tidyverse)
library(shiny)
library(readxl)
library(DT)
library(shinyjs)
library(shinyWidgets)

read_excel("order_late.xlsx") -> order_late

ui <- fluidPage(
  tabPanel("View Data",
           br(),
           fluidRow(
             uiOutput("cluster_ui"), # Dynamic UI for cluster
             uiOutput("profile_owner_ui"), # Dynamic UI for profile_owner
             uiOutput("leader_name_ui"), # Dynamic UI for leader_name
             uiOutput("loc_ui"), # Dynamic UI for loc
             uiOutput("on_time_ui"), # Dynamic UI for on_time
             uiOutput("days_to_acknowledge_ui") # Dynamic UI for days_to_acknowledge
           ),
           fluidRow(
             column(12, DTOutput("viewData"))
           ))
)

server <- function(input, output, session) {
  # Generating dynamic UIs for each filter
  output$cluster_ui <- renderUI({
    column(2, pickerInput("cluster", "Cluster:", 
                          choices = sort(unique(order_late$cluster)),
                          selected = sort(unique(order_late$cluster)),
                          multiple = TRUE, options = list(`actions-box` = TRUE)))
  })
  
  output$profile_owner_ui <- renderUI({
    column(2, pickerInput("profile_owner", "Profile Owner:", choices = sort(unique(order_late$profile_owner)), 
                          selected = sort(unique(order_late$profile_owner)), multiple = TRUE, options = list(`actions-box` = TRUE)))
  })
  
  output$leader_name_ui <- renderUI({
    column(2, pickerInput("leader_name", "Leader Name:", choices = sort(unique(order_late$leader_name)), 
                          selected = sort(unique(order_late$leader_name)), multiple = TRUE, options = list(`actions-box` = TRUE)))
  })
  
  output$loc_ui <- renderUI({
    column(2, pickerInput("loc", "Location:", choices = sort(unique(order_late$loc)), 
                          selected = sort(unique(order_late$loc)), multiple = TRUE, options = list(`actions-box` = TRUE)))
  })
  
  output$on_time_ui <- renderUI({
    column(2, pickerInput("on_time", "On Time:", choices = sort(unique(order_late$on_time)), 
                          selected = sort(unique(order_late$on_time)), multiple = TRUE, options = list(`actions-box` = TRUE)))
  })
  

  
  observe({
    # Filter data based on selected clusters
    filtered_data <- order_late
    
    if (!is.null(input$cluster)) {
      filtered_data <- filtered_data %>% filter(cluster %in% input$cluster)
    }
    
    updatePickerInput(session, "profile_owner", choices = unique(filtered_data$profile_owner))
    updatePickerInput(session, "leader_name", choices = unique(filtered_data$leader_name))
    updatePickerInput(session, "loc", choices = unique(filtered_data$loc))
  })
  
  data_to_display <- reactive({
    filtered_data <- order_late %>%
      filter(if (!is.null(input$cluster)) cluster %in% input$cluster else TRUE) %>%
      filter(if (!is.null(input$profile_owner)) profile_owner %in% input$profile_owner else TRUE) %>%
      filter(if (!is.null(input$leader_name)) leader_name %in% input$leader_name else TRUE) %>%
      filter(if (!is.null(input$loc)) loc %in% input$loc else TRUE) %>%
      filter(if (!is.null(input$on_time)) as.character(on_time) %in% input$on_time else TRUE) 
    
    filtered_data
  })
  
  output$viewData <- renderDT({
    datatable(data_to_display(), options = list(scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)