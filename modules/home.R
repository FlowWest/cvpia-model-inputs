home_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # TODO create modal to welcome and describe the app
    fluidRow(
      column(width = 2, 
             selectInput(ns('region'), 'Select Region', 
                         choices = cvpiaData::watershed_ordering$watershed)),
      column(width = 2,
             selectInput(ns('category'), 'Select Category', 
                         choices = c('Flow', 'Temperature', 'Habitat'))),
      column(width = 2,
             uiOutput(ns('data_type_input_ui')))
      
    ),
    fluidRow(
      column(width = 12, 
             uiOutput(ns('region_name')), 
             uiOutput(ns('data_type_name')))
    ),
    fluidRow(
      column(width = 3, 
             tags$h5("Summary Statistics"),
             tableOutput(ns('summary_stats'))),
      column(width = 9, 
             plotlyOutput(ns('time_series_plot')))
    )
  )
  
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  
  output$data_type_input_ui <- renderUI({
    
    # based on the category chosen return the appropriate 
    # vector of the data_type choices
    data_type_choices <- switch(input$category, 
           'Flow' = c("Monthly Mean Flow", 
                      "Monthly Mean Diverted", 
                      "Monthly Mean Proportion Diverted"), 
           'Temperature' = c("Monthly Mean Temperature", "Degree Days"), 
           'Habitat' = c("Monthly In-channel Rearing Area", 
                         "Monthly Rearing Area",
                         "Monthly Floodplain Rearing Area", 
                         "Monthly Spawning Rearing Area"))

        selectInput(ns('data_type'), 'Select Data Type', 
                choices = data_type_choices)
  })
  
  output$region_name <- renderUI({
    tags$h3(input$region)
  }) 
  
  
  output$data_type_name <- renderUI({
    
    req(input$data_type)
    
    description <- metadata_lookup %>% 
      filter(region == input$region, 
             category == input$category, 
             data_type == input$data_type)
    tagList(
      tags$p(description$data_type),
      tags$p(description$metadata_description),
      tags$a(href = description$metadata_link, target = '_blank', 
             'More info')
    )
  })
  
  
  output$summary_stats <- renderTable({
    df %>% 
      pull(monthly_mean_temp_c) %>% 
      summary() %>% 
      broom::tidy() %>% 
      gather(stat, value)
  }, colnames = FALSE)
  
  
  output$time_series_plot <- renderPlotly({
    df %>% 
      plot_ly(x=~date, y=~monthly_mean_temp_c, type='scatter', mode='lines')
  })
  
  
  
  
  
}