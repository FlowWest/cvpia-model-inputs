home_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12, 
             tags$h1('Explore CVPIA Model Inputs'), 
             tags$p('Expl.... text of what do here'))
      
    ),
    fluidRow(
      column(width = 2, 
             selectInput(ns('region'), 'Select Region', 
                         choices = letters[1:8])),
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
      column(width = 12)
    )
  )
  
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  
  
  output$data_type_input_ui <- renderUI({
    
    option <- switch(input$category, 
                     'Flow' = c(1, 2, 3), 
                     'Temperature' = 4:6, 
                     'Habitat' = 7:9)
    
    selectInput(ns('data_type'), 'Select Data Type', 
                choices = option)
  })
  
  output$region_name <- renderUI({
    tags$h3(input$region)
  }) 
  output$data_type_name <- renderUI({
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
  
  
}