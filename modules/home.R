home_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # TODO create modal to welcome and describe the app
    fluidRow(
      column(width = 3, 
             selectInput(ns('region'), 'Select Region', 
                         choices = c(cvpiaData::watershed_ordering$watershed, 'North Delta', 'South Delta'))),
      column(width = 3,
             selectInput(ns('category'), 'Select Category', 
                         choices = c('Flow', 'Temperature', 'Habitat'))),
      column(width = 3,
             uiOutput(ns('data_type_input_ui'))),
      column(width = 3,
             uiOutput(ns('species_input_ui')))
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
  
  selected_region <- reactive({
    if (input$category == 'Habitat') {
      input$region
    } else {
      ifelse(input$region == 'delta', 'delta', 'watershed')
    }
  })
  
  y_axis_label <- reactive({
    metadata_lookup %>% 
      filter(region == selected_region(), data_type == input$data_type) %>% 
      pull(y_axis_label) %>% 
      unique()
  })
  
  selected_dataset <- reactive({
    df <- switch(input$category,
                 'Habitat' = habitat,
                 'Flow' = flows,
                 'Temperature' = temperatures)
    
    if (input$category == 'Habitat') {
      df %>%
        filter(region == input$region, species == input$species,
               data_type == input$data_type)
    } else {
      df %>%
        filter(region == input$region, data_type == input$data_type)
    }
  })
  
  output$data_type_input_ui <- renderUI({
    
    option <- metadata_lookup %>% 
      filter(region == selected_region(), category == input$category) %>% 
      pull(data_type) %>% 
      unique()
    
    selectInput(ns('data_type'), 'Select Data Type', 
                choices = option)
  })
  
  output$species_input_ui <- renderUI({
    if (input$category == 'Habitat') {
      selectInput(ns('species'), 'Select Species', 
                  choices = c('Fall Run', 'Spring Run', 'Winter Run', 'Steelhead'))
    } else {
      
    }
  })
  
  output$region_name <- renderUI({
    tags$h3(input$region)
  }) 
  
  output$data_type_name <- renderUI({
    description <- metadata_lookup %>% 
      filter(region == selected_region(), 
             category == input$category, 
             data_type == input$data_type)
    tagList(
      tags$h4(description$data_type),
      tags$p(description$metadata_description,
             tags$a(href = description$metadata_link, target = '_blank', 
                    'More info'))
    )
  })
  
  output$summary_stats <- renderTable({
    selected_dataset() %>% 
      pull(value) %>% 
      summary() %>% 
      broom::tidy() %>% 
      gather(stat, value)
  }, colnames = FALSE)
  
  
  output$time_series_plot <- renderPlotly({
    selected_dataset() %>% 
      plot_ly(x=~date, y=~value, type='scatter', mode='lines') %>% 
      layout(yaxis = list(title = y_axis_label(), rangemode = 'tozero')) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  
  
  
}