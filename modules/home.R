home_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # TODO create modal to welcome and describe the app
    fluidRow(
      column(width = 4, 
             selectInput(ns('region'), 'Select Region', 
                         choices = c(cvpiaData::watershed_ordering$watershed, 'North Delta', 'South Delta'))),
      column(width = 4,
             selectInput(ns('category'), 'Select Category', 
                         choices = c('Flow', 'Temperature', 'Habitat'))),
      column(width = 4,
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
    
    if (input$category == 'Habitat') {
      selected_region <- input$region
    } else {
      selected_region <- ifelse(input$region == 'delta', 'delta', 'watershed')
    }
    
    option <- metadata_lookup %>% 
      filter(region == selected_region, category == input$category) %>% 
      pull(data_type) %>% 
      unique()
    
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
  
  # output_df <- reactive({
  #   if (input$category == 'Flow') {
  #     
  #     tmp <- switch(input$data_type, 
  #            '1' = cvpiaFlow::flows_cfs, 
  #            '2' = cvpiaFlow::bypass_flows, 
  #            '3' = cvpiaFlow::delta_flows)
  #     
  #   } else if (input$category == 'Temperature') {
  #     
  #     tmp <- switch(input$data_type, 
  #                   '4' = cvpiaTemperature::juv_temp, 
  #                   '5' = cvpiaTemperature::delta_temps, 
  #                   '6' = cvpiaTemperature::deg_days)
  #     
  #   } else {
  #     
  #   }
  # })
  # 
  
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