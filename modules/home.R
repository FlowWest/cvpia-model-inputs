home_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 3, 
             selectInput(ns('region'), 'Select Region', 
                         choices = c(cvpiaData::watershed_ordering$watershed, 'North Delta', 'South Delta'))),
      column(width = 2,
             selectInput(ns('category'), 'Select Category', 
                         choices = c('Flow', 'Temperature', 'Habitat'))),

      column(width = 4,
             uiOutput(ns('data_type_input_ui'))),
      column(width = 2,
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
      column(width = 7, 
             withSpinner(plotlyOutput(ns('time_series_plot')), type = 8, color = "#666666"), 
             uiOutput(ns("scaled_note"))), 
      column(width = 2, 
             uiOutput(ns('show_unscaled_ui')))
    )
  )
  
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  selected_region <- reactive({
    if (input$category == 'Habitat') {
      input$region
    } else {
      if (input$region %in% c('South Delta', 'North Delta')) {
        'delta'
      } else if (input$region %in% c('Sutter Bypass', 'Yolo Bypass')) {
        'bypass'
      } else {
        'watershed'
      }
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
      req(input$species)
      df %>%
        filter(region == input$region, 
               species == input$species,
               data_type == input$data_type) %>%
        na.omit()
    } else {
      df %>%
        filter(region == input$region, 
               data_type == input$data_type)
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
                    choices = c('Fall Run', 'Spring Run', 'Winter Run', 'Steelhead'), 
                    width = 150)
      
    } else {
      NULL
    }
  })
  
  # determine if the selected combination of data has a scaling factor
  # this is used to determine when to show the checkbox and if to add an
  # additional dashed line to the plot
  data_has_scaling <- reactive({
    if (input$category == "Habitat") {
      scale <- selected_dataset() %>%
        head(1) %>%
        pull(scale)
    } else {
      scale <- 1
    }

    scale != 1
  })

  # Show scaled or unscaled version of the habitat?
  output$show_unscaled_ui <- renderUI({
    if (input$category == 'Habitat' & data_has_scaling()) {
      tags$div(style = "margin-top:33px",
      checkboxInput(ns('show_unscaled'), 'Show Unscaled', value = TRUE))
    } else {
      NULL
    }  })
  
  
  output$region_name <- renderUI({
    tags$h3(input$region)
  }) 
  
  output$data_type_name <- renderUI({
    
    req(input$data_type)
    
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
    req(input$data_type)
    
    stat_label <- metadata_lookup %>% 
      filter(region == selected_region(), category == input$category, 
             data_type == input$data_type) %>% 
      pull(stat_label)
    
    # if (input$category == "Habitat" & data_has_scaling()) {
    #   
    #   unscaled_summary <- selected_dataset() %>% 
    #     pull(value) %>% 
    #     summary() %>% 
    #     broom::tidy() %>% 
    #     gather(stat, unscaled_value) %>% 
    #     mutate(unscaled_value = paste(pretty_num(unscaled_value, 0)))
    #   
    #   scaled_summary <- selected_dataset() %>% 
    #     pull(scaled_habitat) %>% 
    #     summary() %>% 
    #     broom::tidy() %>% 
    #     gather(stat, scaled_value) %>% 
    #     mutate(scaled_value = paste(pretty_num(scaled_value, 0)))
    #   
    #   full_summary <- left_join(unscaled_summary, scaled_summary) %>% 
    #     transmute(
    #       Stat = stat,
    #       `Scaled Habitat (acres)` = scaled_value, 
    #       `Original Habitat (acres)` = unscaled_value)
    #   
    # } else {
      full_summary <- selected_dataset() %>% 
        pull(value) %>% 
        summary() %>% 
        broom::tidy() %>% 
        gather(stat, value) %>% 
        mutate(value = paste(pretty_num(value, 0))) 
      
      col_names <- switch(input$category, 
                          "Flow" = c("Stat", "Flow (cfs)"), 
                          "Temperature" = c("Stat", "Temperature (°C)"), 
                          "Habitat" = c("Stat", "Habitat (acres)"))
      
      colnames(full_summary) <- col_names
    # }
    
    full_summary
  })
  
  
  
  output$time_series_plot <- renderPlotly({
    if (input$category == "Habitat" & input$data_type == "Monthly Floodplain Rearing Area") {
      gg <- selected_dataset() %>%
        arrange(date) %>% 
        ggplot(aes(date, value, fill = weeks_flooded)) +
        theme_minimal() + 
        geom_col() +
        guides(fill = guide_legend(title="Wks Fld")) + 
        theme(legend.position = 'bottom')
      p <- ggplotly(gg)

     } else {
      p <- selected_dataset() %>% 
        arrange(date) %>% 
        plot_ly(x=~date, y=~value, type='scatter', mode='lines', 
                line = list(color = pal[1])) 
     }
    
    # return
    p %>% 
      layout(yaxis = list(title = y_axis_label(), rangemode = 'tozero')) %>% 
      config(displayModeBar = FALSE)
  })
  
  # show this note only when the data has scaling but regardless of whether 
  # the checkbox is checked or not
  # output$scaled_note <- renderUI({
  #   if (input$category == "Habitat" & data_has_scaling()) {
  #     scaling_factor <- selected_dataset() %>% head(1) %>% pull(scale) %>% round(2)
  #     tags$p(tags$em("The original habitat modeling values were scaled by a factor of", 
  #                    tags$b(scaling_factor), "during the calibration process."))
  #   }
  # })
  
}