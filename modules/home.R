home_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h1('model inputs'),
             tags$p('some really interesting things')
             )
    )
  )
  
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  
  
}