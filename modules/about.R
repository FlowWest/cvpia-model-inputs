about_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h1('About'),
             tags$p('some really interesting things')
             )
    )
  )
  
}

about_server <- function(input, output, session) {
  
  ns <- session$ns
  
  
  
}