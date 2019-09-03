about_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 3),
      column(width = 6, 
             tags$h1('About'),
             includeMarkdown("templates/about.md")
             ), 
      column(width = 3)
    )
  )
  
}

about_server <- function(input, output, session) {
  
  ns <- session$ns
  
  
  
}