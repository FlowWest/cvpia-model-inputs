about_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 3),
      column(width = 6, 
             tags$h1('About'),
             includeMarkdown("templates/about.md"), 
             tags$div(id = "brand",
                      tags$hr(),
                      tags$a(tags$img(src = 'TransLogoTreb.png', width = 200, style = "display: block; margin: 0 auto;"),
                             href = 'http://www.flowwest.com/', target = '_blank'),
                      tags$h4('App created and maintained by',
                              tags$a('Emanuel Rodriguez', href = 'mailto:erodriguez@flowwest.com', target = '_blank'),
                              "and",
                              tags$a('Sadie Gill', href = 'mailto:sgill@flowwest.com', target = '_blank'),
                              style = "text-align: center;")
             )
             ), 
      column(width = 3)
    )
  )
  
}

about_server <- function(input, output, session) {
  
  ns <- session$ns
  
  
  
}