function(input, output, session) {
  # first visit modal
  showModal(modalDialog(title = "Welcome!",
                        tagList(
                          tags$p("Something something something"), 
                          tags$p("Is having a modal popup on the first") 
                        ),
                        easyClose = TRUE))
  
  callModule(about_server, 'app')
  callModule(home_server, 'app')
}