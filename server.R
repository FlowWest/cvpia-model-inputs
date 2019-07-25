function(input, output, session) {
  # first visit modal
  showModal(modalDialog(title = "Welcome!",
                        taglist(
                          tags$p("Something something something")
                        ),
                        easyClose = TRUE))
  
  callModule(about_server, 'app')
  callModule(home_server, 'app')
}