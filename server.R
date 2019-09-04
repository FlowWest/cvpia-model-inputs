function(input, output, session) {
  # first visit modal
  showModal(modalDialog(title = "Welcome!",
                        tagList(
                          tags$p("This web-tool was designed to explore inputs to the Science Integration Team’s salmon population model."),
                          tags$p("Use the tool to:"),
                          tags$ul(
                            tags$li("compare model inputs to your understanding of CVPIA watersheds"),
                            tags$li("develop insights"),
                            tags$li("make suggestions to improve the model")
                          ),
                          tags$p("Please help us make this more useful!")
                        ),
                        easyClose = TRUE))
  callModule(about_server, 'app')
  callModule(home_server, 'app')
}