function(input, output, session) {
  # first visit modal
  showModal(modalDialog(title = "Welcome!",
                        tagList(
                          tags$p("This web-tool was designed to explore inputs to the Science Integration Team’s salmon population model. Use the tool to compare model inputs to your understanding of CVPIA watersheds, develop insights, and / or make suggestions to improve the model. Please help us make this more useful!") 
                        ),
                        easyClose = TRUE))
  
  callModule(about_server, 'app')
  callModule(home_server, 'app')
}