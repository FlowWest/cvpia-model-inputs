shinyUI(
  
  navbarPage(
    title = div(img(src="lamp.png"), NULL), 
    header = includeCSS("www/styles.css"),
    collapsible = TRUE,
    windowTitle = "CVPIA Model Inputs",
    theme = shinytheme(theme = 'readable'),
    tabPanel('Home', home_ui('app')),
    tabPanel('About', about_ui('app'))
  )
)