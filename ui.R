shinyUI(
  navbarPage(
    title = div(img(src="lamp.png", width = '70%'), NULL), 
    header = includeCSS("www/styles.css"),
    collapsible = TRUE,
    windowTitle = "CVPIA Model Inputs",
    theme = shinytheme(theme = 'readable'),
    tabPanel('Explore CVPIA Model Inputs', home_ui('app')),
    tabPanel('About', about_ui('app'))
  )
)

