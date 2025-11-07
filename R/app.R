app <- function() {
  shiny::runApp(
    shiny::shinyApp(
      ui     = release2::ui(),                   # <- use your actual UI fn name
      server = release2::server()  # <- and your server fn
    ),
    launch.browser = TRUE
  )
}

