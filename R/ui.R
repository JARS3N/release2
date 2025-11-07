ui <- function() {
  fluidPage(
    textInput("lot",  "Lot", placeholder = "e.g. W23777"),
    selectInput("inst","Instrument", choices = character(0)),
    verbatimTextOutput("state"),   # status line
    hr(),
    h4("Results for selected Lot , Instrument"),
    radioButtons("fmt", "Report format", choices = c("HTML" = "html", "PDF" = "pdf"), inline = TRUE),
    downloadButton("download_report", "Download report"),
    tableOutput("result_tbl")
  )
}
