server<- function() {
  TABLE_MAP <- c('W'="xfe96wetqc",
                 'C' ="xfpwetqc",
                 'B' = "xfe24wetqc"
  )

  con <- adminKraken::con_dplyr()
  function(input, output, session){

    E <- reactiveValues(specs=NULL, BID=NULL, vals=NULL, BCG=NULL, TBL=NULL)

    chosen_table <- function(lot) {
      k <- toupper(substr(trimws(lot), 1, 1))
      if (!nzchar(k) || !(k %in% names(TABLE_MAP))) return(NULL)
      TABLE_MAP[[k]]
    }

    # build & return the table for a Lot/Inst
    build_tbl <- function(lot, inst){
      tbl_name <- chosen_table(lot)
      validate(need(!is.null(tbl_name), "Unknown lot prefix."))
      specs <- get_specs(con, lot)
      BID   <- barcodeValsID(con, lot)
      vals  <- get_lot_values(con, tbl_name, inst, lot)
      BCG   <- get_Gains(con, BID)
      generate_table(vals, specs, BCG)
    }

    # On Lot change: clear everything, repopulate instruments, and if exactly one, build immediately
    observeEvent(input$lot, {
      E$specs <- E$BID <- E$vals <- E$BCG <- E$TBL <- NULL

      lot <- trimws(input$lot %||% "")
      updateSelectInput(session, "inst", choices = character(0), selected = character(0))
      if (!nzchar(lot)) return()

      tbl_name <- chosen_table(lot)
      if (is.null(tbl_name)) return()

      ch <- tryCatch({
        tbl(con, tbl_name) %>%
          filter(.data$Lot == lot) %>%
          distinct(.data$Inst) %>%
          arrange(.data$Inst) %>%
          collect() %>% pull(.data$Inst)
      }, error = function(e) character(0))

      sel <- if (length(ch) == 1) ch[[1]] else character(0)
      updateSelectInput(session, "inst", choices = ch, selected = sel)

      if (length(ch) == 1) {
        E$TBL <- build_tbl(lot, sel)
      }
    }, ignoreInit = TRUE)

    # When user selects (or changes) an instrument, build the table
    observeEvent(input$inst, {
      req(nzchar(input$lot), nzchar(input$inst))
      E$TBL <- build_tbl(trimws(input$lot), input$inst)
    }, ignoreInit = TRUE)

    # status (minimal)
    output$state <- renderText({
      sprintf("lot='%s' inst='%s' rows=%s",
              trimws(input$lot %||% ""),
              input$inst %||% "",
              tryCatch(nrow(isolate(E$TBL)), error=function(e) NA_integer_))
    })

    # table render
    output$result_tbl <- renderTable({
      req(!is.null(E$TBL))
      if (nrow(E$TBL) == 0) data.frame(Note = "No rows for this Lot + Inst") else E$TBL
    }, rownames = FALSE)

    # download: render HTML, print to PDF via pagedown if requested
    output$download_report <- downloadHandler(
      filename = function() {
        ext <- if (identical(input$fmt, "pdf")) ".pdf" else ".html"
        paste0("RFS_Lot-", input$lot, "_Inst-", input$inst, "_", Sys.Date(), ext)
      },
      content = function(file) {
        req(!is.null(E$TBL), nrow(E$TBL) > 0)
        df <- E$TBL

        # get your Rmd (path or text)
        src <- generate_rmd(input$lot, input$inst, df)
        rmd_path <- if (is.character(src) && length(src) == 1 && file.exists(src)) {
          src
        } else {
          p <- tempfile(fileext = ".Rmd"); writeLines(src, p); p
        }

        # render to self-contained HTML
        out_html <- rmarkdown::render(
          rmd_path,
          output_format = rmarkdown::html_document(self_contained = TRUE),
          envir = list2env(list(tbl_out = df, lot = input$lot, inst = input$inst), parent = globalenv()),
          quiet = TRUE
        )

        # HTML → PDF via Edge/Chromium when requested
        if (identical(input$fmt, "pdf")) {
          # If needed: options(pagedown.chrome = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
          pagedown::chrome_print(input = out_html, output = file)
        } else {
          file.copy(out_html, file, overwrite = TRUE)
        }
      }
    )
  }
}
