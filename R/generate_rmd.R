generate_rmd<- function(Lot, Inst, ctg_means) {
  skeleton <- rmd_skeleton() %>%
    gsub("%lotline%", Lot, .) %>%
    gsub("%instline%", Inst, .) %>%
    gsub("%Date%",Sys.Date(),.)

  tbl <- which(skeleton == "%table%")

  paste0(c(skeleton[1:(tbl - 1)],
          get_kable(ctg_means),
           skeleton[(tbl + 2):length(skeleton)])
         , collapse = "\n")
}
