generate_table <- function(lot_vals, specs, get_Gains) {
  # uses outputs from:
  # lot_vals = get_lot_values()
  # specs = get_specs()
  # get_Gains = get_Gains()
  spc_strs <- spec_strings(specs, get_Gains)
  res <- test_results(lot_vals, specs, get_Gains)
  df<-data.frame(
    attributes = c(    "O2LED avg" ,
                       "O2LED %cv" ,
                       "PHLED avg" ,
                       "PHLED %cv" ,
                       "Gain avg"  ,
                       "Gain %cv"  ,
                       "KSV avg"   ,
                       "KSV %cv"   ),
    values = format(
      c(
        "O2LED avg" = lot_vals$O2LED,
        "O2LED %cv" = lot_vals$O2CV,
        "PHLED avg" = lot_vals$PHLED,
        "PHLED %cv" = lot_vals$PHCV,
        "Gain avg"  = lot_vals$GAIN,
        "Gain %cv"  = lot_vals$GAINCV,
        "KSV avg"   = lot_vals$KSV,
        "KSV %cv"   = lot_vals$KSVCV
      ), scientific = FALSE, trim = TRUE
    ),
    specs = spc_strs,
    results = res)
  row.names(df)<-NULL
  df
}
