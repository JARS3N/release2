test_results <- function(VALS, specs, gains) {
  pf<- c(
    "O2LED avg" = VALS$O2LED >= specs$O2LED_avg_low &
      VALS$O2LED <= specs$O2LED_avg_high,
    "O2LED %cv" = VALS$O2CV <= specs$O2LED_avg_cv_high ,
    "PHLED avg" = VALS$PHLED >= specs$pHLED_avg_low &
      VALS$PHLED <= specs$pHLED_avg_high,
    "PHLED %cv" = VALS$PHCV <= specs$pHLED_avg_cv_high,
    "Gain avg"  = (abs(VALS$GAIN - gains$G) / gains$G) <= specs$GAIN_BC_DIFF,
    "Gain %cv"  = VALS$GAINCV <= specs$Gain_cv_high,
    "KSV avg"   = (abs(VALS$KSV - gains$O2_B) / gains$O2_B) <= specs$KSV_BC_DIFF,
    "KSV %cv"   = VALS$KSVCV <= specs$ksv_cv_high
  )
  setNames(
    sapply(pf,dplyr::if_else,"PASS","FAIL"),
    names(pf)
  )
}
