spec_strings<-function(S,B){
  # takes in results of
  # S = get_specs()
  # B = get_Gains()

  more<-">= "
  and <- " & "
  less <- "<="
  pm<-" +/- "

  c(
    "O2LED avg" = paste0(more,S$O2LED_avg_low,and,less,S$O2LED_avg_high),
    "O2LED %cv" = paste0("<",S$O2LED_avg_cv_high),
    "PHLED avg" = paste0(more,S$pHLED_avg_low,and,less,S$pHLED_avg_high),
    "PHLED %cv" = paste0("<",S$pHLED_avg_cv_high),
    "Gain avg"  = paste0(B$G,pm,B$G*S$GAIN_BC_DIFF),
    "Gain %cv"  = paste0("<",S$Gain_cv_high),
    "KSV avg"   = paste0(B$O2_B,pm,B$O2_B*S$KSV_BC_DIFF),
    "KSV %cv"   = paste0("<",S$ksv_cv_high)
  )
}
