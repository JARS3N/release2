get_Gains <-function(con,barcodeValsID){
  tbl(con,"barcodematrixview") %>%
  filter(ID==barcodeValsID) %>%
  mutate(G=(PH_A*PH_B)+PH_C) %>%
  select(O2_B,G) %>%
  collect()
}
