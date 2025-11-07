barcodeValsID<-function(con,inputLot){
  typenameL <- substr(inputLot,1,1)
  lotL<-substr(inputLot,2,nchar(inputLot))
  tbl(con,"lotview") %>%
  filter(Type==typenameL & `Lot Number`==lotL) %>%
  select(ID=`Barcode Matrix ID`) %>%
  collect() %>%
  .$ID
}
