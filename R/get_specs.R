get_specs<-function(con,inputLot){
  tbl(con,"wqcspecs") %>%
    filter(typename==substr(inputLot,1,1)) %>%
    collect()
}
