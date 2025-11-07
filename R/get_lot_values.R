get_lot_values<-function(con,tbl,Instrument,InputLot){
 tbl(con,tbl) %>%
  filter(Lot==InputLot,Inst==Instrument) %>%
  group_by(sn) %>%
  summarise(
    O2LEDMU=mean(O2.LED,na.rm=T),
    O2LEDSD=sd(O2.LED,na.rm=T),
    O2LEDCV=100*(sd(O2.LED,na.rm=T)/mean(O2.LED,na.rm=T)),
    PHLEDMU=mean(pH.LED,na.rm=T),
    PHLEDSD=sd(pH.LED,na.rm=T),
    PHLEDCV=100*(sd(pH.LED,na.rm=T)/mean(pH.LED,na.rm=T)),
    KSVMU=mean(KSV,na.rm=T),
    KSVSD=sd(KSV,na.rm=T),
    KSVCV=100*(sd(KSV,na.rm=T)/mean(KSV,na.rm=T)),
    GAINMU=mean(Gain,na.rm=T),
    GAINSD=sd(Gain,na.rm=T),
    GAINCV=100*(sd(Gain,na.rm=T)/mean(Gain,na.rm=T))
  ) %>%
    ungroup() %>%
  summarise(
    O2LED = mean(O2LEDMU,na.rm=T),
    O2CV = mean(O2LEDCV,na.rm=T),
    PHLED=mean(PHLEDMU,na.rm=T),
    PHCV=mean(PHLEDCV,na.rm=T),
    KSV=mean(KSVMU,na.rm=T),
    KSVCV=mean(KSVCV,na.rm=T),
    GAIN=mean(GAINMU,na.rm=T),
    GAINCV=mean(GAINCV,na.rm=T)
  ) %>%  collect()
}
