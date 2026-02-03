##create a "master" data frame of all fluxes
  AllFlux<-rbind(fluxdat_LV23,fluxdat_RU22)
  AllFlux<-rbind(AllFlux,fluxdat_RU23)
  AllFlux<-rbind(AllFlux,fluxdat_RU24)
  AllFlux<-rbind(AllFlux,fluxdat_EV20)
  AllFlux<-rbind(AllFlux,fluxdat_LV24)
  AllFlux<-rbind(AllFlux,fluxdat_LV25)
  
#separating CH4
  allch4<-subset(AllFlux, Gas=="CH4")

#Gasses by site (years combined)
  ch4_RU<-rbind(fluxdat_RU22[fluxdat_RU22$Gas=="CH4",], fluxdat_RU23[fluxdat_RU23$Gas=="CH4",])
  ch4_RU<-rbind(ch4_RU,fluxdat_RU24[fluxdat_RU24$Gas=="CH4",])
  ch4_RU<-ch4_RU[-79,]
  ch4_LV<-rbind(fluxdat_LV23[fluxdat_LV23$Gas=="CH4",],fluxdat_LV24[fluxdat_LV24$Gas=="CH4",])
  ch4_LV<-rbind(ch4_LV,fluxdat_LV25[fluxdat_LV25$Gas=="CH4",])
  ch4_EV<-fluxdat_EV20[fluxdat_EV20$Gas=="CH4",]

#model response data. unnecessary columns removed as well as 2 data points with soil moisture missing
  resp_dat<-subset(allch4, complete.cases(SOIL_M),select=c(Date, Gas, Flux, PLOT,COLLAR,TREAT,MST,OM_LOSS,OH_AFTER,MORT,SOIL_M,SOIL_T,FLUX_ng,YEAR,SITE,JULIAN,TREAT2,SITETRT,NBR)) #remove unecessay columns and NA values
  resp_dat_clean <- na.omit(resp_dat[,c("FLUX_ng", "SITE","TREAT2","NBR", "MST","OM_LOSS","OH_AFTER","MORT", "SOIL_M", "SOIL_T", "YEAR","JULIAN", "PLOT", "COLLAR")])
  resp_dat_clean$MST_C <- scale(resp_dat_clean$MST, center = TRUE, scale = FALSE)
  resp_dat_clean1<-resp_dat_clean[-c(167,323,397,170,749),] #remove some outliers based on residuals of initial run (see below)##
  resp_dat_clean1 <- droplevels(resp_dat_clean1[resp_dat_clean1$TREAT2 != "deep", ])

##For control plots
  #Ruunaa, all years
  ctr_ru22<-fluxdat_RU22[fluxdat_RU22$TREAT=="ct"&fluxdat_RU22$Gas=="CH4",]
  ctr_ru23<-fluxdat_RU23[fluxdat_RU23$TREAT=="ct"&fluxdat_RU23$Gas=="CH4",]
  ctr_ru24<-fluxdat_RU24[fluxdat_RU24$TREAT=="ct"&fluxdat_RU24$Gas=="CH4",]
  ctr_ru<-rbind(ctr_ru22,ctr_ru23, ctr_ru24)
  
  #Leppävirta, both years
  ctr_lv<-rbind(fluxdat_LV23[fluxdat_LV23$TREAT=="ct"&fluxdat_LV23$Gas=="CH4",],fluxdat_LV24[fluxdat_LV24$TREAT=="ct"&fluxdat_LV24$Gas=="CH4",])
  ctr_lv<-rbind(ctr_lv,fluxdat_LV25[fluxdat_LV25$TREAT=="ct"&fluxdat_LV25$Gas=="CH4",])
  
  #Evo, all years (2020 only)
  ctr_ev<-fluxdat_EV20[fluxdat_EV20$TREAT=="ct"&fluxdat_EV20$Gas=="CH4",]
  
  #All sites and years
  ctr_ev20<-fluxdat_EV20[fluxdat_EV20$TREAT=="ct"&fluxdat_EV20$Gas=="CH4",]
  ctr_ev20$Date<-as.character(ctr_ev20$Date)
  
  ch4_ctr<-rbind(ctr_lv,ctr_ru)
  ch4_ctr<-rbind(ch4_ctr,ctr_ev20)

##Pre-burn comparisons
  preburn.EV<-ch4_EV[ch4_EV$MST<0|ch4_EV$MST<0.5&ch4_EV$TREAT2=="ct",]
  preburn.LV<-ch4_LV[ch4_LV$MST<0&ch4_LV$TREAT2!="deep",]
  
  preburn.EV$TREAT3<-ifelse(preburn.EV$PLOT=="c1"|preburn.EV$PLOT=="c2"|preburn.EV$PLOT=="c3"|preburn.EV$PLOT=="c4","ct","brn")
  preburn.LV$TREAT3<-ifelse(preburn.LV$PLOT=="boff4"|preburn.LV$PLOT=="boff5","ct","brn")

 treat_levels<-c("ct","ll","mm","h")

  
#### For Graphics ####
  treat_colors <- c("ct" = "steelblue3", "ll" = "gold", "mm"="orange3","h" = "black", "deep"="black")
  treat_col <- c("steelblue3","gold", "orange3","black","black")
  nbr_col<- c("ct" = "steelblue3", "ll" = "gold", "lm"="orange3","mh" = "black", "deep"="black")
