##Predicting soil temperature and moisture by Julian date. For seasonal predictions, use 190 days of approx. 200-230 frost-free in central Finland
  #Temperature
  julian_seq<-seq(120,310,by=1)
  temp_pred<-expand.grid(
    TREAT2=treat_levels,
    JULIAN=julian_seq
  )
 temp_pred$SOIL_T<-predict(temp.mod, newdata=temp_pred,level=0)

  #moisture
  moist_pred<-expand.grid(
    TREAT2=treat_levels,
    JULIAN=julian_seq
  )
 moist_pred$SOIL_M<-predict(moist.mod, newdata=temp_pred,level=0)  

##Creating a sequence to have a unique time since treatment value (MST) for each julian date. Assume 30.44 days per month
  MST_C0<-c(seq(-2.6,2.05,by=1/30.44))
  MST_C1<-(seq(7.7,13.95,by=1/30.44))
  MST_C2<-(seq(19.7, 25.95,by=1/30.44))
  
  MST0<-c(seq(0,4.65,by=1/30.44))
  MST1<-c(seq(10.3,16.55,by=1/30.44))
  MST2<-c(seq(22.3,28.55, by=1/30.44))

## Calculating estimated flux values using model 6 (dNBR, soil temp, soil moist, months since fire)

  #model used for estimation (model 6, with random effects removed)
  nbr.pred<-lm(formula = log(-1 * FLUX_ng) ~ MST + NBR + SOIL_M + SOIL_T, 
             data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ])

  #creating a new dataframe for the predictions/estimations
  br.newdata<-cbind(moist_pred,temp_pred)
  nbr.newdata$NBR<-as.character(nbr.newdata$TREAT2)
  nbr.newdata$NBR[nbr.newdata$NBR == "mm"] <- "lm"
  nbr.newdata$NBR[nbr.newdata$NBR == "h"]  <- "mh"
  nbr.newdata$NBR<-as.factor(nbr.newdata$NBR)

  #Adding predicted values to the new dataframe for the first post-fire growing season
  nbr.flux_pred0<-nbr.newdata[nbr.newdata$JULIAN>=169,c(3,6,7)]
  nbr.flux_pred0 <- nbr.flux_pred0[order(nbr.flux_pred0$NBR), ]
  nbr.flux_pred0$MST<-rep(MST0,4)
  nbr.flux_pred0$log_neg_flux <- predict(nbr.pred, newdata = nbr.flux_pred0, level = 0)
  nbr.flux_pred0$FLUX_ng_s <- -1 * exp(nbr.flux_pred0$log_neg_flux)
  nbr.flux_pred0$kgCH4_ha_day <- nbr.flux_pred0$FLUX_ng_s * 86400 * 1e-8

  #Adding predicted values to the new dataframe for the second post-fire growing season
  nbr.flux_pred1 <- nbr.newdata[order(nbr.newdata$NBR), ]
  nbr.flux_pred1$MST<-rep(MST1,4)
  nbr.flux_pred1<-nbr.flux_pred1[,c(3,6,7,8)]
  nbr.flux_pred1$log_neg_flux <- predict(nbr.pred, newdata = nbr.flux_pred1, level = 0)
  nbr.flux_pred1$FLUX_ng_s <- -1 * exp(nbr.flux_pred1$log_neg_flux)
  nbr.flux_pred1$kgCH4_ha_day <- nbr.flux_pred1$FLUX_ng_s * 86400 * 1e-8

  #Adding predicted values to the new dataframe for the third post-fire growing season
  nbr.flux_pred2 <- nbr.newdata[order(nbr.newdata$NBR), ]
  nbr.flux_pred2$MST<-rep(MST2,4)
  nbr.flux_pred2<-nbr.flux_pred2[,c(3,6,7,8)]
  nbr.flux_pred2$log_neg_flux <- predict(nbr.pred, newdata = nbr.flux_pred2, level = 0)
  nbr.flux_pred2$FLUX_ng_s <- -1 * exp(nbr.flux_pred2$log_neg_flux)
  nbr.flux_pred2$kgCH4_ha_day <- nbr.flux_pred2$FLUX_ng_s * 86400 * 1e-8

## Calculating estimated flux values using model 7 (field severity, soil temp, soil moist, months since fire)

  #model used for estimation (model 7, with random effects removed)
  mixsev.pred<-lm(formula = log(-1 * FLUX_ng) ~ MST + TREAT2 + SOIL_M + SOIL_T, 
                data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ])
  
  #Adding predicted values for the first post-fire growing season
  flux_pred0<-cbind(moist_pred,temp_pred)
  flux_pred0<-flux_pred0[flux_pred0$JULIAN>=169,c(1,3,6)]
  flux_pred0 <- flux_pred0[order(flux_pred0$TREAT2), ]
  flux_pred0$MST<-rep(MST0,4)
  flux_pred0$log_neg_flux <- predict(mixsev.pred, newdata = flux_pred0, level = 0)
  flux_pred0$FLUX_ng_s <- -1 * exp(flux_pred0$log_neg_flux)

  #Adding predicted values for the second post-fire growing season
  flux_pred1<-cbind(moist_pred,temp_pred)
  flux_pred1 <- flux_pred1[order(flux_pred1$TREAT2), ]
  flux_pred1$MST<-rep(MST1,4)
  flux_pred1<-flux_pred1[,c(1,3,6,7)]
  flux_pred1$log_neg_flux <- predict(mixsev.pred, newdata = flux_pred1, level = 0)
  flux_pred1$FLUX_ng_s <- -1 * exp(flux_pred1$log_neg_flux)

  #Adding predicted values for the third post-fire growing season
  flux_pred2<-cbind(moist_pred,temp_pred)
  flux_pred2 <- flux_pred2[order(flux_pred2$TREAT2), ]
  flux_pred2$MST<-rep(MST2,4)
  flux_pred2<-flux_pred2[,c(1,3,6,7)]
  flux_pred2$log_neg_flux <- predict(mixsev.pred, newdata = flux_pred2, level = 0)
  flux_pred2$FLUX_ng_s <- -1 * exp(flux_pred2$log_neg_flux)







