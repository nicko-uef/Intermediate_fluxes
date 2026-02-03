#Variance function for flux models, so that variance increases with soil moisture
var_fn<-varExp(form=~SOIL_M) 

##Testing differences between plots prior to burning
#In Leppävirta
  mod.preburnLV24<-lme(log(-1*FLUX_ng) ~ TREAT3, random = ~1 | PLOT/COLLAR, data = preburn.LV[preburn.LV$YEAR=="2024",] )
  anova(mod.preburnLV24, type="m")
  summary(mod.preburnLV24)
  r.squaredGLMM(mod.preburnLV24)
   
#In Evo
  mod.preburnEV<-lme(log(-1*FLUX_ng) ~ TREAT3, random = ~1 | PLOT/COLLAR, data = preburn.EV )
  anova(mod.preburnEV, type="m")
  summary(mod.preburnEV)
  r.squaredGLMM(mod.preburnEV)

##Testing the impact of severity on temperature and moisture
#Temperature
  temp.mod<-lme(SOIL_T~TREAT2*(JULIAN+I(JULIAN^2)),random=~1|SITE/PLOT/COLLAR,data=allch4[allch4$MST>=0.1&allch4$TREAT2!="deep",], na.action=na.exclude,
  method="REML")
  anova(temp.mod, type="m")
  summary(temp.mod)
  r.squaredGLMM(temp.mod)

  plot(residuals(temp.mod), ylim=c(-5,5));abline(0,0)

  #check random effects structure
  temp.mod_simple<-update(temp.mod,random=~1|PLOT/COLLAR )
  anova(temp.mod,temp.mod_simple)

#Moisture
  moist.mod<-lme(SOIL_M~TREAT2+JULIAN+I(JULIAN^2),random=~1|PLOT/COLLAR,data=allch4[allch4$MST>=0.1&allch4$TREAT2!="deep",],na.action=na.exclude,
  method="REML")
  anova(moist.mod, type="m") #significant:
  summary(moist.mod)
  plot(residuals(moist.mod));abline(0,0) 
  r.squaredGLMM(moist.mod)

  #check random effects structure
  moist.mod_simple<-update(moist.mod,random=~1|PLOT/COLLAR )
  anova(moist.mod,moist.mod_simple)

##Testing the impact of canopy mortality, remaining o-horizon thickness, soil temperature,and moisture on CH4 fluxes
  mod.ch4_eco <- lme(log(-1 * FLUX_ng) ~ OH_AFTER+MORT+ MST + SOIL_M + SOIL_T,
               random = ~1 | PLOT/COLLAR,
               data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ], 
               method = "REML")
  anova(mod.ch4_eco)
  summary(mod.ch4_eco)
  r.squaredGLMM(mod.ch4_eco)

  #check random effects structure
  eco_mod_simple<-update(mod.ch4_eco,random = ~1 | PLOT/COLLAR)
  anova(mod.ch4_eco, eco_mod_simple) #this shows that SITE is *barely* insignificant (o=0.0673)

  #check the residuals
  plot(residuals(mod.ch4_eco));abline(0,0) 
  qqnorm(residuals(mod.ch4_eco));qqline(residuals(mod.ch4_eco))
  hist(residuals(mod.ch4_eco), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

##Testing the impact of field-assessed severity (TREAT2) on CH4 fluxes
  ch4_mod <- lme(log(-1 * FLUX_ng) ~ TREAT2 + MST_C ,
                 random = ~1 | PLOT/COLLAR,
                 data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ],
                 weights = var_fn,
                 method = "REML") 
  anova(ch4_mod)
  summary(ch4_mod)
  r.squaredGLMM(ch4_mod)

  #check random effects structure
  ch4_mod_simple<-update(ch4_mod,random = ~1 | PLOT/COLLAR)
  anova(ch4_mod, ch4_mod_simple)
  
  #check the residuals
  plot(residuals(ch4_mod));abline(0,0) 
  qqnorm(residuals(ch4_mod));qqline(residuals(ch4_mod))
  hist(residuals(ch4_mod), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

##Testing the impact of dNBR on CH4 fluxes
  mod_ch4.nbr<- lme(log(-1 * FLUX_ng) ~ NBR + MST,
                  random = ~1 | SITE/PLOT/COLLAR,
                  data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ],
                  weights = var_fn, na.action = na.omit,
                  method = "REML")
  anova(mod_ch4.nbr)
  summary(mod_ch4.nbr)
  r.squaredGLMM(mod_ch4.nbr)

  #check random effects structure
  nbr_mod_simple<-update(mod_ch4.nbr,random = ~1 | PLOT/COLLAR)
  anova(mod_ch4.nbr, nbr_mod_simple)

  #check the residuals
  plot(residuals(mod_ch4.nbr));abline(0,0) 
  qqnorm(residuals(mod_ch4.nbr));qqline(residuals(mod_ch4.nbr))
  hist(residuals(mod_ch4.nbr), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

##Finding the best model, starting with dNBR
  #include all relevant terms
  ch4_mod.mix <- lme(log(-1 * FLUX_ng) ~ NBR + MST + MORT+OH_AFTER+SOIL_M + SOIL_T+I(SOIL_T^2) ,
                 random = ~1 | SITE/PLOT/COLLAR,
                 data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ],
                 weights = var_fn,
                 method = "REML") 
  anova(ch4_mod.mix)
  summary(ch4_mod.mix)

  #Drop non-significant terms
  ch4_mod.mix <- lme(log(-1 * FLUX_ng) ~ NBR + MST + SOIL_M + SOIL_T ,
                 random = ~1 | SITE/PLOT/COLLAR,
                 data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ],
                 weights = var_fn,
                 method = "REML") 
  anova(ch4_mod.mix)
  summary(ch4_mod.mix)
  r.squaredGLMM(ch4_mod.mix)
 
 #check random effects structure
  nbrmix_mod_simple<-update(ch4_mod.mix,random = ~1 | PLOT/COLLAR)
  anova(ch4_mod.mix, nbrmix_mod_simple)

  #check the residuals
  plot(residuals(ch4_mod.mix));abline(0,0) 
  qqnorm(residuals(ch4_mod.mix));qqline(residuals(ch4_mod.mix))
  hist(residuals(ch4_mod.mix), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

##Finding the best model, field-assessed severity (TREAT2)
  #include all relevant terms
  sev_mod.mix <- lme(log(-1 * FLUX_ng) ~ TREAT2 + MST + MORT+OH_AFTER+SOIL_M + SOIL_T+I(SOIL_T^2) ,
                 random = ~1 | SITE/PLOT/COLLAR,
                 data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ],
                 weights = var_fn,
                 method = "REML") 
  anova(sev_mod.mix)
  summary(sev_mod.mix)

  #Drop non-significant terms
  sev_mod.mix <- lme(log(-1 * FLUX_ng) ~ TREAT2 + MST + SOIL_M + SOIL_T ,
                 random = ~1 | PLOT/COLLAR,
                 data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ],
                 weights = var_fn,
                 method = "REML") 
  anova(sev_mod.mix)
  summary(sev_mod.mix)
  r.squaredGLMM(sev_mod.mix)
 
 #check random effects structure
  sevmix_mod_simple<-update(sev_mod.mix,random = ~1 | PLOT/COLLAR)
  anova(sev_mod.mix, sevmix_mod_simple)

  #check the residuals
  plot(residuals(sev_mod.mix));abline(0,0) 
  qqnorm(residuals(sev_mod.mix));qqline(residuals(sev_mod.mix))
  hist(residuals(sev_mod.mix), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")







