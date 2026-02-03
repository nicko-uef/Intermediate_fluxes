## preparing the data to include only the first season (5 months) after fire----

mo5.daily<-resp_dat_clean[resp_dat_clean$MST > 0.01 & resp_dat_clean$MST <6 & resp_dat_clean$TREAT2 != "deep", ]
mo5.daily$dummy<-rep("y", length(mo5.daily$FLUX_ng))
mo5.daily<-mo5.daily[mo5.daily$TREAT2!="ct",]
mo5.daily<-mo5.daily[mo5.daily$FLUX_ng>-89,] #remove 3 outliers

##setting up dataframes with breakpoints at different depths of OH loss----
mo5.daily$OM_lo2<-mo5.daily$OM_LOSS
mo5.daily$OM_lo2[mo5.daily$OM_lo2>0.2]<-0.2

mo5.daily$OM_hi2<-mo5.daily$OM_LOSS
mo5.daily$OM_hi2[mo5.daily$OM_hi2<0.2]<-0.2

mo5.daily$OM_lo3<-mo5.daily$OM_LOSS
mo5.daily$OM_lo3[mo5.daily$OM_lo3>0.3]<-0.3

mo5.daily$OM_hi3<-mo5.daily$OM_LOSS
mo5.daily$OM_hi3[mo5.daily$OM_hi3<0.3]<-0.3

mo5.daily$OM_lo4<-mo5.daily$OM_LOSS
mo5.daily$OM_lo4[mo5.daily$OM_lo4>0.4]<-0.4

mo5.daily$OM_hi4<-mo5.daily$OM_LOSS
mo5.daily$OM_hi4[mo5.daily$OM_hi4<0.4]<-0.4

mo5.daily$OM_lo5<-mo5.daily$OM_LOSS
mo5.daily$OM_lo5[mo5.daily$OM_lo5>0.5]<-0.5

mo5.daily$OM_hi5<-mo5.daily$OM_LOSS
mo5.daily$OM_hi5[mo5.daily$OM_hi5<0.5]<-0.5

mo5.daily$OM_lo6<-mo5.daily$OM_LOSS
mo5.daily$OM_lo6[mo5.daily$OM_lo6>0.6]<-0.6

mo5.daily$OM_hi6<-mo5.daily$OM_LOSS
mo5.daily$OM_hi6[mo5.daily$OM_hi6<0.6]<-0.6


##run a model at each break point of depth of OH loss to check fit and significance----
mod.somall2<-lme(log(-1 * FLUX_ng) ~SOIL_M+SOIL_T + I(SOIL_T^2)+OM_lo2+OM_hi2,
                 random = ~1 | SITE/PLOT/COLLAR,
                 data = mo5.daily,
                 method = "REML")

plot(residuals(mod.somall2)~mo5.daily$OM_LOSS)
lines(loess.smooth(mo5.daily$OM_LOSS,residuals(mod.somall2),span=2/5))
qqnorm(residuals(mod.somall2));qqline(residuals(mod.somall2))
hist(residuals(mod.somall2), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")


anova(mod.somall2, type="m")
summary(mod.somall2)

#0.3 cm AIC: 461, above and below 0.3 cm not significant (above barely)  
  mod.somall3<-lme(log(-1 * FLUX_ng) ~SOIL_M+SOIL_T + I(SOIL_T^2)+OM_lo3+sqrt(OM_hi3),
                random = ~1 | PLOT/COLLAR,
                data = mo5.daily,
                method = "REML")
  
  plot(residuals(mod.somall3)~mo5.daily$OM_LOSS)
  lines(loess.smooth(mo5.daily$OM_LOSS,residuals(mod.somall3),span=2/5))
  qqnorm(residuals(mod.somall3));qqline(residuals(mod.somall3))
  hist(residuals(mod.somall3), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")
  

  anova(mod.somall3, type="m")
  summary(mod.somall3)

#0.4 cm. AIC 461. below 0.4 cm not significant, but above yes
  mod.somall4<-lme(log(-1 * FLUX_ng) ~SOIL_M+SOIL_T + I(SOIL_T^2)+OM_lo4+sqrt(OM_hi4),
                   random = ~1 | SITE/PLOT/COLLAR,
                   data = mo5.daily,
                   method = "REML")
  
  plot(residuals(mod.somall4)~mo5.daily$OM_LOSS)
  lines(loess.smooth(mo5.daily$OM_LOSS,residuals(mod.somall4),span=2/5))
  qqnorm(residuals(mod.somall4));qqline(residuals(mod.somall4))
  hist(residuals(mod.somall4), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")
  
  anova(mod.somall4, type="m")
  summary(mod.somall4)
  
  #checking different shapes. sqrt lo: worse. lo squared: worse. hi linear: slightly worse. remove lo: hi loses significance
  mod.somall4a<-lme(log(-1 * FLUX_ng) ~SOIL_M+SOIL_T + I(SOIL_T^2)+OM_lo4+sqrt(OM_hi4),
                   random = ~1 | SITE/COLLAR,
                   data = mo5.daily,
                   method = "REML")
  
  plot(residuals(mod.somall4)~mo5.daily$OM_LOSS)
  lines(loess.smooth(mo5.daily$OM_LOSS,residuals(mod.somall4),span=2/5))
  qqnorm(residuals(mod.somall4));qqline(residuals(mod.somall4))
  hist(residuals(mod.somall4), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")
  
  anova(mod.somall4a, type="m")
  summary(mod.somall4a)
  

#0.5 cm. AIC 462. below 0.5 cm definitely not significant, above not either
  mod.somall5<-lme(log(-1 * FLUX_ng) ~SOIL_M++SOIL_T + I(SOIL_T^2)+OM_lo5+sqrt(OM_hi5),
                   random = ~1 | SITE/PLOT/COLLAR,
                   data = mo5.daily,
                   method = "REML")
  
  plot(residuals(mod.somall5)~mo5.daily$OM_LOSS)
  lines(loess.smooth(mo5.daily$OM_LOSS,residuals(mod.somall5),span=2/5))
  qqnorm(residuals(mod.somall5));qqline(residuals(mod.somall5))
  hist(residuals(mod.somall5), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")
  
  anova(mod.somall5, type="m")
  summary(mod.somall5)

#0.6 cm. AIC 462. neither 
  mod.somall6<-lme(log(-1 * FLUX_ng) ~SOIL_M++SOIL_T + I(SOIL_T^2)+OM_lo6+sqrt(OM_hi6),
                   random = ~1 | SITE/PLOT/COLLAR,
                   data = mo5.daily,
                   method = "REML")
  
  plot(residuals(mod.somall6)~mo5.daily$OM_LOSS)
  lines(loess.smooth(mo5.daily$OM_LOSS,residuals(mod.somall6),span=2/5))
  qqnorm(residuals(mod.somall6));qqline(residuals(mod.somall6))
  hist(residuals(mod.somall6), breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")
  
  anova(mod.somall6, type="m")
  summary(mod.somall6)

## Extracting model intercepts and slopes for best fit of OH loss data (for Fig. 4)----

mod.lo<-lm(FLUX_ng~OM_LOSS, data= mo5.daily[mo5.daily$OM_LOSS<=0.4,])
summary(mod.lo)
int.lo<-coef(mod.lo)[1]
slp.lo<-coef(mod.lo)[2]

mod.hi<-lm(FLUX_ng~OM_LOSS, data= mo5.daily[mo5.daily$OM_LOSS>0.4,])
summary(mod.hi)
int.hi<-coef(mod.hi)[1]
slp.hi<-coef(mod.hi)[2]

