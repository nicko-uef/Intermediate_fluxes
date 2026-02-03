##Fig. 1, panel A: boxplot with smoothed line, filed-assessed severity classes
  ggplot(allch4[allch4$MST >= 0.1 & allch4$TREAT2 != "deep", ], aes(x = factor(MST), y = FLUX_ng)) +
    geom_boxplot(aes(fill = TREAT2)) +
    geom_smooth(aes(group = TREAT2, color = TREAT2),method = "loess",se = FALSE ) +
    scale_fill_manual(values = treat_colors,name = "Severity") +
    scale_color_manual(values = treat_colors, name = "Severity") +
    labs( x = "Months since fire (not scaled)", y = expression('Flux ng CH'[4]*' m'^-2*' s'^-1) ) +
    scale_y_continuous(limits = c(-115, 0)) +
    theme_minimal()


##Fig. 1, panel B: boxplot with smoothed line, dNBR classes
  ggplot(allch4[allch4$MST > 0.1 & allch4$TREAT2 != "deep", ], aes(x = factor(MST), y = FLUX_ng)) +
    geom_boxplot(aes(fill = NBR)) +
    geom_smooth(aes(group = NBR, color = NBR),method = "loess", se = FALSE) +
    scale_fill_manual(values = nbr_col, name = "Severity" ) +
    scale_color_manual(values = nbr_col,name = "Severity" ) +
    labs(x = "Months since fire (not scaled)",y = expression('Flux ng CH'[4]*' m'^-2*' s'^-1) ) +
    scale_y_continuous(limits = c(-115, 0)) +
    theme_minimal()

##Fig. 2, panel A
  #model used for predictions (model 6 without random effects structure)
  nbr.pred<-lm(formula = log(-1 * FLUX_ng) ~ MST + NBR + SOIL_M + SOIL_T, 
             data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ])

  #combining annual estimates to one dataframe
  flux.predict.nbr<-rbind(nbr.flux_pred0,nbr.flux_pred1) #These dataframes from script "mod_generalize"
  flux.predict.nbr<-rbind(flux.predict.nbr,nbr.flux_pred2) #These dataframes from script "mod_generalize"

  #Plotting estimates over measured data
  ggplot(allch4_oh %>% filter(MST > 0.01, TREAT != "deep"), aes(x = MST, y = FLUX_ng)) +
    geom_point(aes(color = NBR, shape = factor(SITE)), size = 2.5, alpha = 0.9) +
    scale_color_manual(values = nbr_col)+
    new_scale_color() +
    geom_line(data = flux.predict.nbr,aes(x = MST, y = FLUX_ng_s, color = NBR),linewidth = 1.2)  +
    scale_color_manual( values = treat_col,    name = "Severity") +
    geom_vline(xintercept = 0, color = "red3", linewidth = 1.5) +
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    scale_y_continuous(limits = c(-120, 10)) +
    labs(x = "Months since fire", y = expression('Flux ng CH'[4]*' m'^-2*' s'^-1),shape = "Site") +
    theme_minimal()

##Fig. 2, panel B
  #model used for predictions (model 7 without random effects structure)
  mixsev.pred<-lm(formula = log(-1 * FLUX_ng) ~ MST + TREAT2 + SOIL_M + SOIL_T, 
             data = resp_dat_clean1[resp_dat_clean1$MST >= 0.1 & resp_dat_clean1$TREAT2 != "deep", ])

  ##Estimates from generalized model calculations
  plot.mix.severity<-rbind(flux_pred0,flux_pred1)#These dataframes from script "mod_generalize"
  plot.mix.severity<-rbind(plot.mix.severity,flux_pred2)#These dataframes from script "mod_generalize"

  #Adding time since treatment (in months, MST_C) to data
  flux.predict.mix <- resp_dat_clean1 %>%
  filter(MST >= 0.1, TREAT2 != "deep") %>%
    group_by(TREAT2) %>%
    reframe(
      MST_C = seq(min(MST_C, na.rm = TRUE),
                  max(MST_C, na.rm = TRUE),
                  length.out = 200),
      MST = seq(min(MST, na.rm = TRUE),
                max(MST, na.rm = TRUE),
                length.out = 200),
      SOIL_M = median(SOIL_M, na.rm = TRUE),
      SOIL_T = median(SOIL_T, na.rm = TRUE)
    ) %>%
    tidyr::crossing(SITE = sites)

  #Adding estimates from the model into the dataframe for plotting
  flux.predict.mix$TREAT2 <- factor(flux.predict.mix$TREAT2, levels = names(treat_colors))
  flux.predict.mix$SITE   <- factor(flux.predict.mix$SITE)
  flux.predict.mix$log_neg_flux <- predict( mixsev.pred,newdata = flux.predict.mix, level = 0)
  flux.predict.mix$sev.FLUX_ng <- -1 * exp(flux.predict.mix$log_neg_flux)

  #Plotting estimates over measured data
  ggplot(allch4_sub, aes(x = MST, y = FLUX_ng, color = TREAT2, shape = factor(SITE))) +
    geom_point(size = 2.5, alpha = 0.9) +
    geom_line(data = plot.mix.severity,aes(x = MST, y = FLUX_ng_s, color = TREAT2),linewidth = 1.2,inherit.aes = FALSE) +
    geom_vline(xintercept = 0, color = "red3", linewidth = 1.5) +
    scale_color_manual(values = treat_colors) +
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    scale_y_continuous(limits = c(-120, 10)) +
    labs(x = "Months since fire",y = expression('Flux ng CH'[4]*' m'^-2*' s'^-1),color = "Severity",shape = "Site") +
    theme_minimal()

##Fig. 3, panel A
  env_pred<-cbind(moist_pred,temp_pred)
  env_pred<-env_pred[,c(1,2,3,6)]

  ggplot(allch4[allch4$MST != 0 & allch4$MST != 0.01& allch4$TREAT2 != "deep",], 
         aes(x = JULIAN, y = SOIL_T, color = TREAT2, shape = factor(SITE))) +
    geom_point(size = 2.5) +
    #geom_smooth(aes(group = TREAT2, color = TREAT2), method = "loess", linewidth=0.8, se = F) +  # Ensure loess lines match colors
    geom_line(data = env_pred,aes(x = JULIAN, y = SOIL_T, color = TREAT2),linewidth = 1.2,inherit.aes = FALSE) +
    scale_color_manual(values = treat_colors, breaks = levels(allch4$TREAT2)) +  # Corrected breaks
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    #scale_y_continuous(limits = c(0, 20)) +
    labs(x = "Julian Date", y = "Soil temperature (°C)", color = "Burn Severity", shape = "Site") +
    theme_minimal()

##Fig. 3, panel B
  env_pred<-cbind(moist_pred,temp_pred)
  env_pred<-env_pred[,c(1,2,3,6)]

  ggplot(allch4[allch4$MST != 0 & allch4$MST != 0.01 & allch4$TREAT2 != "deep",], 
       aes(x = JULIAN, y = SOIL_M, color = TREAT2, shape = factor(SITE))) +
  geom_point(size = 2.5) +
  geom_line(data = env_pred,aes(x = JULIAN, y = SOIL_M, color = TREAT2),linewidth = 1.2,inherit.aes = FALSE) +
  #geom_smooth(aes(group = TREAT2, color = TREAT2), method = "loess", linewidth=0.8, se = F) +  # Ensure loess lines match colors
  scale_color_manual(values = treat_colors, breaks = levels(allch4$TREAT2)) +  # Corrected breaks
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(x = "Julian Date", y = "Soil moisture (%)", color = "Burn Severity", shape = "Site") +
  theme_minimal()

##Fig. 4 - must run the script "OH_loss" first
ggplot(mo5.daily, aes(x = OM_LOSS, y = FLUX_ng, color = TREAT2, shape = factor(SITE))) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.4, color = "black", linetype = "solid", linewidth = .6) +  # Add vertical line at x = 0
  geom_smooth(aes(group = dummy), method = "loess", se = F) +  # Add separate loess lines for each level of "TREAT"
  geom_abline(intercept = int.lo, slope = slp.lo, color = "blue", lwd=1,linetype = "dashed") +  # Dashed reference line from model
  geom_abline(intercept = int.hi, slope = slp.hi, lwd =1,linetype = "dashed") +  # Dashed reference line from model
  scale_color_manual(values = treat_colors, breaks = levels(mo5.daily$SITE)) +  # Set colors for each level of "TREAT"
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  #scale_y_continuous(limits = c(-120, 10))+
  labs(x = "O-horizon consumption", y = expression('ng CH'[4]*' m'^-2*' s'^-1), color = "Burn Severity", shape = "Site") +
  theme_minimal()



