##These packages are required----
library(FluxCalR)
library(fluxible)
library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(assertthat)
library(magrittr)
library(birk)
library(plotly)
library(htmlwidgets)
library(devtools)
library(ggplot2)
library(nlme)
library(emmeans)
library(colorRamps)
library(patchwork)
library(MuMIn)
library(tidyr)
library(purrr)
library(ggpmisc)
library(ggrepel)
library(ggnewscale)
library(lme4)

##Running the analyses----
# managing data structure
source("Script/data_management.R")

#mixed-effects models used
source("Script/models.R")

# generalizing models to estimate temperature, moisture, and fluxes
source("Script/mod_generalize.R")

# testing the O-horizon loss data for a significant breakpoint
source("Script/OH_loss.R")

#creating figures
source("Script/Figures.R")



