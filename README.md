# Intermediate_fluxes
R script for analyses of post-fire ch4 fluxes and related figures. Contains data management, linear mixed-effects models, calculating generalized estimates of the models, and testing the influence of the depth of o-horizon consumed by fire.

Data required for these analyses are available at:
http://doi.org/

Glossary of variable names:
Date: date of measurement as YYYY-MM-DD
Start: time at the start of the measurement in HH:MM:SS
End: time at the end of the measurement in HH:MM:SS
Gas: which gas was measured: CH4 or CO2
Slope: calculated slope of the best-fit linear regression estimating the change in gas concentration over time during the measurement period
R2: coefficient of determination of the linear regression estimating the change in gas concentration over time during the measurement period
Ta: chamber air temperature (default of 25)
Flux: Calculated change in gas concentration over time in mmol/m²/s
Plot: identifier of the plot where the measurement was completed
TREAT: initial assessment of fire severity where ct=unburned, lo = low severity, hi = moderately high, ll = moderately low, and deep = 40cm deep collars with vegetation removed in unburned areas
MST: months since treatment. number of months (to one decimal) that have elapsed between the prescribed burn and the date of measurement. negative values indicate pre-burn measurements
AIR_PR: atmospheric air pressure (kPa) on the Day of measurement
SOIL_M: soil moisture at 5 cm depth measured as volumetric water content (%)
SOIL_T: soil temperature at 10 cm depth (°C)
AREA: Area of base of the cylindrical chamber (m²)
VOL: chamber volume including collar height (L)
FLUX_ng: Calculated change in gas concentration over time in ng/m²/s
YEAR: year of measurement
SITE: which study site the measurement was conducted at
COLLAR: identifier unique to each collar
TREAT2: fire severity based on field-assessment combining organic gorizon consumption and tree mortality. where ct=unburned, ll = low severity, h = moderately high, mm = moderately low, and deep = 40cm deep collars with vegetation removed in unburned areas
NBR: difference Normalized Burn Index (dNBR) fire severity class, where ct=unburned, lm = low severity, mh = moderately high, and lm = moderately low
OM_LOSS: depth of organic horizon (litter and humus combined) consumed by the fire (cm)
MORT: overstorey tree canopy mortality 1 year after fire (%)
OH_AFTER: depth of organic horizon (litter and humus combined) remaining after the fire (cm)
JULIAN: Julian date on which the measurement was conducted
SITETRT: interaction of study site and burn severity (from TREAT2)

