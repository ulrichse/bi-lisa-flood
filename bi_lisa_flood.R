
data <- read_sf("data/ncdhhsflood.shp")

#Create single risk category-----------------------------------------------------

data <- data %>%
  mutate(max_variable = case_when(
    min_risk >= moderate_r & min_risk >= major_risk & min_risk >= sev_risk & min_risk >= extreme_ri ~ "min",
    moderate_r >= min_risk & moderate_r >= major_risk & moderate_r >= sev_risk & moderate_r >= extreme_ri ~ "moderate",
    major_risk >= min_risk & major_risk >= moderate_r & major_risk >= sev_risk & major_risk >= extreme_ri ~ "major",
    sev_risk >= min_risk & sev_risk >= moderate_r & sev_risk >= major_risk & sev_risk >= extreme_ri ~ "severe",
    TRUE ~ "extreme"  # If none of the conditions are met, assign "NA"
  ))

order_levels <- c("min", "moderate", "major", "severe", "extreme")
data$max_variable <- factor(data$max_variable, levels = order_levels)
table(data$max_variable)

#Create quick thematic map (qtm) using tmap package
qtm(data, "extreme_ri")

#Create ICE categories-----------------------------------------------------------

data$GEOID <- as.numeric(data$GEOID)
ice <- read.csv("data/ICE_2021_tract.csv")
ice <- ice %>%
  rename(GEOID=FIPS)


data <- data %>%
  left_join(ice, by=c('GEOID'))

data$ice_income<-as.numeric(data$ab_ice)
data$ice_race<-as.numeric(data$cd_ice)
data$ice_income_race<-as.numeric(data$ef_ice)

data<-data %>%
  mutate(ice_race_tertiles = ntile(ice_race, 3)) %>%
  mutate(ice_race_tertiles = if_else(ice_race_tertiles == 1, 'Low', if_else(ice_race_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(ice_race_tertiles)
table(data$ice_race_tertiles)##QC

data<-data %>%
  mutate(ice_income_tertiles = ntile(ice_income, 3)) %>%
  mutate(ice_income_tertiles = if_else(ice_income_tertiles == 1, 'Low', if_else(ice_income_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(ice_income_tertiles)
table(data$ice_income_tertiles)##QC

data<-data %>%
  mutate(ice_income_race_tertiles = ntile(ice_income_race, 3)) %>%
  mutate(ice_income_race_tertiles = if_else(ice_income_race_tertiles == 1, 'Low', if_else(ice_income_race_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(ice_income_race_tertiles)
table(data$ice_income_race_tertiles)

qtm(data, "ice_income_race_tertiles")

#Bivariate LISA------------------------------------------------------------------

library(spdep)

nb <- poly2nb(data, queen = TRUE)  # Use queen contiguity, adjust if needed
W <- nb2listw(nb, style = "W")

bivariate_lisa <- localmoran(data$

