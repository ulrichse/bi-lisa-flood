
library(dplyr)

data <- read.csv("data/nc_acs_data_flood.csv")

crosswalk <- read.csv("data/crosswalk.csv")

crosswalk <- crosswalk %>%
  filter(USPS_ZIP_PREF_STATE=="NC")

ruca <- read.csv("data/RUCA.csv")

ruca <- ruca %>%
  filter(STATE=="NC")

write.csv(ruca, "data/RUCA.csv")

crosswalk <- crosswalk %>%
  rename(FIPS=TRACT)

data_join <- data %>%
  left_join(crosswalk, by=c('FIPS'))
