library(sf)
library(dplyr)
library(tmap)

#read in ff data 

natdata <- read_sf("data/us data/ff_nat.shp")

natdata <- natdata%>%
  rename(high_risk=hgh_rsk)%>%
  rename(low_risk=low_rsk)%>%
  rename(mid_risk=mid_rsk)

se_states <- c('FL','GA','NC','SC','LA','MS','AR','AL','TN','KY','VA','WV') #Restrict to Southeast
se_data <- natdata %>%
  filter(STATE_A %in% se_states)

se_data <- se_data %>%
  mutate(high_risk_tertiles = ntile(high_risk, 3)) %>%
  mutate(high_risk_tertiles = if_else(high_risk_tertiles == 1, 'Low', if_else(high_risk_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(high_risk_tertiles)
table(se_data$high_risk_tertiles)

se_tracts <- read.csv("southeast_tracts.csv")

#Read in ICE---------------------------------------------------------------------
ice <- read.csv("data/ICE_2021_natl_tract.csv")

ice <- ice %>%
  select(-fips)
ice <- ice %>%
  rename(fips=Geo_FIPS)

se_data <- se_data %>%
  left_join(ice, by=c('fips'))

se_data$ice_income <- as.numeric(se_data$ice_income)
se_data$ice_income_blk <- as.numeric(se_data$ice_income_blk)
se_data$ice_income_race_w_hispanic <- as.numeric(se_data$ice_income_race_w_hispanic)
se_data$ice_nonwht <- as.numeric(se_data$ice_nonwht)
se_data$ice_race_blk <- as.numeric(se_data$ice_race_blk)

se_data$ice_income_inverse <- -(se_data$ice_income)
se_data$ice_income_blk_inverse <- -(se_data$ice_income_blk)
se_data$ice_income_race_w_hispanic_inverse <- -(se_data$ice_income_race_w_hispanic)
se_data$ice_nonwht_inverse <- -(se_data$ice_nonwht)
se_data$ice_race_blk_inverse <- -(se_data$ice_race_blk)

#LISA----------------------------------------------------------------------------

install.packages("rgeoda")
library(rgeoda)
library(sf)
library(ggplot2)
library(spdep)
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

se_data <- na.exclude(se_data)

queen_weights(se_data, order=1, include_lower_order = FALSE, precision_threshold = 0)
queen_w <- queen_weights(se_data)
summary(queen_w)

#Try to write a for loop...

ice_vars <- c("ice_income_inverse", "ice_income_blk_inverse", "ice_income_race_w_hispanic_inverse", "ice_nonwht_inverse", "ice_race_blk_inverse")

for (i in seq_along(ice_vars)) {
  lisa <- local_bimoran(queen_w, se_data[c(ice_vars[i], 'high_risk')])  # Fix: use ice_vars[i]
  lms <- lisa_values(lisa)
  lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)
  lisa_p <- lisa_pvalues(lisa)
  p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
  lisa_data <- data.frame(lisa_clusters, lisa_p)
  lisa_data <- cbind(lisa_data, se_tracts)
  file_path <- file.path("C:/Users/ulrichse/OneDrive - Appalachian State University/Documents/RStudio/NCDHHS Flood/lisa outputs/southeast", paste0(ice_vars[i], "_lisa.xlsx"))
  write.xlsx(lisa_data, file_path, sheetName = "Sheet1")
}



plot(st_geometry(se_data), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#ffffff", lwd=1)
title(main = "Local Moran Map of High Risk + ICE Race (Nonwhite)")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")

lisa_p <- lisa_pvalues(lisa)
p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")
plot(st_geometry(se_data), 
     col=sapply(lisa_p, function(x){
       if (x <= 0.001) return(p_colors[4])
       else if (x <= 0.01) return(p_colors[3])
       else if (x <= 0.05) return (p_colors[2])
       else return(p_colors[1])
     }), 
     border = "#333333", lwd=0.2)
title(main = "Local Moran Map of High Risk + ICE Race")
legend('right', legend = p_labels, fill = p_colors, border = "#eeeeee")

breaks <- c(-Inf, 0.001, 0.01, 0.05, Inf)
labels <- c("p < 0.001", "p < 0.01", "p < 0.05","Not significant")  # These labels correspond to p_colors
lisap <- cut(lisa_p, breaks = breaks, labels = labels)
lisap <- as.numeric(lisap)

#Write off results and combine with tract file for arcgis mapping----------------

se_tracts <- st_drop_geometry(se_data)
se_tracts <- se_tracts %>%
  select(fips, STATE_A, Geo_TRACT)
write.csv(se_tracts, "southeast_tracts.csv")

southeast_lisa_income_race_w_hispanic <- data.frame(lisa_clusters, lisa_p)
write.csv(southeast_lisa_income_race_w_hispanic, "southeast_lisa_income_race_w_hispanic.csv")


#To plot clusters using ggplot---------------------------------------------------

ggplot(se_data) +
  geom_sf(aes(fill = factor(lisa_clusters), color = factor(lisa_clusters))) +
  scale_fill_manual(values = lisa_colors, name="Clusters", labels(factor(lisa_labels))) +
  scale_color_manual(values = lisa_colors, name="Clusters",labels(factor(lisa_labels))) +
  labs(title = "Local Moran Map of High Risk + ICE Income-Race") +
  theme_minimal()

ggplot(se_data) +
  geom_sf(aes(fill = factor(lisap), color = factor(lisap))) +
  scale_fill_manual(values = p_colors, labels=labels, name="p-values") +
  scale_color_manual(values = p_colors, labels=labels, name="p-values") +
  labs(title = expression(bold("Local Moran Map of High Risk + ICE Income-Race"))) +
  theme_minimal() 












