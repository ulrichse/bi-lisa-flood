
library(sf)
library(dplyr)
library(tmap)

#read in ff data 

natdata <- read_sf("data/us data/ff_nat.shp")
ice <- read.csv("data/ICE_2021_natl_tract.csv")

ice <- ice %>%
  select(-fips)
ice <- ice %>%
  rename(fips=Geo_FIPS)

natdata <- natdata %>%
  left_join(ice, by=c('fips'))

natdata$ice_income <- as.numeric(natdata$ice_income)
natdata$ice_income_blk <- as.numeric(natdata$ice_income_blk)
natdata$ice_income_race_w_hispanic <- as.numeric(natdata$ice_income_race_w_hispanic)
natdata$ice_nonwht <- as.numeric(natdata$ice_nonwht)
natdata$ice_race_blk <- as.numeric(natdata$ice_race_blk)

natdata$ice_income_inverse <- -(natdata$ice_income)
natdata$ice_income_blk_inverse <- -(natdata$ice_income_blk)
natdata$ice_income_race_w_hispanic_inverse <- -(natdata$ice_income_race_w_hispanic)
natdata$ice_nonwht_inverse <- -(natdata$ice_nonwht)
natdata$ice_race_blk_inverse <- -(natdata$ice_race_blk)

#LISA----------------------------------------------------------------------------
install.packages("rgeoda")
library(rgeoda)
library(sf)

natdata <- na.exclude(natdata)

nat_tracts <- st_drop_geometry(natdata)
nat_tracts <- nat_tracts %>%
  select(fips, STATE_A, Geo_TRACT)

queen_weights(natdata, order=1, include_lower_order = FALSE, precision_threshold = 0)
queen_w <- queen_weights(natdata)
summary(queen_w)

#Try to write a for loop...

ice_vars <- c("ice_income_inverse", "ice_income_blk_inverse", "ice_income_race_w_hispanic_inverse", "ice_nonwht_inverse", "ice_race_blk_inverse")

for (i in seq_along(ice_vars)) {
  lisa <- local_bimoran(queen_w, natdata[c(ice_vars[i], 'hgh_rsk')])  # Fix: use ice_vars[i]
  lms <- lisa_values(lisa)
  lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)
  lisa_p <- lisa_pvalues(lisa)
  p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
  lisa_data <- data.frame(lisa_clusters, lisa_p)
  lisa_data <- cbind(lisa_data, nat_tracts)
  file_path <- file.path("C:/Users/ulrichse/OneDrive - Appalachian State University/Documents/RStudio/NCDHHS Flood/lisa outputs/national", paste0(ice_vars[i], "_lisa.xlsx"))
  write.xlsx(lisa_data, file_path, sheetName = "Sheet1")
}




plot(st_geometry(natdata), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#ffffff", lwd=1)
title(main = "Local Moran Map of High Risk + ICE Black")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")

lisa_p <- lisa_pvalues(lisa)
p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")
plot(st_geometry(natdata), 
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

lisa_income_race_w_hispanic <- data.frame(lisa_clusters, lisa_p)
write.csv(lisa_income_race_w_hispanic, "lisa_income_race_w_hispanic.csv")


#To plot clusters using ggplot---------------------------------------------------

ggplot(natdata) +
  geom_sf(aes(fill = factor(lisa_clusters), color = factor(lisa_clusters))) +
  scale_fill_manual(values = lisa_colors, name="Clusters", labels(factor(lisa_labels))) +
  scale_color_manual(values = lisa_colors, name="Clusters",labels(factor(lisa_labels))) +
  labs(title = "Local Moran Map of High Risk + ICE Income-Race") +
  theme_minimal()

ggplot(natdata) +
  geom_sf(aes(fill = factor(lisap), color = factor(lisap))) +
  scale_fill_manual(values = p_colors, labels=labels, name="p-values") +
  scale_color_manual(values = p_colors, labels=labels, name="p-values") +
  labs(title = expression(bold("Local Moran Map of High Risk + ICE Income-Race"))) +
  theme_minimal() 
