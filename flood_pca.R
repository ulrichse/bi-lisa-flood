
#DataCamp Tutorial: https://www.datacamp.com/tutorial/pca-analysis-r

library(dplyr)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

data <- read_sf("data/ncdhhsflood.shp")

data2 <- st_drop_geometry(data)

data_num <- data2 %>% #Extract numeric columns of interest
  select(c(pct_u5, pct_u18, pct_over_65, med_age, pct_wht, pct_blk, pct_asian, pct_hispan, pct_no_hs, pct_unempl, mhi, pct_18_64_, pct_rent_cb, pct_hh_ss, pct_65_pov, pct_fam_pov, med_home_v, med_rent, pct_no_car, no_health_ins, per_no_int, per_no_com, pct_mh, pct_rent))
         
data_num[is.na(data_num)]<-0

data_normalized <- scale(data_num) #Normalize

head(data_num)

corr_matrix <- cor(data_num) #Compute the correlation matrix
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix) #Compute the PCA
summary(data.pca) #Show result

data.pca$loadings #Loading matrix

fviz_eig(data.pca, addlabels = TRUE) #Scree Plot

fviz_pca_var(data.pca, col.var = "black") #Biplot of attributes

fviz_cos2(data.pca, choice = "var", axes=2) #Determine how much each variable is represented in a givencomponent
                   
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE) #Biplot combined with cos2

eigenvalues <- data.pca$sdev^2

# View the eigenvalues
print(eigenvalues)




