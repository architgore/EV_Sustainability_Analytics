---
title: "Find InMap Input"
format: html
editor: visual
---

```{r}
nox<-read.csv("OUTPUT_nox_ercot_byplant.csv")
so2<-read.csv("OUTPUT_so2_ercot_byplant.csv")

#find average nox & so2 coefficients for each plant

nox2 <- nox %>%
  
  group_by(plant_id_eia) %>%
  summarize(average_nox=mean(coef.m1.))
  

so2_new <- so2 %>%
  
  group_by(plant_id_eia) %>%
  summarize(average_so2=mean(coef.m2.))


#add another 2 columns that indicates the latitude and longitude for each plant

longlat<-read.csv("Long:Lat Data.csv")

longlat2<- longlat %>% 
  
    select(X.1, X.8, X.9 ) %>%
    rename(plant_id_eia = X.1) %>%
    rename(Latitude = X.8, Longitude= X.9) 
    
longlat3 <- longlat2[-c(1),]

finaldf_nox <- merge(longlat3,nox2, by="plant_id_eia")    
finaldf_so2 <- merge(longlat3,so2_new, by="plant_id_eia")    

```

```{r}
library("tidyverse")
nox<-read.csv("OUTPUT_nox_ercot_byplant.csv")
so2<-read.csv("OUTPUT_so2_ercot_byplant.csv")
nox_east<-read.csv("OUTPUT_byplant_nox_east_byplant.csv")
nox_ercot<-read.csv("OUTPUT_byplant_nox_ercot_byplant.csv")
nox_wecc<-read.csv("OUTPUT_byplant_nox_wecc_byplant.csv")
nox_wecc2<-read.csv("OUTPUT_byplant_nox_wecc_byplant2.csv")
nox_wecc3<-read.csv("OUTPUT_byplant_nox_wecc_byplant3.csv")
so2_east<-read.csv("OUTPUT_byplant_so2_east_byplant.csv")
so2_ercot<-read.csv("OUTPUT_byplant_so2_ercot_byplant.csv")
so2_wecc<-read.csv("OUTPUT_byplant_so2_wecc_byplant.csv")
so2_wecc2<-read.csv("OUTPUT_byplant_so2_wecc_byplant2.csv")
so2_wecc3<-read.csv("OUTPUT_byplant_so2_wecc_byplant3.csv")

colnames(nox) <- colnames(so2)
colnames(nox_east) <- colnames(so2_east)
colnames(nox_ercot) <- colnames(so2_ercot)
colnames(nox_wecc) <- colnames(so2_wecc)
colnames(nox_wecc2) <- colnames(so2_wecc2)
colnames(nox_wecc3) <- colnames(so2_wecc3)

# Combine all datasets using rbind

combined_data_nox <- rbind(nox,nox_east,nox_ercot,nox_wecc,nox_wecc2,nox_wecc3)
combined_data_so2 <- rbind(so2, so2_east, so2_ercot, so2_wecc, so2_wecc2, so2_wecc3)

# Find average nox and so2 emissions for each power plant and merge data with lat and lon coordinates

average_nox <- combined_data_nox %>%
  group_by(plant_id_eia) %>%
  summarise(avg_nox = mean(coef.m2.))

# Find average SO2 emissions for each power plant
average_so2 <- combined_data_so2 %>%
  group_by(plant_id_eia) %>%
  summarise(avg_so2 = mean(coef.m2.))

#Make a spreadsheet with the columns: plant_id_eia, nox, so2, lat, lon

df_base <- merge(average_nox,average_so2, by="plant_id_eia")  

#Add another 2 columns that indicates the latitude and longitude for each plant

longlat<-read.csv("Long:Lat Data.csv")
longlat2<- longlat %>% 
  
    select(X.1, X.8, X.9 ) %>%
    rename(plant_id_eia = X.1) %>%
    rename(Latitude = X.8, Longitude= X.9) 

longlat3 <- longlat2[-c(1),]

df_final <- merge(longlat3,df_base, by="plant_id_eia")    
```

```{r}
```

```{r}

library(terra)
eastIDs <- nox_east$plant_id_eia
df_east <- df_final %>%
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  filter(plant_id_eia %in% eastIDs) %>%
  filter(Longitude>= -110 & Longitude<=0)
name <- LETTERS[1:10]
longitude <- as.numeric(df_east$Longitude)
latitude <- df_east$Latitude
locations <- cbind(longitude, latitude)
set.seed(0)
nox_emissions<- df_east$avg_nox
psize <-.1+log10(nox_emissions*65)

plot(locations, cex=psize, pch=20, col='red', main='NOx Emissions')
p <- vect("tl_2022_us_state.shp")
v<-p[c(1:6, 8:13, 15:16, 18:19,21:23,25:26,27:29,30,33,34,38:39,43:46,48:49,51:55)]
plot(v, add=TRUE, density=0, lwd=2, col='black')

eastIDs <- so2_east$plant_id_eia
df_east <- df_final %>%
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  filter(plant_id_eia %in% eastIDs) %>%
  filter(Longitude>= -110 & Longitude<=0)
name <- LETTERS[1:10]
longitude <- as.numeric(df_east$Longitude)
latitude <- df_east$Latitude
locations <- cbind(longitude, latitude)
set.seed(0)
so2_emissions<- df_east$avg_so2
psize <-.1+log10(so2_emissions*650)

plot(locations, cex=psize, pch=20, col='red', main='SO2 Emissions')
p <- vect("tl_2022_us_state.shp")
v<-p[c(1:6, 8:13, 15:16, 18:19,21:23,25:26,27:29,30,33,34,38:39,43:46,48:49,51:55)]
#plot(v)
plot(v, add=TRUE, density=0, lwd=2, col='black')
#Note: The scale of the size of the S02 Points is 10 times that of the NOx points.
```
