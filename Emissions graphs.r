---
title: "average emissions graphs"
format: html
editor: visual
---

```{r}
library("tidyverse")

NOx_ercot_byhour <- read.csv("OUTPUT_nox_ercot_byhour.csv")
SO2_ercot_byhour <- read.csv("OUTPUT_so2_ercot_byhour.csv")
NOx_ercot_byplant <- read.csv("OUTPUT_nox_ercot_byplant.csv")




NOx_2 <- NOx_ercot_byhour%>%
  mutate(Type = "NOx")
SO2_2 <-SO2_ercot_byhour %>%
  mutate(Type = "SO2")
df <- rbind(NOx_2, SO2_2)

df2 <- df %>%
  mutate(Hour = substr(df$X,21,22)) %>%
  mutate(Emissions = df$x) %>%
  mutate(Hour = as.numeric(Hour))
ggplot(df2, aes(x = Hour, y = Emissions, color = Type)) + geom_point()

ggsave("ercot_byhour.png")

```

Create a box and whisker plot that shows the distribution of emissions by fuel type

```{r}
library("tidyverse")
attributes2019<-read.csv("2019_plant_static_attributes.csv")
attributes2020<-read.csv("2020_plant_static_attributes.csv")
attributes2021<-read.csv("2021_plant_static_attributes.csv")
NOx_ercot_byplant <- read.csv("OUTPUT_nox_ercot_byplant.csv")
SO2_ercot_byplant <- read.csv("OUTPUT_so2_ercot_byplant.csv")

a2019<-attributes2019 %>%
  select(plant_id_eia, fuel_category) %>%
  mutate(year = 2019)
a2020<-attributes2020 %>%
  select(plant_id_eia, fuel_category) %>%
  mutate(year = 2020)
a2021<-attributes2021 %>%
  select(plant_id_eia, fuel_category) %>%
  mutate(year = 2021)

a1920<-merge(a2019,a2020,by = "plant_id_eia")
aALL<-merge(a1920,a2021,by= "plant_id_eia")
aNew<-aALL %>%
  filter(fuel_category.x==fuel_category.y)%>%
  filter(fuel_category.y==fuel_category)

aIDS<-aNew$plant_id_eia
NOxIDS<-NOx_ercot_byplant$plant_id_eia
SO2IDS<-SO2_ercot_byplant$plant_id_eia

ids<-intersect(aIDS,NOxIDS)

df<-read.csv("OUTPUT_nox_ercot_byplant.csv")

dfHydro<-aNew %>%
  filter(fuel_category == "hydro") %>%
  select(plant_id_eia, fuel_category)
dfBiomass<-aNew %>%
  filter(fuel_category == "biomass") %>%
  select(plant_id_eia, fuel_category)
dfCoal<-aNew %>%
  filter(fuel_category == "coal") %>%
  select(plant_id_eia, fuel_category)
dfGeothermal<-aNew %>%
  filter(fuel_category == "geothermal") %>%
  select(plant_id_eia, fuel_category)
dfNaturalGas<-aNew %>%
  filter(fuel_category == "natural_gas") %>%
  select(plant_id_eia, fuel_category)
dfNuclear<-aNew %>%
  filter(fuel_category == "nuclear") %>%
  select(plant_id_eia, fuel_category)
dfOther<-aNew %>%
  filter(fuel_category == "other") %>%
  select(plant_id_eia, fuel_category)
dfPetroleum<-aNew %>%
  filter(fuel_category == "petroleum") %>%
  select(plant_id_eia, fuel_category)
dfSolar<-aNew %>%
  filter(fuel_category == "solar") %>%
  select(plant_id_eia, fuel_category)
dfWaste<-aNew %>%
  filter(fuel_category == "waste") %>%
  select(plant_id_eia, fuel_category)
dfWind<-aNew %>%
  filter(fuel_category == "wind") %>%
  select(plant_id_eia, fuel_category)

idHydro<-dfHydro$plant_id_eia
NOxHydro <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idHydro) %>%
  mutate(Hour = substr(hour,21,22))
idNaturalGas<-dfNaturalGas$plant_id_eia
NOxNaturalGas <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idNaturalGas)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m1.)
ggplot(NOxNaturalGas,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("NOx emissions from Natural Gas Power Plants")
idNuclear<-dfNuclear$plant_id_eia
NOxNuclear <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idNuclear)%>%
  mutate(Hour = substr(hour,21,22))
idOther<-dfOther$plant_id_eia
NOxOther <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idOther)%>%
  mutate(Hour = substr(hour,21,22))
idPetroleum<-dfPetroleum$plant_id_eia
NOxPetroleum <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idPetroleum)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m1.)
ggplot(NOxPetroleum,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("NOx emissions from Petroleum Power Plants")
idSolar<-dfSolar$plant_id_eia
NOxSolar <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idSolar)%>%
  mutate(Hour = substr(hour,21,22))
idWaste<-dfWaste$plant_id_eia
NOxWaste <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idWaste)%>%
  mutate(Hour = substr(hour,21,22))
idWind<-dfWind$plant_id_eia
NOxWind <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idWind)%>%
  mutate(Hour = substr(hour,21,22))
idCoal<-dfCoal$plant_id_eia
NOxCoal <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idCoal)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m1.)
ggplot(NOxCoal,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("NOx emissions from Coal Power Plants")
idGeothermal<-dfGeothermal$plant_id_eia
NOxGeothermal <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idGeothermal)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m1.)
idBiomass<-dfBiomass$plant_id_eia
NOxBiomass <- NOx_ercot_byplant %>%
  filter(NOx_ercot_byplant$plant_id_eia %in% idBiomass)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m1.)
ggplot(NOxBiomass,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("NOx emissions from Biomass Power Plants")

idHydro<-dfHydro$plant_id_eia
SO2Hydro <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idHydro)%>%
  mutate(Hour = substr(hour,21,22))
idNaturalGas<-dfNaturalGas$plant_id_eia
SO2NaturalGas <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idNaturalGas)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m2.)
ggplot(SO2NaturalGas,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("SO2 emissions from Natural Gas Power Plants")
idNuclear<-dfNuclear$plant_id_eia
SO2Nuclear <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idNuclear)%>%
  mutate(Hour = substr(hour,21,22))
idOther<-dfOther$plant_id_eia
SO2Other <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idOther)%>%
  mutate(Hour = substr(hour,21,22))
idPetroleum<-dfPetroleum$plant_id_eia
SO2Petroleum <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idPetroleum)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m2.)
ggplot(SO2Petroleum,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("SO2 emissions from Petroleum Power Plants")
idSolar<-dfSolar$plant_id_eia
SO2Solar <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idSolar)%>%
  mutate(Hour = substr(hour,21,22))
idWaste<-dfWaste$plant_id_eia
SO2Waste <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idWaste)%>%
  mutate(Hour = substr(hour,21,22))
idWind<-dfWind$plant_id_eia
SO2Wind <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idWind)%>%
  mutate(Hour = substr(hour,21,22))
idCoal<-dfCoal$plant_id_eia
SO2Coal <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idCoal)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m2.)
ggplot(SO2Coal,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("SO2 emissions from Coal Power Plants")
idGeothermal<-dfGeothermal$plant_id_eia
SO2Geothermal <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idGeothermal)%>%
  mutate(Hour = substr(hour,21,22))
idBiomass<-dfBiomass$plant_id_eia
SO2Biomass <- SO2_ercot_byplant %>%
  filter(SO2_ercot_byplant$plant_id_eia %in% idBiomass)%>%
  mutate(Hour = substr(hour,21,22)) %>%
  mutate(Emissions = coef.m2.)
ggplot(SO2Biomass,aes(x=Hour,y=Emissions,color=as.factor(plant_id_eia))) + geom_point() + theme(legend.position = "none") + ggtitle("SO2 emissions from Biomass Power Plants")
```

```{r}

ft<-c("biomass", "coal")

i<-1


df<-aNew %>%
  filter(fuel_category == ft[i]) %>%
  select(plant_id_eia, fuel_category)

id<-df$plant_id_eia

NOx <- NOx_ercot_byplant %>%
  filter(plant_id_eia %in% id)

write.csv(NOx, )
ggsave(paste0("graph_", ft[i], ".png"))



```
