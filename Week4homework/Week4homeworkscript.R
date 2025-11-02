#TASK: Join the global gender inequality index to spatial data of the World, 
#creating a new column of difference in inequality between 2010 and 2019. 

library(sf)
library(tidyverse)
library(here)
library(janitor)
library(countrycode)
library(tmap)

#import and then inspect geojson file
worldshape <- st_read("World_Countries_(Generalized)_-573431906301700955")
plot(worldshape)
str(worldshape)

#NB worldshape has 251 rows

#import and then inspect inequality data file
inequalitydata <- read_csv("HDR25_Composite_indices_complete_time_series.csv",
                           na="NULL",
                           locale = locale(encoding = "latin1"))
names(inequalitydata)
head(inequalitydata)

#inequalitydata has 206 rows; 196-206 are groups/categories
#problems seems to be showing that the columns I need are unexpected type; come back later
#if issues


#filter for columns relating to gii (gender inequality index) in 2019 and 2010,
#keeping iso3 column

giidata <- inequalitydata%>%
  clean_names()%>%
  dplyr::select(iso3, country, gii_2019, gii_2010) %>%
  mutate(giiwithdiff = gii_2019-gii_2010)

#make countrycodes in worldshape and giidata consistent
#create "CODE" column containing verified iso3 country codes for gii data

giidataiso3 <- giidata %>%
  mutate(iso_code = countrycode::countrycode(sourcevar = giidata$country,
                                             origin = "country.name.en",
                                             destination = "iso3c",
                                             warn = TRUE,
                                             nomatch = NA,
                                             custom_dict = NULL,
                                             custom_match = NULL,
                                             origin_regex = NULL))


#do same for worldshape 
worldshapeiso3 <- worldshape %>%
  mutate(code = countrycode::countrycode(sourcevar = worldshape$COUNTRY,
                              origin = "country.name.en",
                              destination = "iso3c",
                              warn = TRUE,
                              nomatch = NA,
                              custom_dict = NULL,
                              custom_match = NULL,
                              origin_regex = NULL
                              ))


#join data. NB this is second (successful) way of doing so after first attempt
# (commented out below) which tried to do so using new aligned ISO3 created
# result with anomalous rows - due to islands?

joineddata <- worldshapewithiso3%>%
  clean_names()%>%
  left_join(.,
            giidataiso3,
            by = c("code" = "iso_code"))


###############################################################################
########### map the data ######################################################
tmap_mode("plot")
tm1 <- tm_shape(joineddata) +
  tm_polygons("giiwithdiff",
              col = "black",
              lwd = 0.5,
              lty = "dashed",
              fill.chart = tm_chart_violin(),
              fill.scale = tm_scale_intervals(
                values = "brewer.bu_pu",   # this line control the color scale for a classed (binned) choropleth
                n = 5,
                style = "jenks"))
tm1
