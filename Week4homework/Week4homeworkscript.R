#TASK: Join the global gender inequality index to spatial data of the World, 
#creating a new column of difference in inequality between 2010 and 2019. 

library(sf)
library(tidyverse)
library(here)
library(janitor)

#import and then inspect geojson file
worldshape <- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")
plot(worldshape)
str(worldshape)

#NB worldshape has 251 rows

#import and then inspect inequality data file
inequalitydata <- read_csv("HDR25_Composite_indices_complete_time_series.csv",
                           na="NULL",
                           locale = locale(encoding = "latin1"))
problems(inequalitydata)
names(inequalitydata)
head(inequalitydata)
str(inequalitydata)
#inequalitydata has 206 rows; 196-206 are groups/categories
#problems seems to be showing that the columns I need are unexpected type; come back later
#if issues


#filter for columns relating to gii (gender inequality index) in 2019 and 2010,
#keeping iso3 column

giidata <- inequalitydata%>%
  clean_names()%>%
  dplyr::select(iso3, country, gii_2019, gii_2010)
str(giidata)
giidata

#make countrycodes in worldshape and giidata consistent

install.packages("countrycode")
library(countrycode)

#work out how to use countrycode!
?countrycode

#create "CODE" column containing verified iso3 country codes for gii data

giidataiso3 <- countrycode(sourcevar = giidata$country,
                           origin = "country.name.en",
                           destination = "iso3c",
                           warn = TRUE,
                           nomatch = NA,
                           custom_dict = NULL,
                           custom_match = NULL,
                           origin_regex = NULL
                           )
giidataiso3
#Warning Some values were not matched unambiguously: Arab States, East Asia and the Pacific, Europe and Central Asia, 
#High human development, Latin America and the Caribbean, 
#Low human development, Medium human development, South Asia, Sub-Saharan Africa, Very high human development, World

giiwithiso3 <- giidata%>%
  add_column(CODE = giidataiso3, .before = "country")
giiwithiso3

#do same for worldshape 
worldshapeiso3 <- countrycode(sourcevar = worldshape$COUNTRY,
                              origin = "country.name.en",
                              destination = "iso3c",
                              warn = TRUE,
                              nomatch = NA,
                              custom_dict = NULL,
                              custom_match = NULL,
                              origin_regex = NULL
                              )
worldshapeiso3

#Received Warning message:
  #Some values were not matched unambiguously: 
#Azores, Bonaire, Canarias, Glorioso Islands, Juan De Nova Island, Madeira, Micronesia, Saba, Saint Eustatius, Saint Martin

worldshapewithiso3 <- worldshape%>%
  add_column(CODE = worldshapeiso3, .before = "COUNTRY")
worldshapewithiso3

#creating new column of difference in inequality between 2010 and 2019

inequalitydiff <- giiwithiso3$gii_2019 - giiwithiso3$gii_2010
inequalitydiff

giiwithdiff <- giiwithiso3%>%
  add_column(diff20192010 = inequalitydiff, .after = "country")
giiwithdiff

#join data. NB this is second (successful) way of doing so after first attempt
# (commented out below) which tried to do so using new aligned ISO3 created
# result with anomalous rows - due to islands?

joineddata2 <- worldshapewithiso3%>%
  clean_names()%>%
  left_join(.,
            giiwithdiff,
            by = c("country" = "country"))
     
#tidy joineddata to remove duplicate columns (codes)

Joineddatatidy <- joineddata2%>%
  dplyr::select(fid, code, country, countryaff, aff_iso, diff20192010, gii_2019, gii_2010, geometry)
Joineddatatidy  


#Joined by country instead of by iso codes (why the hint about countrycode?) because
#attempting to join by ISO codes resulted in confusion and 351 rows due to mismatch in
#how various territories are categorised.
#Could this be solved by grouping?

#Code commented out below:

#join
#joineddatacodes <- worldshapewithiso3%>%
  #left_join(.,
            #giiwithdiff,
            #by = c("CODE"="CODE"))
#joineddatacodes

#Warning message for ref:
#In sf_column %in% names(g) :
  #Detected an unexpected many-to-many relationship between `x` and `y`.
#ℹ Row 16 of `x` matches multiple rows in `y`.
#ℹ Row 196 of `y` matches multiple rows in `x`.
#ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.