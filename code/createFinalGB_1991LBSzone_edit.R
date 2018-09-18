geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MERGING GB GEOGRAPHIES from Scots + Eng/Wales zero count reassigns----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We need a merged 91 PCS / WARD file from my aggregated versions.
#And GB output-area files for 2001 and 2011.
#The intersect I'll then do in QGIS.

#91 pcs and wards----

pcs91scot <- readOGR(dsn='C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount',layer='pseudoPCS_aggregated4CorrectCount')

wards91engWales <- readOGR(dsn='C:/Data/MapPolygons/EnglandWalesMerged/1991/wardsAggForCorrectCount',layer='wardsEngWales_aggregated4CorrectCount')

#can I just...?
#Dunno why makeUniqueIDs doesn't autocomplete.
gb <- rbind(wards91engWales,pcs91scot,makeUniqueIDs = T)

#Need to plot and check join...
writeOGR(gb, "C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount", driver="ESRI Shapefile", overwrite_layer = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fix overlapping zones issue----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sf)
library(tidyverse)
library(tmap)
#Fix two/three overlapping zones in the 1991 aggregated wards file
#These are the two "hidden" by a larger zone:
#20GNGH 20GNGJ
#This is the one they're "under" (or over)
#20GNGG

#Get the original file
wardz <- st_read('C:/Data/MapPolygons/GreatBritain/1991/GB_wards_pcs_agg4correctCount.shp')
st_crs(wardz) <- 27700

#pull out the troublesome zones
trouble <- wardz %>% filter(label %in% c("20GNGH","20GNGJ","20GNGG"))

plot(st_geometry(trouble))

#tmap_mode('view')
#qtm(trouble)

#I should just test this is actually the problem. Thus:
#Let's see if postcode centroids fail to go into right zones
postcodes <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Training/Intro_to_R_Nov2016/Intro_R_again/data/codepoint_open_allGBpostcodes.csv')

table(postcodes$Country_code)

#Drop Scotland: Land Registry data is only for England and Wales
postcodes <- postcodes %>% filter(Country_code != 'S92000003')

#We only need a few of these columns
postcodes <- postcodes %>% dplyr::select(Postcode,Eastings,Northings)

#To sf
pcsf <- st_as_sf(postcodes, coords = c("Eastings", "Northings"), crs = 27700, agr = "identity")
st_crs(pcsf) <- 27700


getwhat <- sapply(st_intersects(pcsf,trouble), function(z) if (length(z)==0) NA_integer_ else z[1])

getwhat[!is.na(getwhat)]
#So presumably these are all in that larger zone?
#Yup.
plot(st_geometry(trouble[1,]))


#OK then. So, to make a new set... let's break this down.
#First, just the two smaller ones alone.
twosmall <- trouble[c(2:3),]
plot(st_geometry(twosmall))

#Doing intersects with these two should now work
getwhat <- sapply(st_intersects(pcsf,twosmall), function(z) if (length(z)==0) NA_integer_ else z[1])
#Tick
getwhat[!is.na(getwhat)]


onebig <- trouble[1,]
smallerdissolved <- st_union(twosmall)

plot(trouble[1,])
plot(trouble)
plot(twosmall)
plot(smallerdissolved)

#Then to get the other zone as the difference
#Yes,this!
otherzone <- st_difference(onebig,smallerdissolved)
plot(st_geometry(otherzone), col = "BLUE")

#Combine and test
allthreeproper <- rbind(otherzone,twosmall)
plot(allthreeproper)
getwhat <- sapply(st_intersects(pcsf,allthreeproper), function(z) if (length(z)==0) NA_integer_ else z[1])
#Tick
table(getwhat[!is.na(getwhat)])

#Replace in original
keepz <- wardz %>% filter(!label %in% c("20GNGH","20GNGJ","20GNGG"))

allz <- rbind(keepz,allthreeproper)

#And that should now work. Check in QGIS.
st_write(allz,'data/qgis/check_ward_processing.shp')

#Seems to be working... replace original (back up original!)
st_write(allz,'C:/Data/MapPolygons/GreatBritain/1991/GB_wards_pcs_agg4correctCount.shp')