geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

source("Function_CoBRegressionFunctions.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~FIVE CENSUS GB EMPLOYMENT/ECON ACTIVE PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#All five censuses to stitch. So out of "economically active" what proportion are unemployed?
#Then once saved, stick all through the geog reassign code.

#1971 Econ Active----

#Want to end with ... let's check to match format of previous
#chk71 <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/1971_econActive_from_71EDs_to_91_aggPostcodeSectors.csv')
#chk81 <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/1981_econActive_from_81EDs_to_91_aggPostcodeSectors.csv')

#Yup, just need econ active as 100%, unemployed count and then get % employed.
#Just gotta decide exactly what's EA for some of these. 

ea71 <- read_csv('data/census_sources/econActiveSources/gb_econActive1971.csv')

#Staring at 71 ea table to get codes.
#Male is in one column, female in two (cos early censuses were sexist as well as racist.)
#I think they're all total EA columns for ages 15 to 75+. Hmm...
#Total = 257+258+259
#Working = 260+263+266
#Seeking work = 261+264+267
#Sick = 262+265+268 (which may count as not econ active but defs don't say. Hum.)

ea71sums <- data.frame(total = apply(ea71[,c("c71s05_257","c71s05_258","c71s05_259")],1,sum),
                       working = apply(ea71[,c("c71s05_260","c71s05_263","c71s05_266")],1,sum),
                       seekingwork = apply(ea71[,c("c71s05_261","c71s05_264","c71s05_267")],1,sum),
                       sick = apply(ea71[,c("c71s05_262","c71s05_265","c71s05_268")],1,sum))

#Use total of working and seeking work for EA total. 
#Seeking work as unemployed
#ps I thought tibble was lazy eval, could do percent in creation, but seems not.
ea71final <- data.frame(
  zone = ea71$`Zone Code`,
  econActive = apply(ea71sums[,c("working","seekingwork")],1,sum),
  unemployed = ea71sums[,"seekingwork"]) 

#There appears to be one zone where no-one is employed...?
#zonez <- ea71final %>% filter(percentEmployed < 1) %>% dplyr::select(zone)

#thing <- ea71[ea71$`Zone Code` %in% zonez$zone,]

#Hmm, some in England.I suspect city centre areas e.g. in London. Leave in...? Issue with summing? Note.

#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its71,ea71final,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#Add percent employed column
result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100

df <- data.frame(result)

write_csv(df,'data/census_sources/econActiveSources/output/1971econActive.csv')





#1981 Econ active----

ea81 <- read_csv('data/census_sources/econActiveSources/gb_81_econActive.csv')

#total persons in employment (full-time...) = 789
#total persons not in employment = 859
#total persons self-employed = 705
ea81final <- data.frame(zone = ea81$`Zone ID`,
                        econActive = ea81$`[81sas090789]`+ea81$`[81sas090859]`+ea81$`[81sas090705]`,
                        unemployed = ea81$`[81sas090859]`) 
#%>% 
#  mutate(percentEmployed = ((econActive-unemployed)/econActive)*100)

#Again with the many NaN... that's a lot! Why? Mark and look in qgis
#Might be one country.
#Oh it's various spots in national parks and city centres. OK.
#table(0 + is.nan(ea81final$percentEmployed))
#ea81final$isnan <- 0 + is.nan(ea81final$percentEmployed)
#write_csv(ea81final,'R_data/isnancheck.csv')


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its81,ea81final,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100
df <- data.frame(result)

write_csv(df,'data/census_sources/econActiveSources/output/1981econActive.csv')


#1991 Econ active----

#Where we will need to do the direct summing to the aggregated zones. But first the data.
ea91 <- read_csv('data/census_sources/econActiveSources/91_gb_econActive.csv')

#1: total persons
#20: econ active
#115: on govt scheme (gonna count as unemployed)
#134: unemployed
#172: econ inactive

ea91final <- data.frame(zone = ea91$`Zone ID`,
                        econActive = ea91$l080020,
                        unemployed = ea91$l080115 + ea91$l080134)



#ZONE AGGREGATION DIRECTLY (cos 91 is the decade we use the geog for)

#Annoyingly I can't find where I did 91 econ active for Scotland. So adapt GB cob.
pcs91shp <- readOGR("C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Yup
shp_df <- data.frame(pcs91shp)
shp_df$label <- as.character(shp_df$label)
shp_df$fnl_rsL <- as.character(shp_df$fnl_rsL)

#work only on those with more than one zone to aggregate
shp_df <- shp_df[!is.na(shp_df$fnl_rsL),]

#Each element a list of zones to combine
zonez <- lapply(c(1:nrow(shp_df)), 
                function(x) c(shp_df$label[x],
                              unlist(lapply(shp_df$fnl_rsL[x], function(y) strsplit(y,"\\|"))))
)


#most will remain the same...
ea91final$aggID <- ea91final$zone

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  ea91final$aggID[ea91final$zone %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- ea91final[,2:4] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
zeroes <- agg2[apply(agg2[,2:3],1,sum)==0,]
zeroes <- merge(zeroes[,1],ea91final,by.x = 'label',by.y = 'zone')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
table(as.character(zeroes$label))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

#add in percent
geogz$percentEmployed <- ((geogz$econActive-geogz$unemployed)/geogz$econActive)*100

geogzdf <- data.frame(geogz)

write_csv(geogzdf,'data/census_sources/econActiveSources/output/1991econActive.csv')



#2001 Econ active----

#No multiple tick box option in CASWEB for 2001 means...
ea01scot <- read_csv('data/census_sources/econActiveSources/2001_Scots_economicallyActive.csv')
ea01eng <- read_csv('data/census_sources/econActiveSources/england_2001_econActive_univariate.csv')
ea01wales <- read_csv('data/census_sources/econActiveSources/wales_2001_econActive_univariate.csv')

#names(ea01scot)
#names(ea01eng)
#names(ea01wales)

#Just 02 and 12 for econ active and unemployed respectively.
ea01 <- do.call(rbind, list(ea01scot,ea01eng,ea01wales))

#Which is already in the right form. Yay! Just a change of names for consistency.
names(ea01) <- c('zone','econActive','unemployed')

#Write in case I need to reload without using readr
write_csv(ea01,'R_data/ea01.csv')


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its01,ea01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100
df <- data.frame(result)

write_csv(df,'data/census_sources/econActiveSources/output/2001econActive.csv')


#2011 econ Active----

ea11 <- read_csv('data/census_sources/econActiveSources/GB_2011_econActive_tidied.csv')

#f243: total
#f244: econ active
#f248: unemployed
#f251: econ inactive
#Ignore rest

ea11final <- ea11 %>% dplyr::select(code = GEO_CODE,econActive = F244, unemployed = F248)


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its11,ea11final,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100
df <- data.frame(result)

write_csv(df,'data/census_sources/econActiveSources/output/2011econActive.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5-census GB econ active checks----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Pretty pic comparing all. And these are in the right order, good...
files5 <- list.files('data/census_sources/econActiveSources/output/',pattern = 'econActive*', full.names = T)

fivez <- lapply(files5,read_csv)

years <- seq(from = 1971, to = 2011, by = 10)
for(x in c(1:5)) fivez[[x]]$year <- years[x]

#Ready for combining
allz <- do.call("rbind",fivez)

#Order by a particular decade. Only need unemployment
unemp <- allz[,c("label", "percentEmployed","year")] %>% 
  spread(year,percentEmployed)

unemp <- unemp[order(-unemp$`1971`),] %>% 
  mutate(order = c(1:nrow(unemp)))

unemp <- unemp %>% 
  gather(key = year, value = percentEmployed, `1971`:`2011`)

ggplot(unemp,aes(y = percentEmployed, x = order, colour = year)) +
  geom_line()

#Well that didn't work! I mean, it did in theory, just not in practice.
#OK so let's just do some boxplots instead. I can come on to regional ones in a mo.
ggplot(unemp,aes(y = percentEmployed, x = year)) +
  geom_boxplot()

#ranks
empranks <- unemp %>% group_by(year) %>% 
  mutate(rank = rank(percentEmployed,ties.method = 'random'))

#wide to view in QGIS
empranks <- empranks %>% 
  dplyr::select(label,year,rank) %>% 
  spread(year,rank)

write_csv(empranks,'R_data/empranks.csv')

