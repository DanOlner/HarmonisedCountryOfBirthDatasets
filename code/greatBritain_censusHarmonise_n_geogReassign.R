#England / wales data wrangling
#Get it all into shape prior to aggregating / intersecting etc
#Plus verifying against shapefiles etc.
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

source("Function_DataFromSmalltoLargeGeog.R")
source('Function_Shapefile_uniqueIDs_to_singleRowFeatures.R')
source("Function_bulkColumnRecode.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~3 CENSUS~~~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~COB: CHECKING ENGWALES vs SCOTS ON EACH DECADE~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#That is e.g. for 2001, making a single GB file. So getting those categories the same 
#because Scotland is a bit different to Eng/Wales
#Before then harmonising across censuses. Delightful!

#1991----
engWales91 <- read_csv('1991/EnglandWales/EnglandWales_LBS_ward_CoB_totalPersons_countryNamesAdded.csv')
scot91 <- read_csv('1991/Scotland//1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv')

#Ah, think those two are the same
gsub("\\."," ",names(engWales91))==names(scot91)

#Annoying... EYEBALL!! 
chk <- data.frame(names(engWales91),names(scot91))

#Yup, match. So make into GB. Assuming the IDs are unique, right? Right.
table(engWales91$Zone.ID %in% scot91$`Zone ID`)

#Cos we just checked they're identical (but formatting is different)
names(engWales91) <- names(scot91)
gb91 <- rbind(engWales91,scot91)

write_csv(gb91,'1991/GreatBritain/1991_GreatBritain_LBS_CoB_Ward_n_PCS_countyNamesAdded.csv')

#2001----

engWales01 <- read_csv('2001/EnglandWales/2001_EnglandWales_CoB_OA_totalPersons_countryNamesAdded.csv')
#Ah - this is where the missing "Non.EU.Countries.in.Western.Europe" is. Need to get that back.
#Let's just check I'm right that it's really not a sum column... newp, it's not. Shouldn't have deleted.
#OK, updated. Reload, double-check.
scot01 <- read_csv('2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv')

names(engWales01)
names(scot01)

chk <- data.frame(names(engWales01),names(scot01))

#Yup, all good now. Save!
names(engWales01) <- names(scot01)
gb01 <- rbind(engWales01,scot01)

write_csv(gb01,'2001/GreatBritain/2001_GreatBritain_CoB_OA_countyNamesAdded.csv')

#2011----

#Think this might be the one with the awkward non-matching categories.
engWales11 <- read_csv('2011/EnglandWales/EngWales_CountryOfBirth_OA_2011_NOMIS_countryNamesAdded.csv')
scot11 <- read_csv('2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv')

chk <- data.frame(names(engWales11),c(names(scot11),'-'))

#Won't let me look at that properly. Save and excel!
write_csv(chk,'R_data/bits/looksee.csv')

#Check matches. 2011 Scots is long-name.
sapply(names(engWales11),function(x) grepl(x,names(scot11)))  

#Some notes from looking:
#EngWales has North Africa. That will have to go into "other". Not present elsewhere.

#UPDATE 2011 TO MAKE ENG/WALES AND SCOTLAND CONSISTENT BEFORE COMBINING----
#From this 'ere printout wot I am lookin at
#Let's use the thingyo
combo <- list(
  
  list(engWales11,scot11),#all data to sum columns on
  list('1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:11','7'),
  list('UK part not specified','6,7','6'),#Oops, Isle of Man is in here too. And I double counted N Ire
  list('Other EU member countries','16,18','11,13,15'),
  list('Other EU accession countries','22','16:18,22:23'),
  list('Other Europe','24','24,26'),
  list('Other Central and Western Africa','26,28','28'),
  list('Other South and Eastern Africa','30,33','32'),
  list('Other Middle East','36','34:35'),
  list('Other Southern Asia','43,44','42'),
  list('Other South-East Asia','46','43,45,46'),
  list('Other Eastern Asia','39','38'),
  list('Other North America','49','48,50'),
  list('Caribbean','52:53','53'),
  list('Antarctica other Oceania','54,57','56'),
  list('Rest of world','25,34,56,58','55,57')#includes north africa from eng/wales
  
)

result <- recode(combo)
#Should be able to combine those two parts now...
chk <- result[[2]]


#Get a list of all those not containing other then check for matches across both
# engWales11noOther <- engWales11[,!grepl('other',names(engWales11),ignore.case = T)]
# #Yup, that'll do. 
# names(engWales11noOther)
# names(engWales11)[!(names(engWales11) %in% names(engWales11noOther))]

#So periods in the scots names
names(scot11)[grepl('south',names(scot11),ignore.case = T)]

#Will be in the right index orderrrr oh no, engNamez won't be. OK, so...
#Slightly more faff

#1. List of no-"other" names indexed by original column number
engWales11noOther <- data.frame(engcob = names(engWales11), engnum = c(1:ncol(engWales11)))
  
#Subset of names that don't contain 'other'
engWales11noOther <- engWales11noOther[!grepl('other',engWales11noOther$engcob,ignore.case = T),]

#Create index for scots names...
#Wait, this is a silly way of doing this, surely? Actually, no...
#scotNamez <- data.frame(scotcob = gsub('\\.',' ',names(scot11)), scotnum = 1:ncol(scot11))
scotNamez <- gsub('\\.',' ',names(scot11))

#can't see a non-loop way
#engWales11noOther$scotindex <- NA
engWales11noOther$scotindex <- lapply(engWales11noOther$engcob,function(x) which(grepl(x,scotNamez,ignore.case = T)))
#engWales11noOther$scotindex2 <- lapply(engWales11noOther$scotindex,function(x) ifelse(length))

#OK! Phew. Eyeball to see which of those are useable.
#table(length(engWales11noOther$scotindex[2])==0)

#Sod it, manually pull out those to keep
row.names(engWales11noOther) <- 1:nrow(engWales11noOther)

#Hong kong missing. It's in both...
keepz <- engWales11noOther[c(2:5,9:12,14:18,21:22,24:27,29:31,33:37,40),]
  
#Just gotta fix the china/hong-kong thing.
keepz$scotindex[keepz$engcob=='China'] <- 36
#And Ireland (got lost cos of the N Ire thing)
keepz$scotindex[keepz$engcob=='Ireland'] <- 8
#And Spain!
keepz$scotindex[grepl('Spain',keepz$engcob)] <- 14

keepz$engcob <- as.character(keepz$engcob)
keepz <- rbind(keepz,c('Hong Kong',38,37))



#So now: keep only those columns from Eng and Scot, change order to match
#Just order first so Hong Kong is in a sensible place
keepz <- keepz[order(as.numeric(keepz$engnum)),]

engSingles <- engWales11[,as.numeric(keepz$engnum)]
scotSingles <- scot11[,as.numeric(keepz$scotindex)]

#Correct column order?
chk <- data.frame(names(engSingles),names(scotSingles))

#damn european countries... Yup, match.
write_csv(chk,'R_data/bits/lookseeagain.csv')

#Now need to join up the four pieces
engAll <- cbind(engSingles,result[[1]])
scotAll <- cbind(scotSingles,result[[2]])

#Just checked column names are right so should be good here. But... 
write_csv(data.frame(names(engAll),names(scotAll)),'R_data/bits/lookseeagain.csv')

#Yup, good. Just a couple of changes to make...
names(scotAll) <- names(engAll)

allAll <- rbind(scotAll,engAll)

#shift zone code column to the start
allAll <- allAll[,c(30,1:29,31:45)]

#Shorten Hong Kong name
names(allAll)[grepl('Hong',names(allAll),ignore.case = T)] <- 'Hong Kong'
names(allAll)[grepl('Spain',names(allAll),ignore.case = T)] <- 'Spain'

#Think that might be it!
write_csv(allAll,'2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')
#allAll <- read_csv('2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')

#Check no categories were missed:
chk <- data.frame(names(engWales11),c(names(scot11),'-'),c(names(allAll),rep('-',16)))

#Looks... OK? No... Ireland I think is missing. Fixed. Faff!
write_csv(chk,'R_data/bits/lookseeagain.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#HARMONISE GB COB DECADES INTO THE SAME CATEGORIES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load em and look
gb91 <- read_csv('1991/GreatBritain/1991_GreatBritain_LBS_CoB_Ward_n_PCS_countyNamesAdded.csv')
gb01 <- read_csv('2001/GreatBritain/2001_GreatBritain_CoB_OA_countyNamesAdded.csv')
gb11 <- read_csv('2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')

#Are names roughly the same format? Yup.
lapply(list(gb91,gb01,gb11),names)

#Save that to stick in spreadsheet and remove as we combine
chk <- data.frame(gb91 = names(gb91),gb01 = c(names(gb01),rep('-',23)),gb11 = c(names(gb11),rep('-',47)))

write_csv(chk,'R_data/bits/3census_gb_removes.csv')

#A couple of 91 countries need their names changing to make sure they match with the other two
names(gb91)[grepl('United States',names(gb91))] <- 'United States'
names(gb91)[grepl('China',names(gb91))] <- 'China'

#Code from scots 3 census stitch...
foundMatches <- list()
#List grows one at a time...
j = 1

#cycle through all names in 91LBS (has the most categories)
for (i in 3:ncol(gb91)) {
  
  #Ignore these matches
  #"other" will vary across years and need checking
  #ignorz <- c("Channel.Islands","Isle.of.Man","other")
  #Actually, just going to remove them manually
  
  two001 <- which(grepl(names(gb91)[i],names(gb01), ignore.case = T))
  two011 <- which(grepl(names(gb91)[i],names(gb11), ignore.case = T))
  
  #print(length(two011))
  
  #need matches for both
  if(length(two011) != 0 & length(two001) != 0) {
    
    foundMatches[[j]] <- list(names(gb91)[i],i,two001,two011)
    
    j <- j + 1
    
  }
  
}

#check what that looks like
for(i in 1:length(foundMatches)){
  
  print(paste(i,foundMatches[i][[1]][1],
              foundMatches[i][[1]][2],
              foundMatches[i][[1]][3],
              foundMatches[i][[1]][4],
              sep = " "))
  
}

#remove channel islands/Isle of man
#And other middle east
#And other Europe
#And South America. 
#AND Caribbean... ARGH
#All done in the recode below
foundMatches <- foundMatches[-c(5,6,21,24,25,27)]


#Passing everything in to function to make sure names across everything remains consistent
#The second list: indexes columns to keep from the dataframes to re-attach after re-coding
#All others: common variable name and, for each dataframe, columns to sum
#(or just a single column)
#Add in 91 SAS
threeCensusCombo <- list(
  list(gb91,gb01,gb11),#all data to sum columns on
  list('1,2','1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:9','9','31'),
  list('UK part not specified','7','6','32'),
  list('Irish Republic','10,11','7,8','6'),#Ireland part not specified is already added in 2011, so do the same for 91/01
  list('South Africa','77','40','17'),#didn't get from single name search cos it's "afria" in '91, search missed it
  list('Africa other','16:19,21:23,25,78,44:45','34,36:37,39,42','36:37'),#excludes North Africa due to '11 scots eng mismatch
  list('Caribbean','26:33,80','62:63','43'),
  list('South America','81:82','66','28:29'),
  list('Other Middle East','84:85','45:46','38'),
  #list('Other Eastern Asia','88:90','49:52','40'),No, has to go in rest-of-world
  #list('Other South East Asia','86,89:90','58',''),#newp, will have to be rest of world! No S.E Asia in 01
  list('Other EU member states (non-accession)','47:48,51,53:55,58,61,66','10:13,16,18:20,22','33'),
  list('Other Europe','41:43,57,59:60,62:63,67:69,71','23:25,29:32,43','11,34:35'),#includes a mix of accession countries and other, as well as USSR
  list('Rest of world','13:14,37,39:40,46,72:76,86,88:92','33,49:54,58:60,64,67:69','25:26,39:42,44:45')#North Africa has to go here due to 2011 mismatch in Eng/Scot. South-East Asia has to go here too. 'Other Asia' 91 is fine here too, see 91 defs.
)

#Add automatically found results
threeCensusCombo <- c(threeCensusCombo,foundMatches)

#Then work out what's left.
#check what that looks like
for(i in 3:length(threeCensusCombo)){
  
  print(paste(i,threeCensusCombo[i][[1]][1],
              threeCensusCombo[i][[1]][2],
              threeCensusCombo[i][[1]][3],
              threeCensusCombo[i][[1]][4],
              sep = " "))
  
}

#Returns list of re-coded dataframes
#"Error: column_recode_lists_are_all_correct_length not equal to TRUE"
#Means lists defining recodes aren't all of a length that recodes in each dataframe
results <- recode(threeCensusCombo)

looksee <- results[[3]]

#Save as five dataframes, ready for geog re-assigning (apart from '91 which is correct already)
savens <- c('91LBS_PCS','01_OA','11_OA')

lapply(1:length(savens),function(x) write.csv(results[[x]],
                                              paste0("data/census/VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/",savens[x],".csv"),
                                              row.names = F))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MERGING GB GEOGRAPHIES----
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

#~~~~~~~~~~~~
#2001 OAs----
#~~~~~~~~~~~~

#Ah, haven't downloaded them yet! And it turns out counties and unitary authorities are in different files
#FOR GOD KNOWS WHAT REASON
# oa01engCounties <- readOGR(dsn='C:/Data/MapPolygons/England/2001/England_oa_2001_gen_counties',layer='england_oa_2001_gen')
# oa01engUAs <- readOGR(dsn='C:/Data/MapPolygons/England/2001/England_ua_oa_2001_gen_unitaries',layer='england_ua_oa_2001_gen')
# 
# oa01both <- rbind(oa01engCounties,oa01engUAs,makeUniqueIDs = T)
# 
# writeOGR(oa01both, "C:/Data/MapPolygons/England/2001/England_oa_2001_gen","England_oa_2001_gen", driver="ESRI Shapefile", overwrite_layer = T)

#GET ENGLAND OAS, TWO FILES, COMBINE AND DISSOLVE BY ID----

#both!
# oa01eng <- readOGR(dsn='C:/Data/MapPolygons/England/2001/England_oa_2001_gen',layer='England_oa_2001_gen')
# 
# #Checks: one ID per polygon? Newp!
# unique(oa01eng$label) %>% length
# #Dissolve
# oa01engDissolve <- gUnaryUnion(oa01eng,id = oa01eng$label)
# 
# #quick, save! Oh yeah, it loses the IDs. May or may not be in the same order. It's in the row names isn't it? Yup.
# row.names(oa01engDissolve)[1:10]
# 
# oa01dissolve_spdf <- SpatialPolygonsDataFrame(oa01engDissolve,data.frame(zone.code = row.names(oa01engDissolve)),match.ID = F)
# 
# writeOGR(oa01dissolve_spdf, "C:/Data/MapPolygons/England/2001/England_oa_2001_gen","England_oa_2001_gen_dissolvedByID", driver="ESRI Shapefile", overwrite_layer = T)
# 
# #Oh it gets better! That was just counties - OAs from unitary authorities is, for some insane reason, in a different file.
# #It is at least the correct zones...
# 
# #Before that, let's just check it's actually getting the correct zones at all... Yup.
# chk <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/01_OA.csv')
# # oa01dissolve_spdf <- readOGR(dsn="C:/Data/MapPolygons/England/2001/England_oa_2001_gen",layer="England_oa_2001_gen_dissolvedByID")
# 
# #Yup, all good.
# table(oa01dissolve_spdf$zone.code %in% chk$`Zone Code` )

#SAME FOR WALES----

# oa01wales <- readOGR(dsn='C:/Data/MapPolygons/Wales/2001/Wales_oa_2001_clipped',layer='wales_oa_2001_clipped')
# 
# #Needs dissolving? Yes but there aren't many.
# unique(oa01wales$label) %>% length
# 
# oa01WalesDissolve <- gUnaryUnion(oa01wales,id = oa01wales$label)
# 
# oa01dissolve_spdf <- SpatialPolygonsDataFrame(oa01WalesDissolve,data.frame(zone.code = row.names(oa01WalesDissolve)),match.ID = F)
#  
# #Yup, looks good.
# writeOGR(oa01dissolve_spdf, "C:/Data/MapPolygons/Wales/2001/Wales_oa_2001_clipped","wales_oa_2001_clipped_dissolvedByID", driver="ESRI Shapefile", overwrite_layer = T)
 
#RELOAD NATIONS 2001 OAS, COMBINE----

oa01scot <- readOGR(dsn='C:/Data/MapPolygons/Scotland/2001/Scotland_outputareas_2001',layer='scotland_oa_2001_dissolvedTo_OneIDperRow_noSelfIntersect')

oa01eng <- readOGR(dsn="C:/Data/MapPolygons/England/2001/England_oa_2001_gen",layer="England_oa_2001_gen_dissolvedByID")

oa01wales <- readOGR("C:/Data/MapPolygons/Wales/2001/Wales_oa_2001_clipped","wales_oa_2001_clipped_dissolvedByID")

#First isn't working. Problem is what? Not any overlap of labels. 
#Oh, it's the names... like it sez. Duh.
names(oa01eng)
names(oa01wales)
names(oa01scot) <- 'zone_code'

#All
oa01gb <- rbind(oa01eng,oa01scot,makeUniqueIDs = T)
oa01gb <- rbind(oa01gb,oa01wales,makeUniqueIDs = T)

#all unique? Yup.
unique(oa01gb$zone_code) %>% length

#SAVE!
writeOGR(oa01gb, "C:/Data/MapPolygons/GreatBritain/2001/outputAreas","greatBritain2001_outputAreas_dissolvedByID", driver="ESRI Shapefile", overwrite_layer = T)

#Check match in CoB file
chk <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/01_OA.csv')

#Perfect. That's a nice surprise! Chances of repeating that with 2011 are slim...
table(oa01gb$zone_code %in% chk$`Zone Code` )


#~~~~~~~~~~~~
#2011 OAs----
#~~~~~~~~~~~~

#I think there's a chance these ones don't need dissolving. Scotland didn't, did it? Also, we don't need all those fields...
oa11scot <- readOGR('C:/Data/MapPolygons/Scotland/2011/Scotland_output_areas_2011','scotland_oac_2011')

#Yup, good
chk <- data.frame(oa11scot)
unique(oa11scot$code) %>% length

#1 field this time
oa11eng <- readOGR('C:/Data/MapPolygons/England/2011/England_outputareas_2011_gen','England_oa_2011_gen')

#Tick!
unique(oa11eng$CODE) %>% length

oa11wales <- readOGR('C:/Data/MapPolygons/Wales/2011/Wales_oa_2011_gen','wales_oa_2011_gen')

#TICK!
unique(oa11wales$code) %>% length

#OK, immediate combine then. Get names lined up.
#Oh. Why wouldn't @data work? Oh well, this does.
oa11scot <- oa11scot[,2]

names(oa11scot)
names(oa11wales)
names(oa11eng) <- 'code'#only different one

#Quite how it separates out the passed function argument, I don't know...
oa11gb <- do.call(rbind,list(oa11scot,oa11wales,oa11eng,makeUniqueIDs=T))

#Well that seemed to work.
unique(oa11gb$code) %>% length

#Check against 2011 cob codes
chk <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/11_OA.csv')

#Good lord! Full match again. Unprecedented!
table(oa11gb$code %in% chk$geography)

writeOGR(oa11gb, "C:/Data/MapPolygons/GreatBritain/2011/outputAreas","greatBritain2011_outputAreas", driver="ESRI Shapefile", overwrite_layer = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1991: AGGREGATE VARIABLES TO NEW LBS ZONES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Code taken from 91_PCSzeroCountZones_aggregateTables.R
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

#~~~~~~~~~~~~~~~~~~

#Great Britain 91 CoB
CoB91 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/91LBS_PCS.csv")

#We seem to have more zones in CoB91 than in the the shapefile. What's the crack?
table(CoB91$Zone.ID %in% pcs91shp$label)
#But we do have all the geographies covered. 
table(pcs91shp$label %in% CoB91$Zone.ID)

#What are the CoB values for those we have no GB geography for?
noGeogz <- CoB91[!(CoB91$Zone.ID %in% pcs91shp$label),]

#Ah, we still have a lot of shipping I hadn't removed. Oops.

#~~~~~~~

#most will remain the same...
CoB91$aggID <- CoB91$Zone.ID

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  CoB91$aggID[CoB91$Zone.ID %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- CoB91[,3:36] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
zeroes <- agg2[apply(agg2[,2:34],1,sum)==0,]
zeroes <- merge(zeroes[,1],CoB91[,c(1:2)],by.x = 'label',by.y = 'Zone.ID')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
table(as.character(zeroes$Zone.name))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")
geogzdf <- data.frame(geogz)

#save
# writeOGR(geogz, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch", driver="ESRI Shapefile", overwrite_layer = T)

writeSpatialShape(geogz,
                  "StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2001 + 2011 COB: USING INTERSECTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Code from censusData_smallToLargeGeogs_3_censusTo1991aggPCS.R
#source("Function_DataFromSmalltoLargeGeog.R")

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#~~~~~~~~~~~~~~~~~~~~~
#2001 country of birth----

#Get the data
cob01 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/01_OA.csv")

#Get the intersect geog:
its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:ncol(cob01))#from dta

result <- moveData(its01,cob01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "2001_CountryOfBirthRecode_91LBS_noZeroPCS", driver="ESRI Shapefile", overwrite_layer = T)

#Better column abbreviation
writeSpatialShape(result,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~~~~
#2011 country of birth----

#Get the data
cob11 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/11_OA.csv")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:ncol(cob11))#from dta

result <- moveData(its11,cob11,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "2001_CountryOfBirthRecode_91LBS_noZeroPCS", driver="ESRI Shapefile", overwrite_layer = T)

#Better column abbreviation
writeSpatialShape(result,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Saving GB COB shapefiles and CSVs with a total pop column added----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Harmonised shapefiles----
cobs <- lapply(list.files(path='StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth',full.names = T,pattern = '*.shp'), function(x) readShapeSpatial(x))

lapply(cobs,names)

#Ah - don't dplyr spatial tings!
#cobs2 <- lapply(cobs,function(x) x %>% mutate(totalPop = rowSums(.[4:34])))

#Not pass by reference. Also silly. Java thinking, oops.
#lapply(cobs,function(x) x@data$totalPop <- apply(x@data[,4:34],1,sum))

for(i in 1:3){
  cobs[[i]]@data$totalPop <- apply(cobs[[i]]@data[,4:ncol(cobs[[i]]@data)],1,sum)
}

savens <- c('91CoBwithTotalPop','01CoBwithTotalPop','11CoBwithTotalPop')

lapply(1:length(savens),function(x) writeSpatialShape(cobs[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/",savens[x],".shp")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Each of the census CSVs (same CoB category, not same geography)----

cobs <- lapply(list.files(path='VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS',full.names = T,pattern = '*.csv'), function(x) read_csv(x))

#Watch out, loads in this order:01 / 11 / 91
#One ID column please
cobs[[3]] <- cobs[[3]][,c(1,3:ncol(cobs[[3]]))]

#Ah - don't dplyr spatial tings!
#cobs2 <- lapply(cobs,function(x) x %>% mutate(totalPop = rowSums(.[4:34])))

#Not pass by reference. Also silly. Java thinking, oops.
#lapply(cobs,function(x) x@data$totalPop <- apply(x@data[,4:34],1,sum))

for(i in 1:3){
  cobs[[i]]$totalPop <- apply(cobs[[i]][,2:ncol(cobs[[i]])],1,sum)
}

savens <- c('01OA_CoBwithTotalPop','11OA_CoBwithTotalPop','91LBS_CoBwithTotalPop')

lapply(1:length(savens),function(x) write_csv(cobs[[x]],paste0("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/",savens[x],".csv")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SOME CHECKS ON ENG VS SCOTS ZONE SIZES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CoB_GB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','11CoBwithTotalPop')

#area per zone
CoB_GB$area <- gArea(CoB_GB,byid=T)/1000000

#sum(CoB_GB$area)
#Which should be metres, right? COnverted to KM. Err. Yup, that's the right ballpark.

CoB_GB$popPerKM2 <- CoB_GB$totalPop/CoB_GB$area

#About right? Probably.
ggplot(CoB_GB@data,aes(x = 1, y = popPerKM2)) +
  geom_boxplot() +
  scale_y_log10()


#Is there an easy way to tell the nations apart from the zone codes?
#I would say no! Need the originals
sample(CoB_GB$label,50)

scot <- readOGR('C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount','pseudoPCS_aggregated4CorrectCount')

engWales <- readOGR('C:/Data/MapPolygons/EnglandWalesMerged/1991/wardsAggForCorrectCount','wardsEngWales_aggregated4CorrectCount')

CoB_GB$country <- 'Scotland'
CoB_GB$country[CoB_GB$label %in% engWales$label] <- 'EngWales'

table(CoB_GB$country)

#So those >1000 per km2 as urban-ish...
moreThan <- CoB_GB[CoB_GB$popPerKM2 > 1000,]

#Size distribution across the countries for these?
ggplot(moreThan@data, aes(x = country, y = area)) +
  geom_boxplot() +
  scale_y_log10()

#what were the original scots countries?
orig <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.csv')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~FIVE CENSUS GB COUNTRY OF BIRTH PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names <- read.csv("VariableCoding/1971to2011_CountryOfBirthCategories_Scotland.csv", check.names = F, as.is = T)

#Updates:
#1991 LBS: Caribbean under Americas needs distinguishing from Caribbean/New Commonwealth
names[35,4] <- "Caribbean [heading]"

#Apply those labels to column names, checking they match as we go along

#1971----

#Able to download 71 and 81 for the whole of GB. Is the data all there...?
seventyOne <- read_csv('1971/GreatBritain/greatBritain_CoB1971.csv')

#Yup, all values populated by the look of it.
lapply(names(seventyOne), function(x) table(0 + is.na(seventyOne[,x])) )

#And country codes easy to pick out? Yup!
#e       s       w 
#103129  15890   6457 
table(substring(seventyOne$`Zone Code`,1,1))

#So this has male-female and we want to add country names.
#We've done this before for Scotland with exactly the same data...
#In CoB_columnNameUpdate.R


####
#First-up: sum male/female into single columns
#Just column-pair sums e.g. 439/400 are England, male/female
#Staring at casweb to check.
all71 <- seventyOne[,c(1,2)]

#Take pairs of male/female columns, sum them into new
for(n in seq(from = 3, to = 34, by = 2)){
  
  all71[,as.character(n)] <- seventyOne[,n] + seventyOne[,(n+1)]
  
}

#First two remain the same
names(all71) <- c(names(all71)[c(1:2)],names[1:16,1])

#remove 'new commonwealth' - it's a sum column, we don't want those
all71 <- all71[,-9]

#OK, checked. That's named and summed...
write_csv(all71,"1971/GreatBritain/1971_GreatBritain_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv")

#1981----

#Doesn't need m/f columns summing. Let's check the rest of it.
eightyOne <- read_csv('1981/GreatBritain/greatBritain_CoB1981.csv')

#Yup, all values populated by the look of it.
lapply(names(eightyOne), function(x) table(0 + is.na(eightyOne[,x])) )

#Trickier to pick out which country is which... No country-specific subcode
eightyOne$`Zone ID`[sample(100)]
#But we don't necessarily need to do that as long as it does match the GB
#zone shapefile. (Which needs collating still...)

#Staring at casweb table to check...
#two total columns to exclude: 320 is all. 341 is new commonwealth. Leaves 19.
eightyOne <- eightyOne[,-c(2,9)]

#Double-check the match is correct. Tick.
namecheck81 <- data.frame(codes = names(eightyOne)[2:19], names = names$`1981`[c(1:6,8:19)])

names(eightyOne) <- c("Zone ID",as.character(namecheck81$names))

#Done. Save.
write_csv(eightyOne,"1981/GreatBritain/1981_GreatBritain_CoB_EnumDistrict_countryNamesAdded.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checks for 5-census GB COB harmonisation----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#First, can I use the GB 91-11 harmonised I've already done? What does it look like?
#91 will be the same as 01 and 11 here.
GB91_COB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch') %>% 
  data.frame()

#Put those together so I can load in Excel and look
seventyOne <- read_csv('1971/GreatBritain/1971_GreatBritain_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv') 
eightyOne <- read_csv('1981/GreatBritain/1981_GreatBritain_CoB_EnumDistrict_countryNamesAdded.csv')

names(seventyOne)[3:17] %>% length
names(eightyOne)[2:19] %>% length
names(GB91_COB)[4:36] %>% length

gbCoBs <- data.frame(
  seventyOne = c(names(seventyOne)[3:17],rep(' ',33-15)),
  eightyOne = c(names(eightyOne)[2:19],rep(' ',33-18)),
  ninetyOneTo11 = names(GB91_COB)[4:36]
)

#Actually want row names for a change
write.csv(gbCoBs,'VariableCoding/GB_5Census_91to11alreadyHarmonisedFrom3census.csv')

#Remind me what 5-census categories we had just for Scotland?
scots5censusCOB <- read_csv('C:/Users/SMI2/Dropbox/SheffieldMethodsInstitute/Census_DX/StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/1971_CoB_from_71EDs_to_91_aggPostcodeSectors.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 CENSUS GB PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Where I will be keeping to definite cats so a larger 'other' category than before
#And harmonising with existing GB 91-11 CoBs
#(GB data for 71/81 can be downloaded as is; the others are a pain)

#note that only the resulting 71 and 81 should need to then be geographically reassigned
#As 91-11 have already been done (10182 zones)
#But we still need to harmonise CoB across them all.

#So getting all that data and having a look again
seventyOne <- read_csv('1971/GreatBritain/1971_GreatBritain_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv') 
eightyOne <- read_csv('1981/GreatBritain/1981_GreatBritain_CoB_EnumDistrict_countryNamesAdded.csv')

ninetyOne <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp') %>% data.frame()

OhOne <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp') %>% data.frame()

Eleven <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp') %>% data.frame()

#Check the names are in the same order for the last three... yup
names(ninetyOne) == names(OhOne)
names(ninetyOne) == names(Eleven)

#FIVE CENSUS RECODE
#Recoding all of them here, no automation
fiveCensusCombo <- list(
  list(seventyOne, eightyOne, ninetyOne, OhOne, Eleven),#all data to sum columns on
  list('1,2','1','1,2,3','1,2,3','1,2,3'),#columns to keep from those
  list('England','3','2','15','15','15'),
  list('Scotland','4','3','16','16','16'),
  list('Wales','5','4','17','17','17'),
  list('Irish Republic','7','6','6','6','6'),
  list('Rest of UK','6','5','4,5,18','4,5,18','4,5,18'),
  list('India','12','11','24','24','24'),
  list('Pakistan','13','16','25','25','25'),
  list('Europe','16','17,18','12,13,27:32','12,13,27:32','12,13,27:32'),
  list('Rest of world','8:11,14,15,17','7:10,12:15,19','7:11,14,19:23,26,33:36','7:11,14,19:23,26,33:36','7:11,14,19:23,26,33:36')#North Africa has to go here due to 2011 mismatch in Eng/Scot. South-East Asia has to go here too. 'Other Asia' 91 is fine here too, see 91 defs.
)

#Then work out what's left.
#check what that looks like
for(i in 3:length(fiveCensusCombo)){
  
  print(paste(i,
              fiveCensusCombo[i][[1]][1],
              fiveCensusCombo[i][[1]][2],
              fiveCensusCombo[i][[1]][3],
              fiveCensusCombo[i][[1]][4],
              fiveCensusCombo[i][[1]][5],
              fiveCensusCombo[i][[1]][6],
              sep = " "))
  
}

#Returns list of re-coded dataframes
#"Error: column_recode_lists_are_all_correct_length not equal to TRUE"
#Means lists defining recodes aren't all of a length that recodes in each dataframe
results <- recode(fiveCensusCombo)

looksee <- results[[4]]

#Save as five dataframes, ready for geog re-assigning (apart from '91 which is correct already)
savens <- c('71ED','81ED','91LBS_PCS','01LBS_PCS','11LBS_PCS')

lapply(1:length(savens),function(x) write.csv(results[[x]],
                                              paste0("VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/",savens[x],".csv"),
                                              row.names = F))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Processing GB 71 and 81 enumeration district shapefiles----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Already merged 3 countries in QGIS. Now just need to make sure we have single polygons per ID
#Load the GB files.
gb71eds <- readOGR('C:/Data/MapPolygons/Merges/1971GB','1971GB_enumerationDistrictMerge')
gb81eds <- readOGR('C:/Data/MapPolygons/Merges/1981GB','1981GB_enumerationDistrictMerge')

gb71eds_dissolveByID <- IDtoSingleRow(gb71eds,1)
gb81eds_dissolveByID <- IDtoSingleRow(gb81eds,1)

writeOGR(gb71eds_dissolveByID, "C:/Data/MapPolygons/GreatBritain/1971", "gb71eds_dissolvedByID", driver="ESRI Shapefile", overwrite_layer=TRUE)

writeOGR(gb81eds_dissolveByID, "C:/Data/MapPolygons/GreatBritain/1981", "gb81eds_dissolvedByID", driver="ESRI Shapefile", overwrite_layer=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Next: CoB-recoded 71 and 81 need aggregating to 91 new-PCS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#~~~~~~~~~~~~~~~~~~
#1971 COB----

#Get the data
cob71 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/71ED.csv")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(3:ncol(cob71))#from dta

result <- moveData(its71,cob71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#Do numbers match?
apply(cob71[,3:ncol(cob71)],2,sum)
df <- data.frame(result)
apply(df[,3:ncol(df)],2,sum)

#Some are NA. Same zones? Yup... Just the island off Barrow-in-Furness.
dfna <- df[is.na(df$England),]
dropz <- dfna$label %>% as.character()

#Drop those two.
result2 <- result[!(result$label %in% dropz),]

#Check again
apply(cob71[,3:ncol(cob71)],2,sum)
apply(data.frame(result2)[,3:ncol(result2)],2,sum)

writeSpatialShape(result2,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1971_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~
#1981 COB----

#Get the data
cob81 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/81ED.csv")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:ncol(cob81))#from dta

result <- moveData(its81,cob81,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#Do numbers match?
apply(cob81[,2:ncol(cob81)],2,sum)
df <- data.frame(result)
apply(df[,3:ncol(df)],2,sum)

#We didn't lose those two zones this time but may need to later to keep everything matching.

#Better column abbreviation
writeSpatialShape(result,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1981_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 Census CoB: get them all matching----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Includes checking 91 to 11 are actually OK and checking we have all the same zones.
cob71shp <- readShapeSpatial("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1971_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")
cob81shp <- readShapeSpatial("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1981_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

cob71 <- cob71shp %>% data.frame
cob81 <- cob81shp %>% data.frame

#91 to 11 in CSV form - only needed CoB cats combining, were already in correct geog (I think. We'll see...)
cob91 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/91LBS_PCS.csv')
cob01 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/01LBS_PCS.csv')
cob11 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/11LBS_PCS.csv')

#Issue of those two zones missing in 71. OK to drop them from the rest I think. And they were?
#17FFFM 17FFFN
rmz <- cob81$label[!(cob81$label) %in% (cob71$label)]

cob81 <- cob81[!(cob81$label %in% rmz),]
cob91 <- cob91[!(cob91$label %in% rmz),]
cob01 <- cob01[!(cob01$label %in% rmz),]
cob11 <- cob11[!(cob11$label %in% rmz),]

#OK, all the same now, 10180 zones in total
table(cob71$label %in% cob91$label)

#Let's harmonise column names. Shapefile ones were abbreviated but they're all the same
lapply(list(cob71,cob81,cob91,cob01,cob11), names)
#Ah, keep on forgetting we're not doing pass by reference! Java head.
#lapply(list(cob71,cob81,cob91,cob01,cob11), function(x) names(x) <- names(cob11))

names(cob71) <- names(cob11)
names(cob81) <- names(cob11)

#Should probably save all those
write_csv(cob71,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/71LBS_PCS_sameZoneNumber.csv')
write_csv(cob81,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/81LBS_PCS_sameZoneNumber.csv')
write_csv(cob91,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/91LBS_PCS_sameZoneNumber.csv')
write_csv(cob01,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/01LBS_PCS_sameZoneNumber.csv')
write_csv(cob11,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/11LBS_PCS_sameZoneNumber.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert 3 census CoBs to CSVs----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cob91three <- 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp'
cob01three <- 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp'
cob11three <- 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp'

cobzThree <- lapply(list(cob91three,cob01three,cob11three), function(x) readShapeSpatial(x) %>% data.frame)

#Keerect
lapply(cobzThree, nrow)

namez <- c('91CoB_threeCensus','01CoB_threeCensus','11CoB_threeCensus')

lapply(1:length(namez),function(x) write_csv(cobzThree[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/",namez[x],".csv")))