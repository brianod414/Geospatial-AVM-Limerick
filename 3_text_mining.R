###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Mining the descriptions 
## Date: 19/10/2023
## Author: BOD
###########################################################################

hd <- readr::read_csv("./Cleaned Data/house_apartment_data_limerick.csv")

dim(hd)
colSums(is.na(hd))

no_info_data <- hd[is.na(hd$description) & is.na(hd$property_type_original) & is.na(hd$beds) & is.na(hd$baths), ]
dim(no_info)

# removing 5888 rows with no bed, bath, proprty type, and no description ot mine it. left with 6882
hd <- hd[!(hd$id %in% no_info$id), ]
dim(hd)


######## Mining Data file 

table(hd$property_type_original)
colnames(hd)
table(hd$property_category, hd$property_type, useNA = 'ifany')

description_valid <- hd[!is.na(hd$description), ]
hd$complete_data <- ifelse(!is.na(hd$description), TRUE, FALSE)
dim(description_valid)

non_mining_data <- hd[!hd$complete_data, ]
dim(non_mining_data)

mining_data <- hd[hd$complete_data, ]
mining_data$remove <- NA 

dim(mining_data)
table(mining_data$property_category, useNA = 'ifany')

write.csv(mining_data, "Cleaned Data/mining_data.csv", row.names = FALSE)


######## NA Property Categories 

na_category_mining_data <- mining_data[is.na(mining_data$property_category), ]
colnames(na_category_mining_data)


dim(na_category_mining_data)
# 30 have no categories, 1 is a site. 
site_match <- grepl("full planning permission", na_category_mining_data$description, ignore.case = TRUE)
table(site_match)
na_category_mining_data$property_category[site_match] <- 'Land'
# house, home, Semi - Detached Residence, two storey residence, 

# houses. 
house_match <- grepl("house|home|Semi - Detached residence|two storey Residence|cottage|Detached Four bedroom property", na_category_mining_data$description, ignore.case = TRUE)
table(house_match)
na_category_mining_data$property_category[is.na(na_category_mining_data$property_category) & house_match] <- 'House'
table(na_category_mining_data$property_category, useNA = 'ifany')

# 5 are apartments 
na_category_mining_data$property_category[is.na(na_category_mining_data$property_category)] <- 'Apartment'
table(na_category_mining_data$property_category, useNA = 'ifany')

table(mining_data$property_category, useNA = 'ifany')
id_category_update <- tibble(id = na_category_mining_data$id, category = na_category_mining_data$property_category)
mining_data$property_category[mining_data$id %in% id_category_update$id] <- na_category_mining_data$property_category

#now No NA in category for descriptions, 

# Checking property Categories 
mining_data_apt <- (mining_data[mining_data$property_category == 'Apartment', ])
dim(mining_data_apt)
mining_data_houses <- mining_data[mining_data$property_category == "House", ]
dim(mining_data_houses)


apt_match <- grepl("apartment", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$mined_apartment <- NA
mining_data_houses$mined_apartment[apt_match]<- TRUE
#view(mining_data_houses[mining_data_houses$apt_match, ])
apt_match <- grepl("14 x Two bedroom apartments", mining_data_houses$description, ignore.case = TRUE)
#view(mining_data_houses[apt_match, ])
table(apt_match)
# 7 apartments in the house category 
mining_data_houses$property_category[apt_match] <- "Apartment"
mining_data_houses$is_apartment[apt_match] <- TRUE


# 2 apartments are actually 8x2 bed apartment for sale. Dividing the total lot by 8 to get an estimate of unit prices 
321973
320974
mining_data$size[mining_data$id ==321973 ] <- mining_data$size[mining_data$id ==321973 ]/8
mining_data$size[mining_data$id ==320974 ] <-mining_data$size[mining_data$id ==320974 ]/8
mining_data$price[mining_data$id ==321973 ] <-mining_data$price[mining_data$id ==321973 ]/8
mining_data$price[mining_data$id ==320974 ] <-mining_data$price[mining_data$id ==320974 ]/8
mining_data$advertised_price[mining_data$id ==321973 ] <-mining_data$advertised_price[mining_data$id ==321973 ]/8
mining_data$advertised_price[mining_data$id ==320974 ] <-mining_data$advertised_price[mining_data$id ==320974 ]/8



house_match <- grepl("house", mining_data_apt$description, ignore.case = TRUE)
# no changes 



# update the category 
mining_data$property_category[mining_data$id %in% mining_data_houses$id] <- mining_data_houses$property_category
mining_data$is_apartment <- ifelse(mining_data$property_category == 'Apartment', TRUE, FALSE)
mining_data$remove[mining_data$property_category == 'Land'] <- TRUE


################################
# House Types 
################################
table(mining_data_houses$property_type, useNA = 'ifany')
mining_data_houses$property_type2 <- NA
# end of terrace
et_match <- grepl("end of terrace|end of a terrace|end terraced", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[et_match] <- "End of Terrace"
table(mining_data_houses$property_type2, mining_data_houses$property_type)
#view(mining_data_houses[mining_data_houses$property_type2=='End of Terrace' & mining_data_houses$property_type != 'End of Terrace House', ])
table(mining_data_houses$property_type2, useNA = 'ifany')

# semi-detatched
# semi detatched
# semi- detatched
sd_match <- grepl("semi\\s*-?\\s*detached|simi detached|semi- detached|semi detatched|semi- detatched", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & sd_match] <- "Semi-Detached"
table(mining_data_houses$property_type2, mining_data_houses$property_type)
#view(mining_data_houses[mining_data_houses$property_type2=='Semi-Detached' & mining_data_houses$property_type != 'Semi-Detached House', ])

# Terrace
terrace_match <- grepl("terrac[ee]d|mid terrace|terrace", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & terrace_match] <- "Terraced"
table(mining_data_houses$property_type2, mining_data_houses$property_type)
#view(mining_data_houses[mining_data_houses$property_type2=='Terraced' & mining_data_houses$property_type != 'Terraced House', ])

# Townhouse
detach_match <- grepl("townhouse", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & detach_match] <- "Townhouse"
table(mining_data_houses$property_type2, mining_data_houses$property_type)

# Country House
detach_match <- grepl("country house|cottage|farmhouse", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & detach_match] <- "Detached"


# Detatched
detach_match <- grepl("detached", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & detach_match] <- "Detached"
table(mining_data_houses$property_type2, mining_data_houses$property_type)
#view(mining_data_houses[mining_data_houses$property_type2 == 'Detached' & mining_data_houses$property_type != 'Detached House', ])

mining_data_houses$property_type2[mining_data_houses$id == 271386] <- 'Terraced'
mining_data_houses$property_type2[mining_data_houses$id == 290652] <- 'Semi-Detached'


table(mining_data_houses$property_type2, useNA = 'ifany')
table(mining_data_houses$property_type, mining_data_houses$property_type2)

table(mining_data_houses$property_type[is.na(mining_data_houses$property_type2)])
#view(mining_data_houses[is.na(mining_data_houses$property_type2)])
commercial_match <- grepl("Residential/shop|residential/commercial", mining_data_houses$description, ignore.case = TRUE)
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & commercial_match] <- "Commercial"
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & mining_data_houses$property_type == 'Detached House'] <- 'Detached'
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & mining_data_houses$property_type == 'End of Terrace House'] <- 'End of Terrace'
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & mining_data_houses$property_type == 'Semi-Detached House'] <- 'Semi-Detached'
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & mining_data_houses$property_type == 'Townhouse'] <- 'Townhouse'
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2) & mining_data_houses$property_type == 'Terraced House'] <- 'Terraced'

# remaining bungalows are on large sites in the countryside, detached 
mining_data_houses$property_type2[is.na(mining_data_houses$property_type2)] <- 'Detached'
#view(mining_data_houses[is.na(mining_data_houses$property_type2), ])


dim(mining_data_houses)
colSums(is.na(mining_data_houses))
table(mining_data_houses$property_type, mining_data_houses$property_type2)
table(mining_data_houses$property_type2, useNA = 'ifany')

# update the mining_data with mined_types 
mining_data$property_type_mined <- NA
mining_data$property_type_mined[mining_data$id %in% mining_data_houses$id] <- mining_data_houses$property_type2
table(mining_data$property_type_mined)




################ Apartments  


table(mining_data_apt$property_type, useNA = 'ifany')
#view(mining_data_apt[is.na(mining_data_apt$property_type), ])
mining_data_apt$property_type2 <- mining_data_apt$property_type
mining_data_apt$property_type2[is.na(mining_data_apt$property_type) ] <- 'Apartment'
table(mining_data_apt$property_type2, useNA = 'ifany')

# update the mining_data with mined_types 
mining_data$property_type_mined[mining_data$id %in% mining_data_apt$id] <- mining_data_apt$property_type2
table(mining_data$property_type_mined[mining_data$is_apartment], useNA = 'ifany')
mining_data$property_type_mined[mining_data$is_apartment & mining_data$property_type_mined=="Terraced"] <- 'Apartment'


colSums(is.na(mining_data))



# Missing bedrooms 
#view(mining_data[is.na(mining_data$beds), ])
bed_mine <- sub(".*?(\\b\\w+|\\d+)\\s*bed.*", "\\1", hd$beds)
mining_data$beds[mining_data$id %in% c(278153,
                                       327617,
                                       282937)] <- 3
# remove 3 sites
mining_data$remove[is.na(mining_data$beds)] <- TRUE
mining_data$remove[is.na(mining_data$remove)] <- FALSE
table(mining_data$remove)



#view(mining_data[is.na(mining_data$baths) & !mining_data$remove, ])
bath_count <- sapply(regmatches(hd$bath, gregexpr("\\bbath\\b", hd$bath)), length)

bathrooms <- tibble(id = c(333955,303011,300180, 278153,287244,241629, 318502, 278542, 263113, 267658,267658, 318040, 319794, 325317, 247432, 271743, 327617, 303854, 327523, 282937, 254488), 
                    bath = c(2, 2, 2, 2, 3, 1, 1, 4, 1, 1, 1, 2, 1, 2, 1, 3, 2, 2, 3, 1, 1))
mining_data$baths <- ifelse(mining_data$id %in% bathrooms$id, bathrooms$bath, mining_data$baths)
mining_data$baths[is.na(mining_data$baths) & !mining_data$remove] <- 0

# outlier in bedroom and bathroom, 10 bathrooms actully 1. 4 Rooms marked as bedooms, actually 3
mining_data$beds[mining_data$id== 327762] <- 3
mining_data$baths[mining_data$id== 327762] <- 1


colSums(is.na(mining_data[!mining_data$remove, ]))
dim(mining_data)





# tidy up file 
colnames(mining_data)
mining_data$property_type <- mining_data$property_type_mined
table(mining_data$property_type[mining_data$is_apartment])
table(mining_data$property_type[!mining_data$is_apartment])

# Remove Commerical 
mining_data$remove[mining_data$property_type == "Commercial"] <- TRUE

mining_data <- mining_data[!mining_data$remove, ]
dim(mining_data)

join_mined <- select(mining_data, -c("remove", "property_type_mined"))

dim(join_mined)
colnames(join_mined)
colnames(non_mining_data)
dim(non_mining_data)

hd_mined <- rbind(join_mined, non_mining_data)

dim(hd_mined)
colnames(hd_mined)

table(hd_mined$property_type, useNA = 'ifany')




write.csv(hd_mined, "Cleaned Data/houses_apartments_limerick_mined.csv", row.names = FALSE)




######## Property Quality 
hd$quality <- NA
derelict_match <- grepl("Derelict", hd$description, ignore.case = TRUE)
derelict_id <- (subset(hd$id, derelict_match))
# 285084 not derelict 
derelict_id <- derelict_id[-2]
hd$quality[hd$id %in% derelict_id] <- 'Derelict'

renovate_match <- grepl("renovation", hd$description, ignore.case = TRUE)
renovation_id <- (subset(hd$id, renovate_match))
# renovated = 'beautifully renovated'|'recently renovated'|'carefully renovated'
# 283120 only garage needs renovation, the rest are 'in need of renovation', 'complete renovation'
renovation_id <- renovation_id[renovation_id != 283120]
hd$quality[hd$id %in% renovation_id] <- 'Renovation'

mining_data <- hd[is.na(hd$quality),]
dim(mining_data)
match <- grepl("redecora", mining_data$description, ignore.case = TRUE)
# in need of refurbishmant = 244221, the rest are 'recently redecorated'
hd$quality[hd$id==244221] <- 'Renovation'

match <- grepl("refurbishment", mining_data$description, ignore.case = TRUE)
refurbishment_id <- c(272247,268475,282792,280616,288682,307879,245508,253938,248778,308873,283545)
hd$quality[hd$id %in% refurbishment_id] <- 'Renovation'

# properites in need of repair - some say 'decking in need of repair'. Only two houses in need of repair 
repair_id <- c(267016,	253603)
hd$quality[hd$id %in% repair_id] <- 'Renovation'

# properties can be 'upgraded'
upgrade_id <- c(316944,293702)
hd$quality[hd$id %in% upgrade_id] <- 'Renovation'

check_quality <- hd[!is.na(hd$quality),]
match <- grepl("extensive|complete|builders", check_quality$description, ignore.case = TRUE)
# properties in need of extensive/complete renovation, and those requiring building work 
match_id <- subset(check_quality$id, match)
match_id <- match_id[match_id != 264597]
hd$quality[hd$id %in% match_id] <- 'Derelict'

# adding properties that list no info on rooms/internal features as derelict 
derelict_id <- c(256139, 263711, 263711, 307879, 271764)
hd$quality[hd$id %in% derelict_id] <- 'Derelict'

# combine property condition (new, second hand) with quality. since all derelict or needreno properties are second hand 
hd$qualityo <- hd$quality

table(hd$property_condition, useNA = 'ifany')
table(hd$quality, useNA = 'ifany')


hd$quality[is.na(hd$quality)] <- hd$property_condition[is.na(hd$quality)]

table(hd$quality, hd$property_condition, useNA = 'ifany')

hd <- select(hd, -c('qualityo', 'property_condition'))
names(hd)[names(hd) == 'quality'] <- 'property_condition'



####### BER 

# only 6/8 properties with listed BER in the description. Others have BER:... missing text.

######## Key Terms 


# cul de sac
cds_match <- grepl("cul de sac|cul-de-sac|cul the sac|cul des ac|cul dec sac|de sac", hd$description, ignore.case = TRUE)
sum(cds_match)
cds_id <- subset(hd$id, cds_match)
hd$cul_de_sac <- FALSE
hd$cul_de_sac[hd$id %in% cds_id] <- TRUE
table(hd$cul_de_sac)


# fireplace

fp_match <- grepl("fireplace|fire place|mantelpiece|stove|open fire|electric fire", hd$description, ignore.case = TRUE)
sum(fp_match)
fp_id <- subset(hd$id, fp_match)
hd$fireplace <- FALSE
hd$fireplace[hd$id %in% fp_id] <- TRUE

mining <- hd[hd$fireplace==FALSE, ]
dim(mining)
fp_match <- grepl("\\bfire\\b.*\\bsurround\\b.*\\.", mining$description, ignore.case = TRUE, perl = TRUE)
fp_id <- subset(mining$id, fp_match)
hd$fireplace[hd$id %in% fp_id] <- TRUE

fp_match <- grepl(" fire ", mining$description, ignore.case = TRUE)
sum(fp_match)
fp_id <- subset(mining$id, fp_match)
hd$fireplace[hd$id %in% fp_id] <- TRUE

no_fp_match <- grepl("\\bGas Fire central heating\\b|\\bfire damaged\\b", mining$description, ignore.case = TRUE)
no_fp_id <- subset(mining$id, no_fp_match)
hd$fireplace[hd$id %in% no_fp_id] <- FALSE


# double glazing

dg_match <- grepl("double\\s*-?\\s*glazing|double\\s*-?\\s*glazed|d.g window|double glaze|PVC", hd$description, ignore.case = TRUE)
sum(dg_match)
dg_id <- subset(hd$id, dg_match)
hd$glazing <- NA
hd$glazing[hd$id %in% dg_id] <- "Double"

tg_match <- grepl("triple\\s*-?\\s*glazing|triple\\s*-?\\s*glazed", hd$description, ignore.case = TRUE)
sum(tg_match)
tg_id <- subset(hd$id, tg_match)
hd$glazing[hd$id %in% tg_id] <- "Triple"

sg_match <- grepl("single\\s*-?\\s*glazing|single\\s*-?\\s*glazed", hd$description, ignore.case = TRUE)
sum(sg_match)
sg_id <- subset(hd$id, sg_match)
hd$glazing[hd$id %in% sg_id] <- "Single"


mining <- hd[is.na(hd$glazing), ]
dim(mining)
dg_match <- grepl("window", mining$description, ignore.case = TRUE)
# may be easier to use BER rather than windows. Many properties have 'new windows' or 'big windows'
hd <- select(hd, -c("glazing"))

# Garage

garage_match <- grepl("barn|lean\\s*-?\\s*to|leanto|\\bgarage|\\bshed|\\bout\\s*-?\\s*building|outbuilding|outhouse|\\bout\\s*-?\\s*house", hd$description, ignore.case = TRUE)
garage_id <- (subset(hd$id, garage_match))
hd$garage <- FALSE
hd$garage[hd$id %in% garage_id] <- TRUE


# Parking 

parking_match <- grepl("\\bdriveway\\b|drive\\s*-?\\s*way|\\bparking\\b", hd$description, ignore.case = TRUE)
parking_id <- (subset(hd$id, parking_match))
hd$parking <- FALSE
hd$parking[hd$id %in% parking_id] <- TRUE

# set all semi/detached houses outside the city to have parking. 
hd$parking[hd$municipality != 'Limerick City and Suburbs' & hd$property_type == 'Detached'] <- TRUE 
hd$parking[hd$municipality != 'Limerick City and Suburbs' & hd$property_type == 'Semi-Detached'] <- TRUE 
# viewing those with no parking, many are detached houses in rural areas, many are in estates with parking -> cannot say there is no parking as it is not standard to mention parking in Limerick properties. 


# Period House 

period_match <- grepl("period property|period residence|period home|period 3|period style 3 bedroom", hd$description, ignore.case = TRUE)
period_id <- (subset(hd$id, period_match))

georgian_victorian_match <- grepl("georgian residence|georgian carriage|edwardian|victorian", hd$description, ignore.case = TRUE)
georgian_victorian_id <- (subset(hd$id, georgian_victorian_match))

hd$period <- FALSE
hd$period[hd$id %in% period_id | hd$id %in% georgian_victorian_id] <- TRUE

mean(hd$price)
mean(hd$price[!hd$period])


