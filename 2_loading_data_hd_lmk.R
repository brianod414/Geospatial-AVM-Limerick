###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Loading the Limerick data 2010-2018
## Date: 19/10/2023
## Author: BOD
###########################################################################


#############################
## Loading Data & Packages 
#############################

library(tidyverse)
library(janitor)
library(cowplot) # has a nice theme for ggplot and allows you to combine multiple
library(leaflet)
library(xtable)
library(osmdata)
library(ggmap)
register_google(key = "AIzaSyC13XPJ_YS5kqjdYELM206IkI7r89Iou5s")
library(stringr)
library(geosphere)

hd <- readr::read_csv('./Raw Data/limerick_property_data.csv')
dim(hd)


##############################################################################################################################
## Size and Checks 
##############################################################################################################################

head(hd)
dim(hd)

# clean column names
hd <- hd %>% clean_names()
colnames(hd)

# Count Missing Values
colSums(is.na(hd))

# No Duplicate rows 
any(duplicated(hd))
hd_duplicates <- hd %>%
  filter(duplicated(id) | duplicated(id, fromLast = TRUE))
dim(hd_duplicates)


##############################################################################################################################
## Sample and Subsets 
##############################################################################################################################

hd_sample <- hd[sample(nrow(hd), floor(nrow(hd)*0.1)),]

no_info <- hd[is.na(hd$description) & is.na(hd$beds) & is.na(hd$baths) & is.na(hd$property_type) & is.na(hd$property_type_original),]
dim(no_info)
colSums(is.na(no_info))


##############################################################################################################################
## Data Types  
##############################################################################################################################

# Convert numerical columns to INT
hd$id <- as.integer(hd$id)
hd$price <- as.integer(hd$price)
hd$present_price <- as.integer(hd$present_price)
hd$size <- as.integer(hd$size)
hd$days_to_sell <- as.integer(hd$days_to_sell)
hd$no_vat_price <- as.integer(hd$no_vat_price)
hd$beds <- as.integer(hd$beds)
hd$baths <- as.integer(hd$baths)
hd$advertised_price <- as.integer(hd$advertised_price)
hd$months <- as.integer(hd$months)
hd$years <- as.integer(hd$years)

# Convert Datetime entries to objects 
hd$date_advertised <- as.POSIXct(hd$date_advertised, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
hd$sale_date <- as.POSIXct(hd$sale_date, format = "%d/%m/%Y %H:%M", tz = "UTC")


##############################################################################################################################
## Cleaning Columns 
##############################################################################################################################

colnames(hd)
# Remove unnecessary columns - company checks
hd <- select(hd, -c('x1', "geo_code_api_tried", "agency", "is_inferred_data", "has_perfect_match"))

# write pre-cleaning data
write.csv(hd, "Raw Data/pre_cleaning_data.csv", row.names = FALSE)

### id
# No NA's or duplicates 
summary(hd$id) 

### county
# just limerick
table(hd$county, useNA = "ifany")
length(unique(hd$county))

### area
table(hd$area, useNA = "ifany")

### region 
table(hd$region, useNA = "ifany")
# remove the clare
hd <- hd[hd$region != 'Clare County',]

### postal_code
table(hd$postal_code, useNA = "ifany")

###  not_full_market_price - meaning? 
table(hd$not_full_market_price, useNA = 'ifany')

### inconsistent address is all false, can be removed
table(hd$inconsistent_address, useNA = "ifany")
hd <- select(hd, -c("inconsistent_address"))

### declared_county
dim(hd[!is.na(hd$declared_county), ])

# removing geocode_precision, fixed_address, and declared_county variables 
# internal proxies used by the company who supplied the data
hd <- select(hd, -c("geocode_precision", "fixed_address", "declared_county"))

### property_description
# better described as propery_condition [New, Second Hand]
# Reassign value in property_description
names(hd)[names(hd) == 'property_description'] <- 'property_condition'
hd$property_condition <- ifelse(hd$property_condition == "New Dwelling house /Apartment", 'New', "Second Hand")

### property type original
table(hd$property_type_original, useNA = 'ifany')
hd$property_type_original <- tolower(hd$property_type_original)
hd$property_type_original <- gsub("for sale", "", hd$property_type_original)
hd$property_type_original <- gsub("to let", "", hd$property_type_original)
hd$property_type_original <- gsub(" property", "", hd$property_type_original)
hd$property_type_original <- gsub("farm land", "agricultural land", hd$property_type_original)
hd$property_type_original <- trimws(hd$property_type_original, "right")

# extracting the site values
hd$site_size_values <- NA_character_
contains_acre <- grepl("acre", hd$property_type_original, ignore.case = TRUE)
hd$site_size_values[contains_acre] <- sub("(.*) acre site*", "\\1", hd$property_type_original[contains_acre])
hd$property_type_original <- sub("^[0-9.]+ acre ", "", hd$property_type_original)
summary(as.integer(hd$site_size_values))


#### property_type
table(hd$property_type, useNA = "ifany")
# better described as property_category [House, Apartment, Land,...]
# checking the property type original for the different property types
names(hd)[names(hd) == 'property_type'] <- 'property_category'
table(hd$property_type_original[(hd$property_category == 'House') ], useNA = 'ifany')
table(hd$property_type_original[(hd$property_category == 'Apartment') ], useNA = 'ifany')
table(hd$property_type_original[(hd$property_category == 'Land') ], useNA = 'ifany')

# NA values for property_category, mine values from the property type original to assign these to land, apartment, or house
table(hd$property_type_original[is.na(hd$property_category) ], useNA = 'ifany')

# assign any land related entries as land
indices_site <- which(is.na(hd$property_category) & (
  grepl("site", hd$property_type_original, ignore.case = TRUE)|
    grepl("land", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indices_site] <- 'Land'

# assign any apartment/penthouse to apartment
indices_apartment <- which(is.na(hd$property_category) & (
  grepl("apartment", hd$property_type_original, ignore.case = TRUE)|
    grepl("penthouse", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indices_apartment] <- 'Apartment'

# assign any house entries as a house
indices_house <- which(is.na(hd$property_category) & (
  grepl("house", hd$property_type_original, ignore.case = TRUE)|
    grepl("home", hd$property_type_original, ignore.case = TRUE)|
    grepl("cottage", hd$property_type_original, ignore.case = TRUE)|
    grepl("mews", hd$property_type_original, ignore.case = TRUE)|
    grepl("bungalow", hd$property_type_original, ignore.case = TRUE)|
    grepl("dormer", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indices_house] <- 'House'

# Creating a level for industrial properties
indeces_industrial <- which(is.na(hd$property_category) & (
  grepl("industrial", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indeces_industrial] <- 'Industrial'

# Creating a level for investment properties
indeces_investment <- which(is.na(hd$property_category) & (
  grepl("investment", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indeces_investment] <- 'Investment Property'

# Creating a level for commercial properties 
indeces_retail_commercial <- which(is.na(hd$property_category) & (
  grepl("retail", hd$property_type_original, ignore.case = TRUE)|
    grepl("businesses", hd$property_type_original, ignore.case = TRUE)|
    grepl("office", hd$property_type_original, ignore.case = TRUE)|
    grepl("Commercial", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indeces_retail_commercial] <- 'Commercial'

# Creating a level for hotels and restaurants
indeces_hospitatlity <- which(is.na(hd$property_category) & (
  grepl("hotel", hd$property_type_original, ignore.case = TRUE)|
    grepl("Restaurant", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indeces_hospitatlity] <- 'Hostpitality and Bars'

# Creating a level for new development 
indeces_new_development <- which(is.na(hd$property_category) & (
  grepl("new", hd$property_type_original, ignore.case = TRUE)))
hd$property_category[indeces_new_development] <- 'New Development'

table(hd$property_category, useNA = 'ifany')
dim(hd)

# Investment Properties do not have information on the beds, baths, ber, only 2 have descriptions and the prices 
# and mean price is twice that of the mean of all properties, hence it is hard to know what drives this price
# one description outlines a retail unit and accommodation, removing due to ambiguity. 
# 14 removed 

summary(hd$price[hd$property_category=='Investment Property'])
summary(hd$price)
hd <- subset(hd, property_category != 'Investment Property'  | is.na(property_category))
dim(hd)

# Industrial -  No info on beds, baths, description -> removed 
hd <- subset(hd, property_category != 'Industrial'  | is.na(property_category))

# Unknown property type for new developments, investigate if appt or house 
# Made up of common estates: "Bloomfield", "Caisleán Na hAbhann", all houses 
hd$property_condition[hd$property_category == "New Development"] <- "New"
hd$property_category[hd$property_category == "New Development"] <- 'House'

# Valid commercial units: office space, garagee. No relevance 
hd <- subset(hd, property_category != 'Commercial'  | is.na(property_category))

hd$property_category[hd$property_category == "Land" & grepl("residence", hd$description, ignore.case = TRUE)] <- "House"
# 1 house in the land category, comes with substantial land
# remove the rest of land: No relevance  
hd <- subset(hd, property_category != 'Land'  | is.na(property_category))
hd <- select(hd, -c("site_size_values"))

# Hospitality and Bars -  No info on beds, baths, description -> removed 
hd <- subset(hd, property_category != 'Hostpitality and Bars'  | is.na(property_category))



## property_type column for both Apartment and House
table(hd$property_type_original[hd$property_category == 'House'])
table(hd$property_type_original[hd$property_category == 'Apartment'])

hd <- hd %>%
  mutate(
    property_type = case_when(
      property_category == 'House' & grepl("Bungalow", property_type_original, ignore.case = TRUE) ~ "Bungalow",
      property_category == 'House' & grepl("Cottage", property_type_original, ignore.case = TRUE) ~ "Cottage",
      property_category == 'House' & grepl("Period House", property_type_original, ignore.case = TRUE) ~ "Period House",
      property_category == 'House' & grepl("Dormer", property_type_original, ignore.case = TRUE) ~ "Dormer",
      property_category == 'House' & grepl("Country House", property_type_original, ignore.case = TRUE) ~ "Country House",
      property_category == 'House' & grepl("Townhouse", property_type_original, ignore.case = TRUE) ~ "Townhouse",
      property_category == 'House' & (grepl("Terraced House", property_type_original, ignore.case = TRUE) |
                                        grepl("mews", property_type_original, ignore.case = TRUE)) ~ "Terraced House",
      property_category == 'House' & grepl(".*End of Terrace House", property_type_original, ignore.case = TRUE) ~ "End of Terrace House",
      property_category == 'House' & grepl("Semi-Detached House", property_type_original, ignore.case = TRUE) ~ "Semi-Detached House",
      property_category == 'House' & grepl("Detached House", property_type_original, ignore.case = TRUE) ~ "Detached House",
      property_category == 'House' & (grepl("house", property_type_original, ignore.case = TRUE) |
                                        grepl("site", property_type_original, ignore.case = TRUE)|
                                        grepl("new development", property_type_original, ignore.case = TRUE)|
                                        grepl("home", property_type_original, ignore.case = TRUE)) ~ "House",
      property_category == 'Apartment' & grepl("studio", property_type_original, ignore.case = TRUE) ~ "Studio Apartment",
      property_category == 'Apartment' & grepl("Duplex", property_type_original, ignore.case = TRUE) ~ "Duplex",
      property_category == 'Apartment' & (grepl("apartment", property_type_original, ignore.case = TRUE) |
                                            grepl("penthouse", property_type_original, ignore.case = TRUE)) ~ "Apartment",
      TRUE ~ NA_character_
    )
  )

table(hd$property_type[hd$property_category == 'House'])


#### is_apartment
# the property types for TRUE consist of land and business premises. 
# I use the information for property_type to reassign values 

# house and land are false 
hd$is_apartment[hd$property_category != "Apartment"] <-  FALSE
# Apartment is true 
hd$is_apartment[hd$property_category == "Apartment"]<-  TRUE
# There is overlap where business, hotels, investment are marked as apartments with property_category = NA
table(hd$is_apartment, useNA = 'ifany')
table(hd$property_category)
# Change any apartments with NA property types to FALSE
hd$is_apartment[hd$is_apartment == TRUE & ((hd$property_category != 'Apartment') | is.na(hd$property_category))]  <- FALSE


### property_size_description
table(hd$property_size_description, useNA = 'ifany')

### ber 
table(hd$ber, useNA = "ifany")



### present_price investigation
summary(hd$present_price)

###  size   
summary(hd$size)


# vat price and vat_exclusive
# # 13.5% VAT rate 
# vat_rate <- hd$price/hd$no_vat_price
# summary(vat_rate, digits= 6)
# 
# # VAT charged on new properties only
# table(hd$property_condition[hd$vat_exclusive])
# table(hd$property_condition)
# # 154 New dwellings have no VAT listed, calculate no_vat_price for these
# summary(hd$no_vat_price[hd$property_condition ==  "New" & hd$vat_exclusive == FALSE])
# replacement_vat <- hd$price[hd$property_condition ==  "New" & hd$vat_exclusive == FALSE]*(0.865)
# hd$no_vat_price[hd$property_condition ==  "New" & hd$vat_exclusive == FALSE] <- replacement_vat
# hd$vat_exclusive <- ifelse(hd$no_vat_price >0, TRUE, FALSE)
# hd$vat_exclusive[is.na(hd$vat_exclusive)] <- FALSE

# VAT only on new homes, not needed 
hd <- select(hd, -c('no_vat_price', 'vat_exclusive'))

dim(hd)
colSums(is.na(hd))
table(hd$property_category)


write_csv(hd, "Raw Data/cleaning_data_pre_location.csv")



##############################################################################################################################
## Cleaning Coordinates for Houses and Apartments 
##############################################################################################################################

hd_mapping_data <- hd
hd_mapping_data <- hd_mapping_data[!is.na(hd_mapping_data$location), ]
dim(hd_mapping_data)
colSums(is.na(hd_mapping_data))

# plot all data 
house_apartment_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = hd,
                   lng = ~longitude, 
                   lat = ~latitude,
                   radius = 2,
                   color = "red")  # Set a constant color, e.g., "red" for all points

house_apartment_map %>%
  addLegend(position = "topright", colors = "red", title = "Legend")
house_apartment_map

# Fixing outliers on the map 
plot(hd$longitude, hd$latitude)

limerick_centre <- data.frame(
  latitude = 52.500078,  # Example latitude
  longitude = -8.893433  # Example longitude
)

house_apartment_map <- house_apartment_map %>%
  addMarkers(data = limerick_centre, lng = ~longitude, lat = ~latitude, 
             label = "limerick_centre",  # Label for the point
  )
house_apartment_map

hd$distance_km <- (1/1000)*distVincentySphere(
  p1 = cbind(hd$longitude, hd$latitude),
  p2 = c(-8.893433, 52.500078)
)
summary(hd$distance_km)

hd$outside_lmk <- hd$distance_km > 50

outside_addresses <- hd$raw_address[hd$outside_lmk]
result_df <- data.frame()

for (address in outside_addresses) {
  geo_result <- ggmap::geocode(address)
  result_entry <- data.frame(Address = address, Latitude = geo_result$lat, Longitude = geo_result$lon)
  result_df <- rbind(result_df, result_entry)
} 

result_df$Latitude[(result_df$Address == "6 Hazel Grove, Bloomfield")]  <- 52.66047147390773
result_df$Longitude[(result_df$Address == "6 Hazel Grove, Bloomfield")]  <- -8.53982573958246
result_df$Latitude[(result_df$Address == "Apt 7, The Park, Nth Circ Rd")]  <- 52.66223081443916
result_df$Longitude[(result_df$Address == "Apt 7, The Park, Nth Circ Rd")]  <- -8.65493048469873
result_df$Latitude[(result_df$Address == "7 Templeville, Punches Cross")]  <- 52.65008125429552
result_df$Longitude[(result_df$Address == "7 Templeville, Punches Cross")]  <- -8.636682463626192
result_df$Latitude[(result_df$Address == "6 Templeville, Punches Cross")]  <- 52.64982442704089
result_df$Longitude[(result_df$Address == "6 Templeville, Punches Cross")]  <- -8.638409022968444
result_df$Latitude[(result_df$Address == "Apartment 9, Carriage Court, Dublin Road")]  <- 52.66634855079484
result_df$Longitude[(result_df$Address == "Apartment 9, Carriage Court, Dublin Road")]  <- -8.605291260406368
result_df$Latitude[(result_df$Address == "201 The Windmill, Dock Rd, Windmill")] <- 52.66193571513087
result_df$Longitude[(result_df$Address == "201 The Windmill, Dock Rd, Windmill")] <- -8.619867918931329
result_df$Latitude[(result_df$Address == "Ard Na Mara, Newtown, Clarina")] <- 52.63216714150626
result_df$Longitude[(result_df$Address == "Ard Na Mara, Newtown, Clarina")] <- -8.732389637319084
result_df$Latitude[(result_df$Address == "304 Richmond, Court, Dock Rd")] <- 52.66037057198545
result_df$Longitude[(result_df$Address == "304 Richmond, Court, Dock Rd")] <- -8.634283531570285
result_df$Latitude[(result_df$Address== "Apt 4A, Chandler House, Henry St")] <- 52.662522907653496
result_df$Longitude[(result_df$Address == "Apt 4A, Chandler House, Henry St")] <- -8.630713946915805
result_df$Latitude[(result_df$Address== "22 Templeville, Punches Cross, Ballincurra")] <- 52.65006501797327
result_df$Longitude[(result_df$Address == "22 Templeville, Punches Cross, Ballincurra")] <- -8.63987546350719
result_df$Latitude[(result_df$Address== "23 Templeville, Punches Cross, Ballinacurra")] <- 52.65000045769452
result_df$Longitude[(result_df$Address == "23 Templeville, Punches Cross, Ballinacurra")] <- -8.639668413387453
result_df$Latitude[(result_df$Address== "5 Templeville, Punches Cross, Ballinacurra")] <- 52.6498015999426
result_df$Longitude[(result_df$Address == "5 Templeville, Punches Cross, Ballinacurra")] <- -8.638306507544153
result_df$Latitude[(result_df$Address== "8 Templeville, Punches Cross, Ballinacurra")] <- 52.64987177953488
result_df$Longitude[(result_df$Address == "8 Templeville, Punches Cross, Ballinacurra")] <- -8.638633392836006
result_df$Latitude[(result_df$Address== "Church Quarter, Kilbehenny, Co. Limerick")] <- 52.29414413204687
result_df$Longitude[(result_df$Address == "Church Quarter, Kilbehenny, Co. Limerick")] <- -8.199919831142395
result_df$Latitude[(result_df$Address== "BOHER, KILBEHENNY, LIMERICK")] <- 52.29424847038789
result_df$Longitude[(result_df$Address == "BOHER, KILBEHENNY, LIMERICK")] <- -8.205320684380121


hd$latitude[hd$outside_lmk] <- result_df$Latitude
hd$longitude[hd$outside_lmk] <- result_df$Longitude
hd$location[hd$outside_lmk] <- paste(result_df$Latitude, result_df$Longitude, sep = ",")

house_apartment_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = hd,
                   lng = ~longitude, 
                   lat = ~latitude,
                   radius = 2,
                   color = "red")  # Set a constant color, e.g., "red" for all points

house_apartment_map %>%
  addLegend(position = "topright", colors = "red", title = "Legend")
house_apartment_map

dim(hd)
colSums(is.na(hd))

hd <- hd[!hd$outside_lmk, ]

hd<- select(hd, -c("distance_km", "outside_lmk"))

table(hd$months)
table(hd$months, hd$years)

hd$month_year[hd$years == 2017] <- hd$months[hd$years == 2017]
hd$month_year[hd$years == 2018] <- hd$months[hd$years == 2018]+12
table(hd$month_year, hd$years)

hd <- hd %>%
  mutate(quarter = case_when(
    month_year %in% c(1, 2, 3) ~ 1,
    month_year %in% c(4, 5, 6) ~ 2,
    month_year %in% c(7, 8, 9) ~ 3,
    month_year %in% c(10, 11, 12) ~ 4,
    month_year %in% c(13, 14, 15) ~ 5,
    month_year %in% c(16, 17, 18) ~ 6,
    month_year %in% c(19, 20, 21) ~ 7,
    month_year %in% c(22, 23) ~ 8,
    TRUE ~ NA_integer_  # Default case, replace NA with your default value
  ))



##############################################################################################################################
## Saving File 
##############################################################################################################################

houses_apartments_limerick <- hd # Limerick Data 2010-2018 
dim(houses_apartments_limerick)
colSums(is.na(houses_apartments_limerick))

write.csv(houses_apartments_limerick, "Cleaned Data/house_apartment_data_limerick.csv", row.names = FALSE)



