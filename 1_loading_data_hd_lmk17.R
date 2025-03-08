###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: loading data
## Date: 19/10/2023
## Author: BOD
###########################################################################

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UL Bsc Maths Science/AY2023:24 4th year /MS4037 - FYP/Final Thesis/Data and Analysis")


#############################
## Load Packages & Data
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




# plots together
theme_set(theme_cowplot())


hd1 <- readr::read_csv('./Raw Data/IrelandData2017.csv')
hd <- hd1
dim(hd)


#############################
## Data Size & Checks
#############################


glimpse(hd)
dim(hd)
# view(hd)

# clean column names
hd <- hd %>% clean_names()
colnames(hd)

# Missing Values
colSums(is.na(hd))
# 1 row of NA values can be removed 
hd <- hd[!is.na(hd$id), ]

# No Duplicate rows 
any(duplicated(hd))
hd_duplicates <- hd %>%
  filter(duplicated(id) | duplicated(id, fromLast = TRUE))
dim(hd_duplicates)

#############################
## Creating a sample dataset
#############################

hd_sample <- hd[sample(nrow(hd), floor(nrow(hd)*0.1)),]
dim(hd_sample)
view(hd_sample)

no_info <- hd[is.na(hd$description) & is.na(hd$beds) & is.na(hd$baths) & is.na(hd$property_type) & is.na(hd$property_type_original),]
dim(no_info)
colSums(is.na(no_info))

#############################
## Converting Data Types 
#############################

# Convert numerical columns to INT
hd$id <- as.integer(hd$id)
hd$price <- as.integer(hd$price)
hd$present_price <- as.integer(hd$present_price)
hd$advertised_price <- as.integer(hd$advertised_price)
hd$days_to_sell <- as.integer(hd$days_to_sell)
hd$no_vat_price <- as.integer(hd$no_vat_price)
hd$beds <- as.integer(hd$beds)
hd$baths <- as.integer(hd$baths)
hd$size <- as.integer(hd$size)
hd$months <- as.integer(hd$months)
hd$years <- as.integer(hd$years)

# Convert Datetime entries to objects 
hd$date_advertised <- as.POSIXct(hd$date_advertised, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
hd$sale_date <- as.POSIXct(hd$sale_date, format = "%d/%m/%Y %H:%M", tz = "UTC")





#############################
## Cleaning Column by Column 
#############################

colnames(hd)

# Remove unnecessary columns - company checks
hd <- select(hd, -c('x1', "geo_code_api_tried", "agency", "is_inferred_data", "has_perfect_match"))

# id
# No NA's or duplicates 
summary(hd$id) 

# county
# 27 counties
table(hd$county, useNA = "ifany")
length(unique(hd$county))

# area
table(hd$area, useNA = "ifany")

# region 
table(hd$region, useNA = "ifany")

# postal_code
table(hd$postal_code, useNA = "ifany")

# not_full_market_price - meaning? 
table(hd$not_full_market_price, useNA = 'ifany')
view(hd[hd$not_full_market_price,])

# inconsistent address is all false, can be removed
table(hd$inconsistent_address, useNA = "ifany")
hd <- select(hd, -c("inconsistent_address"))



# declared_county
dim(hd[!is.na(hd$declared_county), ])
with(na.omit(hd[c('declared_county', 'county')]),table(ifelse(declared_county == county, "Yes", "No"))  )

# Select the rows where declared_county and county do not match
non_matching_rows <- hd[!is.na(hd$declared_county) & hd$declared_county != hd$county, ]
view(non_matching_rows)
# revgeocode(c(non_matching_rows$longitude[1], non_matching_rows$latitude[1]))

# removing geocode_precision, fixed_address, and declared_county variables 
# internal proxies used by the company who supplied the data
hd <- select(hd, -c("geocode_precision", "fixed_address", "declared_county"))




# property_description
table(hd$property_description, useNA = "ifany")
# better described as propery_condition [New, Second Hand]
# Reassign value in property_description
names(hd)[names(hd) == 'property_description'] <- 'property_condition'
hd$property_condition[hd$property_condition == 'Teach/Árasán Cónaithe Atháimhe'] <- 'Second-Hand Dwelling house /Apartment'
hd$property_condition <- ifelse(hd$property_condition == "New Dwelling house /Apartment", 'New', "Second Hand")
table(hd$property_condition, useNA = 'ifany')


table(hd$property_type_original, useNA = 'ifany')
hd$property_type_original <- tolower(hd$property_type_original)
hd$property_type_original <- gsub("for sale", "", hd$property_type_original)
hd$property_type_original <- gsub("to let", "", hd$property_type_original)
hd$property_type_original <- gsub("property", "", hd$property_type_original)
hd$property_type_original <- gsub("farm land", "agricultural land", hd$property_type_original)
hd$property_type_original <- trimws(hd$property_type_original, "right")
hd$site_size_values <- NA_character_
contains_acre <- grepl("acre", hd$property_type_original, ignore.case = TRUE)
hd$site_size_values[contains_acre] <- sub("(.*) acre site*", "\\1", hd$property_type_original[contains_acre])
hd$property_type_original <- sub("^[0-9.]+ acre ", "", hd$property_type_original)
summary(as.integer(hd$site_size_values))




#### property_type
table(hd$property_type, useNA = "ifany")
# better described as property_category
# checking the property type original for the different property types
names(hd)[names(hd) == 'property_type'] <- 'property_category'
table(hd$property_type_original[(hd$property_category == 'House') ], useNA = 'ifany')
table(hd$property_type_original[(hd$property_category == 'Apartment') ], useNA = 'ifany')
table(hd$property_type_original[(hd$property_category == 'Land') ], useNA = 'ifany')

# NA values for property type, scrape values from the property type original to assign these to land, apartment, or house
table(hd$property_type_original[is.na(hd$property_category) ], useNA = 'ifany')

# assign any land related entries as land
indices_site <- which(is.na(hd$property_category) & (
    grepl("site", hd$property_type_original, ignore.case = TRUE)|
    grepl("farm", hd$property_type_original, ignore.case = TRUE)|
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




## property_type column for both Apartment and House
table(hd$property_type_original[hd$property_category == 'House'])
table(hd$property_type_original[hd$property_category == 'Apartment'])
table(hd$property_type_original[hd$property_category == 'Land'])

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
                                        grepl("home", property_type_original, ignore.case = TRUE)) ~ "House",
      property_category == 'Apartment' & grepl("studio", property_type_original, ignore.case = TRUE) ~ "Studio Apartment",
      property_category == 'Apartment' & grepl("Duplex", property_type_original, ignore.case = TRUE) ~ "Duplex",
      property_category == 'Apartment' & (grepl("apartment", property_type_original, ignore.case = TRUE) |
                                            grepl("penthouse", property_type_original, ignore.case = TRUE)) ~ "Apartment",
      property_category == 'Land' & (grepl("commercial", property_type_original, ignore.case = TRUE) |
                                       grepl("industrial", property_type_original, ignore.case = TRUE)|
                                       grepl("development", property_type_original, ignore.case = TRUE))~ "Commercial",
      property_category == 'Land' & (grepl("farm", property_type_original, ignore.case = TRUE) |
                                        grepl("agricultural", property_type_original, ignore.case = TRUE)) ~ "Agricultural",
      property_category == 'Land' & grepl("site", property_type_original, ignore.case = TRUE) ~ "Site",
      TRUE ~ NA_character_
    )
  )

table(hd$property_type[hd$property_category == 'House'])
table(hd$property_type[hd$property_category == 'Apartment'])
table(hd$property_type[hd$property_category == 'Land'])


# Update property condition for new home property type 
hd$property_condition[hd$property_type_original == "New Home"] <- 'New'




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
hd$is_apartment[hd$is_apartment == TRUE & (hd$property_category != 'Apartment')] <- FALSE


# Now the values of is_apartment correclty consist of apartments
table(hd$property_type_original[hd$is_apartment], useNA = 'ifany')
table(hd$property_type_original[hd$property_category=='Apartment'], useNA = 'ifany')
table(hd$property_category[hd$is_apartment], useNA = 'ifany')

table(hd$property_type_original[!hd$is_apartment], useNA = 'ifany')
table(hd$property_type_original[hd$property_category!='Apartment'], useNA = 'ifany')






# property_size_description
table(hd$property_size_description, useNA = 'ifany')

# ber 
table(hd$ber, useNA = "ifany")



# present_price investigation
# we see present_price is either = 0 or price, it can be removed. 
hd$present_price <- ifelse(hd$present_price == 0, 0, hd$price - hd$present_price)
summary(hd$present_price)
hd <- select(hd, -c('present_price'))



# size and advertised_price investigation
# we see the size and advertised_price columns are the same in the data. 
table(is.na(hd$size) & is.na(hd$advertised_price))
table(ifelse(hd$size == hd$advertised_price, TRUE, FALSE))
values <- hd$size - hd$advertised_price
table(values, useNA = 'ifany')
with(na.omit(hd[c('size', 'advertised_price')]),table(ifelse(size == advertised_price, "Yes", "No"))  )

# too big for a houze size, it must be the advertised price
summary(hd$size)
hd <- select(hd, -c('size'))




# vat price and vat_exclusive
# 13.5% VAT rate 
vat_rate <- hd$price/hd$no_vat_price
summary(vat_rate, digits= 6)

# VAT charged on new properties only
table(hd$property_condition[hd$vat_exclusive])
table(hd$property_condition)
# 154 New dwellings have no VAT listed, calculate no_vat_price for these
summary(hd$no_vat_price[hd$property_condition ==  "New Dwelling house /Apartment" & hd$vat_exclusive == FALSE])
replacement_vat <- hd$price[hd$property_condition ==  "New Dwelling house /Apartment" & hd$vat_exclusive == FALSE]*(0.865)
hd$no_vat_price[hd$property_condition ==  "New Dwelling house /Apartment" & hd$vat_exclusive == FALSE] <- replacement_vat
hd$vat_exclusive <- ifelse(hd$no_vat_price >0, TRUE, FALSE)
hd$vat_exclusive[is.na(hd$vat_exclusive)] <- FALSE







# the NA values in the property_type are investment, offices, pubs, hotels etc. Removing these
hd_NA_type <- (hd[is.na(hd$property_category), ])
dim(hd_NA_type)
table(hd_NA_type$property_type_original)

hd_NA_type_desc <- hd_NA_type[!is.na(hd_NA_type$description), ]
dim(hd_NA_type_desc)

# Only 457 have a description
view(hd_NA_type_desc[is.na(hd_NA_type_desc$property_type_original), ])

hd <- hd[!(is.na(hd$property_type)), ]
dim(hd)
table(hd$property_type, useNA = 'ifany')



colSums(is.na(hd))



hd_lmk17 <- hd[hd$county == "Limerick",] # Limerick Data 2017 
dim(hd_lmk17)
view(hd_lmk17)


write.csv(hd, "Cleaned Data/house_data_2017.csv", row.names = FALSE)
write.csv(hd_lmk17, "Cleaned Data/house_data_limerick_2017.csv", row.names = FALSE)





