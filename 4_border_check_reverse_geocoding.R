###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Shape file, removing outliers from map 
## Date: 19/10/2023
## Author: BOD
###########################################################################

library(sf)
library(terra)
library(tmap)
library(dplyr)
library(spData)
library(viridis)
library(geosphere)
library(sp)
library(rgdal)
library(rgeos)
library(rnaturalearth)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)


hd <- readr::read_csv("./Cleaned Data/houses_apartments_limerick_mined.csv")


##########################################################
## Loading Shapefiles 
##########################################################

# Limerick county border from the Ireland map
ire_counties_sf <- st_read("./Map Data/counties/counties.shp", quiet = TRUE)
limerick_county_border_sf <- ire_counties_sf[ire_counties_sf$NAME_TAG == 'Limerick', ]

# Limerick municipalities
ire_municipalities_sf <- st_read("./Map Data/municipalities/municipalities.shp")
limerick_municipalities_sf <- ire_municipalities_sf[ire_municipalities_sf$COUNTY == 'LIMERICK', ]
limerick_municipalities_sf <- st_transform(limerick_municipalities_sf, st_crs(limerick_county_border_sf))
value_mapping <- c('Metropolitan District of Limerick City' = 'Limerick City and Suburbs', 'Municipal District of Adare-Rathkeale' = 'Adare — Rathkeale', 'Municipal District of Cappamore-Kilmallock' = 'Cappamore — Kilmallock', 'Municipal District of Newcastle West' = 'Newcastle West' )
limerick_municipalities_sf <- limerick_municipalities_sf %>%
  mutate(ENGLISH = recode_factor(ENGLISH, !!!value_mapping)) %>%
  rename("municipality" = "ENGLISH")

# Limerick city and suburbs electoral division
city_eds_sf <- st_read("./Map Data/eds/eds.shp")
table(city_eds_sf$NAME_TAG, useNA = 'ifany')

# Limerick townslands
ire_townslands_sf <- st_read("./Map Data/townlands/townlands.shp", quiet = TRUE)
table(ire_townslands_sf$NAME_EN[ire_townslands_sf$CO_NAME == 'Limerick'])
limerick_townslands_sf <- townslands_sf[townslands_sf$CO_NAME == 'Limerick', ]

##########################################################
## Creating Urban Areas with Shapefile  
##########################################################

limerick_county_sf <- limerick_municipalities_sf[limerick_municipalities_sf$ENGLISH != 'Metropolitan District of Limerick City', ]

plot(limerick_county_sf)

part1 <- limerick_county_sf
part2 <- city_eds_sf
colnames(part2)
part1 <- dplyr::select(part1, -c('GAEILGE', "CONTAE", "COUNTY", "PROVINCE", "GUID"))
part1 <- part1 %>% rename("NAME_TAG" = "ENGLISH")
part2 <- dplyr::select(part2, -c('NAME_GA', "NAME_EN", "ALT_NAME", "ALT_NAME_G", "CO_NAME", "CO_OSM_ID"))

part1 <- dplyr::select(part1, c('NAME_TAG', "geometry"))
part2 <- dplyr::select(part2, c('NAME_TAG', "geometry"))
colnames(part1)
colnames(part2)

limerick_sf <- rbind(part1, part2)

limerick_sf <- limerick_sf %>%
  mutate(
    municipality = ifelse(grepl("district", NAME_TAG, ignore.case = TRUE), NAME_TAG, "Limerick City and Suburbs"),
    region = ifelse(grepl("Limerick", municipality, ignore.case = TRUE), "Limerick City and Suburbs", "Limerick County"),
    NAME_TAG = gsub("municipal district of ", "", NAME_TAG, ignore.case = TRUE),
    NAME_TAG = gsub("-", " — ", NAME_TAG),
    municipality = gsub("municipal district of ", "", municipality, ignore.case = TRUE),
    municipality = gsub("-", " - ", municipality)
  ) %>%
  rename("urban_area" = "NAME_TAG")

st_write(limerick_sf, "./Map Data/urban_area/urban_area.shp")

# Limerick urban area shape file (municipality and city areas)
limerick_sf <- st_read("./Map Data/urban_area/urban_area.shp")






##########################################################
## Geospatial Analysis - Removing Outliers 
##########################################################
house_mapping_data <- hd

# Looking at limerick county border - there are points outside the border 
ggplot(limerick_county_border_sf) + geom_sf() +
  scale_fill_viridis() + theme_bw()
g <- ggplot(limerick_county_border_sf) + geom_sf() +
  scale_fill_viridis() + theme_bw()
g <- g + geom_point(data = house_mapping_data, 
                    aes(x = longitude, y = latitude, size = price, color = price))
g

# Fixing the outside-border points
house_mapping_data_transform <- select(hd, c(id, longitude, latitude, raw_address))
house_mapping_data_transform <- house_mapping_data_transform %>%
  rename(Longitude = longitude, Latitude = latitude)
house_mapping_data_sf <- st_as_sf(house_mapping_data_transform, coords = c("Longitude", "Latitude"))
st_crs(house_mapping_data_sf) <- st_crs("EPSG:4326")
limerick_county_border_sf <- st_transform(limerick_county_border_sf, st_crs(house_mapping_data_sf))
points_sf <- house_mapping_data_sf

# set the same crs 
st_crs(house_mapping_data_sf) <- st_crs(limerick_county_border_sf)

#
ggplot() +
  geom_sf(data = limerick_county_border_sf) +
  geom_sf(data = house_mapping_data_sf) +
  theme_minimal()
house_mapping_data_sf_joined <- 
  st_join(house_mapping_data_sf, limerick_county_border_sf) %>% 
  filter(!is.na(NAME_GA)) 
ggplot() +
  geom_sf(data = limerick_county_border_sf) +
  geom_sf(data = points_sf_joined) +
  theme_minimal()
dim(house_mapping_data_sf_joined)

dim(house_mapping_data_sf)
correct_ids <- (house_mapping_data_sf_joined$id)
house_mapping_data <- house_mapping_data[house_mapping_data$id %in% correct_ids, ]
dim(house_mapping_data)
hd <- house_mapping_data
dim(hd)


write.csv(hd, "Cleaned Data/houses_apartments_limerick_mined_border_check.csv", row.names = FALSE)





##########################################################
## Geocoding for address
##########################################################
rows <- hd[, c("id", "longitude", "latitude")]
result_df <- data.frame()  # Initialize an empty data frame to store the result

reverse_geocoded <- rows %>%
reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                address = address_found, full_results = TRUE)

reverse <- reverse_geocoded

property_and_areas <- select(reverse, c("id", "longitude", "latitude", "address_found", "city_district", "city", "suburb", "town", "village"))

write.csv(property_and_areas, "Cleaned Data/houses_apartments_limerick_geocoding_results.csv", row.names = FALSE)

# city = municipalities 
# city_disctrict = areas outside Limerick
# suburb = within cirty, areas 

dim(property_and_areas)
colSums(is.na(property_and_areas))
colnames(property_and_areas)

# Regions of the county
table(property_and_areas$city, useNA = 'ifany')
view(property_and_areas[is.na(property_and_areas$city), ])
property_and_areas$city[(property_and_areas$village=='Ballyneety')] <- "The Municipal District of Cappamore — Kilmallock"
property_and_areas$suburb[property_and_areas$id==283016] <- "Castleconnell"
property_and_areas$city[is.na(property_and_areas$city) & property_and_areas$suburb=='Castleconnell'] <- "Limerick"
property_and_areas$city[is.na(property_and_areas$city) & property_and_areas$town=='Abbeyfeale'] <- "The Municipal District of Newcastle West"
names(property_and_areas)[names(property_and_areas) == 'city'] <- 'county_region'
table(property_and_areas$county_region, useNA = 'ifany')

# Suburbs 
table(property_and_areas$suburb, useNA = 'ifany')
names(property_and_areas)[names(property_and_areas) == 'suburb'] <- 'city_suburbs'
table(property_and_areas$city_suburbs, useNA = 'ifany')

property_and_areas$city_suburbs <- gsub(" ED", "", property_and_areas$city_suburbs)
property_and_areas$city_suburbs <- gsub(" A", "", property_and_areas$city_suburbs)
property_and_areas$city_suburbs <- gsub(" B", "", property_and_areas$city_suburbs)
property_and_areas$city_suburbs <- gsub(" C", "", property_and_areas$city_suburbs)
property_and_areas$city_suburbs <- gsub(" D", "", property_and_areas$city_suburbs)


house_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = hd[is.na(hd$area), ],
                   lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 2)

house_map %>%
  addLegend(data = house_mapping_data, position = "topright", pal = pal, values= ~beds, title = "Number of bedrooms")



id_and_area <- select(hd, c("id", "area"))
na_ids <- id_and_area[is.na(id_and_area$area), ]
id_and_suburb <- select(property_and_areas, c("id", "city_suburbs"))
id_and_suburb_join <- merge(na_ids, id_and_suburb, by='id', all.x=TRUE)
colSums(is.na(id_and_suburb))

id_and_area$area[id_and_area$id %in% id_and_suburb_join$id] <- id_and_suburb_join$city_suburbs
table(id_and_area$area, useNA = 'ifany')

# updating hd with the county regions 
hd <- merge(hd, property_and_areas[, c("id", "county_region")], by = "id", all.x = TRUE)
colSums(is.na(hd))
table(hd$county_region, useNA = 'ifany')

value_mapping <- c('Limerick' = 'Limerick City and Suburbs', 'The Municipal District of Adare — Rathkeale' = 'Adare — Rathkeale', 'The Municipal District of Cappamore — Kilmallock' = 'Cappamore — Kilmallock', 'The Municipal District of Newcastle West' = 'Newcastle West' )
hd <- hd %>%
  mutate(county_region = recode_factor(county_region, !!!value_mapping))
names(hd)[names(hd) == 'county_region'] <- 'municipality'


hd <- merge(hd, id_and_area[, c("id", "area")], by = "id", all.x = TRUE)
colnames(hd)

names(hd)[names(hd) == 'area.x'] <- 'address_area'
names(hd)[names(hd) == 'area.y'] <- 'area'
names(hd)[names(hd) == 'region'] <- 'address_region'

hd$region <- ifelse(hd$municipality == 'Limerick City and Suburbs', 'Limerick City and Suburbs', 'Limerick County')




write.csv(hd, "Cleaned Data/houses_apartments_limerick_complete.csv", row.names = FALSE)


##########################################################
## Assigning Municipalities
##########################################################

limerick_county_map <- ggplot(limerick_municipalities_sf) + geom_sf() +
  scale_fill_viridis() + theme_bw()

municipalitys_map <- limerick_county_map + geom_point(data = hd, 
                                                      aes(x = longitude, y = latitude, color = municipality))
municipalitys_map

houses_sf <- st_as_sf(hd, coords = c("longitude", "latitude"), crs = st_crs(limerick_municipalities_sf))

# Perform a spatial join to assign houses to regions
houses_in_regions <- st_join(houses_sf, limerick_municipalities_sf)

# Check the resulting dataset
head(houses_in_regions)
colnames(houses_in_regions)

correct_municipalities <- select(houses_in_regions, c('id', 'ENGLISH'))
hd$update_municipality <- correct_municipalities$ENGLISH

table(hd$update_municipality)
hd$municipality <- hd$update_municipality

value_mapping <- c('Metropolitan District of Limerick City' = 'Limerick City and Suburbs', 'Municipal District of Adare-Rathkeale' = 'Adare — Rathkeale', 'Municipal District of Cappamore-Kilmallock' = 'Cappamore — Kilmallock', 'Municipal District of Newcastle West' = 'Newcastle West' )
hd <- hd %>%
  mutate(municipality = recode_factor(municipality, !!!value_mapping))
table(hd$municipality)

hd <- select(hd, -c("update_municipality"))


##########################################################
## Assinging Urban Areas 
##########################################################

limerick_city_suburbs <- hd[hd$municipality == 'Limerick City and Suburbs', ]
dim(limerick_city_suburbs)
table(limerick_city_suburbs$area, useNA = 'ifany')

houses_sf <- st_as_sf(limerick_city_suburbs, coords = c("longitude", "latitude"), crs = st_crs(city_eds))

# Perform a spatial join to assign houses to regions
houses_in_areas <- st_join(houses_sf, city_eds)

areas <- select(houses_in_areas, c('id', 'NAME_TAG'))
areas <- areas[!duplicated(areas$id), ]
limerick_city_suburbs$urban_area <- areas$NAME_TAG
table(limerick_city_suburbs$urban_area)

hd$urban_area <- hd$municipality
hd$urban_area[hd$municipality=='Limerick City and Suburbs'] <- areas$NAME_TAG



##########################################################
## Distance to Milk Market  
##########################################################

cbd_latitude <- 52.663523428776934
cbd_longitude <- -8.62214939965467
hd$cbd_distance <- numeric(nrow(hd))

for (i in seq_len(nrow(hd))) {
  distance <- distm(c(hd$longitude[i], hd$latitude[i]), c(cbd_longitude, cbd_latitude), fun = distHaversine)
  hd$cbd_distance[i] <- distance
}
hd$cbd_distance <- hd$cbd_distance/1000
summary(hd$cbd_distance)

hd$cbd_range <- cut(hd$cbd_distance,
                    breaks = c(-Inf, 5, 10, 20, Inf),
                    labels = c("in5", "in5to10", "in10to20", "over20"),
                    right = FALSE)
hd$cbd_range.f <- factor(hd$cbd_range)



##########################################################
## Cartesian Coordinates 
##########################################################

sf_hd <- st_as_sf(hd, coords = c("longitude", "latitude"), crs = 4326)
# Transform to Cartesian coordinates (EPSG:3857 projection)
sf_hd_cartesian <- st_transform(sf_hd, "+proj=merc +a=6378137 +b=6378137")
# Extract x, y, z coordinates
cartesian_coords <- st_coordinates(sf_hd_cartesian)
hd$cartesian_x <- cartesian_coords[,1]
hd$cartesian_y <- cartesian_coords[,2]

##########################################################
## Duplicate Coordinates 
##########################################################

# Find duplicated rows based on longitude and latitude
duplicated_rows <- hd[duplicated(hd$longitude) & duplicated(hd$latitude), ]

# Display duplicated rows
print(duplicated_rows)

dim(duplicated_rows)

dup_indices <- duplicated(hd$Longitude) & duplicated(hd$Latitude) & duplicated(hd$)

# Divide the data into duplicates and originals
duplicates <- hd[dup_indices, ]

duplicated_ll <- data.frame(id = duplicates$id, longitude = duplicates$longitude, latitude = duplicates$latitude)
dim(duplicated_ll)

# repeat this code until all unique 
hd$count <- ave(seq_along(hd$longitude), hd$longitude, FUN = seq_along)
table(hd$count)
hd$longitude[hd$count !=1] <- hd$longitude[hd$count !=1] + 0.0001
hd$latitude[hd$count !=1] <- hd$latitude[hd$count !=1] + 0.0001


# Find duplicated rows based on longitude and latitude
duplicated_rows <- hd[duplicated(hd$longitude) & duplicated(hd$latitude), ]
dim(duplicated_rows)
# Display duplicated rows
hd <- select(hd, -c("count"))


#### Fixing false addresses for Ballyclough 
hd$latitude[hd$id == 302591] <- 52.62388903291949
hd$longitude[hd$id == 302591] <- -8.630275483062242
hd$location[hd$id == 302591] <- "52.62388903291949, -8.630275483062242"
hd$urban_area[hd$id == 302591] <- "West"
distance <- distm(c(-8.630275483062242, 52.62388903291949), c(cbd_longitude, cbd_latitude), fun = distHaversine)
hd$cbd_distance[hd$id == 302591] <- distance/100





##########################################################
## Combine datasets with shapefiles 
##########################################################

write.csv(hd, "Cleaned Data/houses_apartments_limerick_complete.csv", row.names = FALSE)



