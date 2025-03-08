###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Geospatial Analysis
## Date: 19/10/2023
## Author: BOD
###########################################################################


hd <- readr::read_csv("./Cleaned Data/houses_apartments_limerick_complete.csv")
hd$price_log <- log(hd$price)
dim(hd)

colnames(hd)
plot(hd$price)
##########################################################
## Create Base Maps
##########################################################

# map of limerick county 
map_county <- ggplot() + theme_map() +
  geom_sf(data = limerick_county_border, fill = "cornsilk2", color = "black", alpha = 1) 
map_county

# map of limerick city 
limerick_city <- limerick_sf[limerick_sf$municipality == 'Limerick City and Suburbs',]
map_city <- ggplot() + theme_map() +
  geom_sf(data = limerick_city, fill = "cornsilk2", color = "black", alpha = 1) 
map_city

# map of limerick municipality 
map_municipalities <- ggplot() + theme_map() +
  geom_sf(data = limerick_municipalities_sf, fill = "cornsilk2", alpha = 1, color = "black") 
map_municipalities

# map of limerick municipaliy labels 
labels_df <- tibble(Labels = c("NC", "CK", "AR", "LCS"), Municipality = c("Newcastle West (NW)", "Cappamore — Kilmallock (CK)", "Adare — Rathkeale (AR)", "Limerick City and Suburbs (LCS)"))
labels_city_df <- tibble(Labels = c("LCS-1", "LCS-2", "LCS-3", "LCS-4","LCS-5", "LCS-6", "LCS-7", "LCS-8","LCS-9"), urban_area = c("North", "City North", "Corbally", "City South", "West", "Balinacurra", "Outer City South", "Ballysimon", "East"))
municipality_labels <- data.frame(
  label = c("NC", "CK", "AR", "LCS"),
  longitude = c(-9.25, -8.3, -9.05, -8.74),
  latitude = c(52.5, 52.65, 52.63, 52.685)
)
map_municipality_labels <- map_municipalities+
  geom_label(fill = 'lightskyblue1', color = 'black', alpha = 1, family = "sans", data = municipality_labels, aes(x = longitude, y = latitude, label = label), 
             size = 3.5, fontface = 'bold')  
map_municipality_labels

# map of limerick urban_areas 
limerick_sf$id_2 <- NA
# order by id 
for (i in 1:12) {
  limerick_sf$id_2[i] <- i
}
limerick_sf$id_2[limerick_sf$region == "Limerick County"] <- 1
updated_sf <- limerick_sf %>% 
  group_by(id_2) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
map_urban_area <- ggplot() + theme_map() +
  geom_sf(data = updated_sf, fill = "cornsilk2", color = "#6c6c6c", alpha =1) +
  geom_sf(data = limerick_municipalities_sf, fill = NA, color = "black") 
map_urban_area

# map of urban are labels 
municipality_labels <-   geom_label(fill = 'lightskyblue1', color = 'black',alpha = 1, family = 'sans', data = municipality_labels, aes(x = longitude, y = latitude, label = label), 
                                  size = 4) 
map_urban_area_labels <- map_urban_area+municipality_labels
map_urban_area_labels

# map of limerick city with labels
city_labels_df <- data.frame(
  label = c("LCS-1", "LCS-2", "LCS-3", "LCS-4","LCS-5", "LCS-6", "LCS-7", "LCS-8","LCS-9"),
  longitude = c(-8.69, -8.63, -8.58, -8.598, -8.73, -8.69, -8.61, -8.55, -8.5),
  latitude = c(52.67, 52.707, 52.685, 52.698, 52.665, 52.64, 52.638, 52.63, 52.65)
)
urban_area_labels <-   geom_label(fill = 'lightskyblue1', color = 'black', alpha = 1, family = "sans", data = city_labels_df, aes(x = longitude, y = latitude, label = label), 
                                 size = 3.2) 
map_city_labels <- map_city+urban_area_labels
map_city_labels

map_city_labels_arrows <- map_city_labels+
geom_segment(aes(x = -8.63, y = 52.707-0.0035, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
geom_segment(aes(x = -8.58, y = 52.685-0.0035, xend = -8.60, yend = 52.67),
             arrow = arrow(length = unit(0.1, "cm")), #3
             color = "black") +
geom_segment(aes(x = -8.60, y = 52.698-0.0035, xend = -8.625, yend = 52.665),
             arrow = arrow(length = unit(0.1, "cm")), #4
             color = "black") 
map_city_labels_arrows



##########################################################
## Municipalities Maps 
##########################################################
hd$municipality_abbrev <- factor(hd$municipality, levels = c("Newcastle West", "Cappamore — Kilmallock", "Limerick City and Suburbs", "Adare — Rathkeale"), labels = c("Newcastle West (NW)", "Cappamore — Kilmallock (CK)", "Limerick City and Suburbs (LCS)", "Adare — Rathkeale (AR)" ))
municipality_colours_vec <-  c('Newcastle West (NW)' = '#9467BD', 'Adare — Rathkeale (AR)' = '#CD534C', 'Limerick City and Suburbs (LCS)' = '#46B8DA', 'Cappamore — Kilmallock (CK)' = '#FF7F0E' )
municipality_colours <- scale_color_manual(values = municipality_colours_vec)


##### Municipalities and Observations
municipalitys_map <- map_municipalities +
geom_point(data = hd,  aes(x = longitude, y = latitude, color = municipality_abbrev), size = 1) + municipality_colours + 
  theme_map(font_size = 18) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.2, "cm"),  # Adjust the size as needed
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + labs(color = "Municipalities") 

# extract the legend from this map 
municipalities_legend <- get_legend(municipalitys_map) 
municipalities_legend

limerick_city_centre <- data.frame(municipality_abbrev = 'Limerick City Centre', longitude = -8.621291, latitude = 52.66569)
  
  
# mapping LCS city centre and the municipalities 
mapping_data <- rbind(dplyr::select(hd, c('municipality_abbrev', 'longitude', 'latitude')), limerick_city_centre)
municipalities_city_centre_map <- municipalitys_map + geom_point(data = mapping_data, 
                                                   aes(x = longitude, y = latitude, 
                                                       color = municipality_abbrev), size = 1) + 
  scale_color_manual(values = c(municipality_colours_vec, 'Limerick City Centre' = 'black')) + 
  theme_map(font_size = 22) + 
  theme(
    legend.position = "none",
    legend.key.size = unit(0.2, "cm"),  # Adjust the size as needed
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  ) + labs(color = "Location")

city_centre <- data.frame(label = 'City Centre', longitude = -8.8, latitude = 52.74)

# add line segment from city centre to title 
municipalities_city_centre_map <- municipalities_city_centre_map + geom_segment(aes(x = -8.7, y = 52.73, xend = -8.625, yend = 52.669),
                                              arrow = arrow(length = unit(0.1, "cm")), #2
                                              color = "black") + 
  geom_text(color = 'black', family = "Helvetica", data = city_centre, aes(x = longitude, y = latitude, label = label), 
            size = 6)
municipalities_city_centre_map


# create title for the grid
title <- ggdraw() + 
  draw_label(
    "Map of Limerick Municipalities with Limerick City Centre",
    fontface = 'bold',
    x = 0,
    hjust = -0.3,
    size = 22
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )  

# grid of title, legend and plot 
municipalties_map <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.0, 1, 0.1)),
  plot_grid(municipalities_city_centre_map, NULL, municipalities_legend, nrow = 1, ncol = 3, rel_widths = c(0.6,0.0001,0.3)),
  rel_heights = c(1,15),
  nrow = 2
)
municipalties_map
ggsave(filename = 'municipalities_centre_map.png', path = './Figures', width = 30, height = 15, units = 'cm')



##### Municipalities and Median Prices 
median_prices <- hd %>%
  group_by(municipality) %>%
  summarize(median_price = round(median(price), -3))

label_mapping <- c('Limerick City and Suburbs' = 'LCS', 'Adare — Rathkeale' = 'AR', 'Cappamore — Kilmallock' = 'CK','Newcastle West'  = 'NW')

limerick_municipalities_sf <- limerick_municipalities_sf %>%
  mutate(municipality_labels = recode_factor(municipality, !!!label_mapping))

map_data <- merge(limerick_municipalities_sf, median_prices, by =  "municipality")
centroids <- st_centroid(map_data)
centroids$longitude <- coordinates[, "X"]
centroids$latitude <- coordinates[, "Y"]
centroids$median_price_euro <- paste("€", centroids$median_price/1000, ",000", sep= '')

median_price_region <- ggplot() + 
  geom_sf(data = map_data, aes(fill = median_price), color = "white", size = 1) + theme_map() +
  scale_fill_viridis(begin = 0.7, end = 1, option = "viridis", breaks = c(140000,190000), labels = c('Low', 'High')) +
  geom_point(data = hd, aes(x = longitude, y = latitude), colour = 'lightcoral', size = 0.7) + 
  geom_text(data = centroids, aes(x = longitude, y = latitude, label = paste( municipality_labels, '    \n', median_price_euro)), 
            size = 7, hjust = 0.9, vjust = 0.8, color = 'black', alpha = 0.9) +  
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))+ 
  ggtitle('Median Residential Property Prices \nof Limerick Municipalities') +   
  theme(plot.title = element_text(hjust = 0.1, vjust = -20, color = 'black', size = 18)) + 
  labs(fill = "Median Price (€)", color = "black")
median_price_region
ggsave(filename = 'map_median_municipalities.png', path = './Figures', width = 30, height = 20, units = 'cm')





# map of observation id 
map_municipality_price <-  map_urban_area_labels +
  geom_sf(data = hd_sf, aes(color = 'red'), size = 1.3) +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = hd, aes(x = longitude, y = latitude, label = paste(id)), size = 2, nudge_y = 0.01)  # Add text labels for id
map_municipality_price





## Urban Area Map and placenames 
city_labels <- data.frame(
  label = c("LCS-1", "LCS-2", "LCS-3", "LCS-4","LCS-5", "LCS-6", "LCS-7", "LCS-8","LCS-9"),
  longitude = c(-8.69, -8.63, -8.58, -8.60, -8.72, -8.69, -8.61, -8.55, -8.5),
  latitude = c(52.67, 52.7, 52.685, 52.695, 52.665, 52.64, 52.638, 52.63, 52.65)
)

suburb_locations <- data.frame(
  label = c("Anacotty", "Castleconnell", "Weston", "Castletroy", "Caherdavin", "Dooradoyle", "N.C Road", "Ennis Road", 'Ballyclough'),
  longitude = c(-8.532286271234595, -8.487099473506486,-8.631617117777088, - 8.56874561609878, -8.67200377305012, -8.6653297686484896, -8.64947102942218, -8.655894471593134, -8.624278764015553),
  latitude = c(52.66511811066273, 52.71301386098046,52.651443124799286, 52.66767209474872, 52.675865516262434, 52.6341856407736, 52.663172642493876, 52.66942818060706, 52.62200605340709)
)


map_city <- ggplot() + theme_map() + 
  geom_sf(data = limerick_city_sf, fill = "cornsilk2", alpha = 0.5,  color = "darkgrey") 
suburb_map_back <-  map_city + ggtitle('Map of LCS Urban Areas with \nNoteable Addresses') + theme(plot.title = element_text(size=18, vjust = -30)) +   
  urban_area_labels+
  geom_segment(aes(x = -8.63, y = 52.707-0.0035, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "darkgrey") +
  geom_segment(aes(x = -8.58, y = 52.685-0.0035, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "darkgrey") +
  geom_segment(aes(x = -8.60, y = 52.698-0.0035, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "darkgrey")
suburb_map_back

ua_city_centre <- data.frame(label = 'City Centre', longitude = -8.7, latitude = 52.695)

# add line segment from city centre to title 
suburb_map_back_cc <- suburb_map_back + geom_segment(aes(x = -8.675, y = 52.695, xend = -8.625, yend = 52.667),
                                                                                arrow = arrow(length = unit(0.1, "cm")), #2
                                                                                color = "black") + 
  geom_text(color = 'black', family = "Helvetica", data = ua_city_centre, aes(x = longitude, y = latitude, label = label), 
            size = 5) + 
  geom_point(data = mapping_data[mapping_data$municipality=='Limerick City Centre',], 
             aes(x = longitude, y = latitude, 
                 fill = 'black'), size = 3) + theme(legend.position = 'none')


suburbs_map <- suburb_map_back_cc +
  geom_text(color = 'red4', family = "sans", data = suburb_locations, aes(x = longitude, y = latitude, label = label), 
             size = 4)
suburbs_map

ggsave(filename = 'map_addresses_lcs.png', path = './Figures', width = 30, height = 20, units = 'cm')

head(hd)


ennis_road <- read.csv("./Map Data/ennis_road.csv")
ennis_road_properties <- hd[hd$address_area=='Ennis Road',]
ennis_road_properties <- ennis_road_properties[ennis_road_properties$id !=318532, ]
ennis_ids <- c(268070, 309849, 249568, 286547, 274394, 318530, 257426, 317230, 280148, 275136, 273301, 318957, 332024, 329861, 244060)
# proportion of correct addresses
length(ennis_ids)/length(ennis_road_properties)
ennis_road_properties$on_road <- ifelse(ennis_road_properties$id %in% ennis_ids, T, F)
ennis_road_properties$colour <- ifelse(ennis_road_properties$on_road, 'green', 'red')
table(ennis_road_properties$on_road)

# Define your legend
legend_values <- c("Correct", "Incorrect")  # Define legend values
legend_colors <- c("green", "red")  # Define corresponding colors
legend_labels <- paste(legend_values)  # Create legend labels

# Create the Leaflet map
ennis_rd_map <- leaflet() %>%
  addTiles() %>%  
  addPolylines(lng = ennis_road$longitude, 
               lat = ennis_road$latitude,
               data = ennis_road, weight = 8, color = 'orange', opacity = 1) %>%
  addCircles(data = ennis_road_properties, 
             lng=ennis_road_properties$longitude, 
             lat=ennis_road_properties$latitude, radius = 20, color = ~colour, opacity = 3, 
             popup  = paste0("<b>","Crime Outcome: ", "</b>",
                             ennis_road_properties$id)) %>%
  # Add legend
  # Add legend
addLegend("topright", colors = legend_colors, labels = legend_labels, title = "Location Sepecification") %>% 
  addLegend("topright", colors = 'orange', labels = 'Ennis Road in LCS', title = element_blank())

# Display the map
ennis_rd_map
# export  2000x1169

##########################################################
## Plotting Points - Price
##########################################################

limerick_towns <- data.frame(label = c('Adare', 'Newcastle\nWest'), longitude = c(-8.79+0.03, -9.025), latitude =c(52.55-0.05,52.42-0.05))

# map of property price county 
map_municipality_price <-  map_urban_area_labels +
  geom_sf(data = hd_sf, aes(color = price_log), size = 1) +
  scale_color_gradient(name = "Price (€)", low = "red", high = "green", labels = c("22,000", "60,000", "160,000", "440,000", "1,200,000")) +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.5)) + theme(plot.margin = margin(0,0,0,0, 'cm')) + 
  geom_segment(aes(x = -9.058+0.03, y = 52.44-0.05, xend = -9.058, yend = 52.44),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") + 
  geom_segment(aes(x = -8.79+0.03, y = 52.56-0.05, xend = -8.79, yend = 52.56),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") + 
  geom_text(color = 'black',  family = "Helvetica",  data = limerick_towns, aes(x = longitude, y = latitude, label = label), 
            size = 4) + geom_segment(aes(x = -8.75, y = 52.73, xend = -8.625, yend = 52.669),
                                       arrow = arrow(length = unit(0.1, "cm")), #2
                                       color = "black") + 
  geom_text(color = 'black', family = "Helvetica", data = city_centre, aes(x = longitude, y = latitude, label = label), 
            size = 4)
  map_municipality_price
# label values are 10, 11, 12, 13, 14.
# hard to see differences, better to use polygon map


# map of price in city 
map_city_price <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = hd_sf[hd_sf$municipality=='Limerick City and Suburbs',], aes(color = price_log), size =1) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))  + 
  scale_color_gradient(name = "Price (thousands €)", low = "red", high = "green", labels = c("20", "60", "160", "440", "1,200")) + 
  geom_segment(aes(x = -8.63, y = 52.707-0.003, xend = -8.63, yend = 52.676),  # LCS2 label
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.003, xend = -8.60, yend = 52.67), # LCS3 label
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.595, y = 52.698-0.003, xend = -8.625, yend = 52.665), # LCS4 label 
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") + theme(plot.margin = margin(0,0,0,0, 'cm'))
map_city_price
# notably lower prices in the city center, differences in north east and north west (ennis road), clear cluster in Castletroy (higher), prospect (low), and Dooradoyle (mid)

# title for grid property prices map \
title <- ggdraw() + 
  draw_label(
    "Property Price in Limerick",
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) + theme(plot.margin = margin(0,0,0,0))
# legend for grid 
legend_price <- get_legend(map_city_price) 
# grid of proeprty prices 
price_map_city_county <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.4, 1, 0.1)),
  plot_grid(map_municipality_price + theme(legend.position = 'none'), map_city_price + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2, vjust = 4),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(0.05,1, 0.1),
  nrow = 3 
)
price_map_city_county
ggsave(filename = 'map_price_county_city.png', path = './Figures', dpi = 600, width = 30, height = 15, units = 'cm')




##########################################################
## Plotting Points - Attributes 
##########################################################

# Plot of baths - All 
map_beds <- map_urban_area +
  ggtitle("Bedrooms in Limerick") +
  geom_sf(data = hd_sf, aes(color = beds), size = 1) +
  scale_color_gradient(name = "Bedrooms)", low = "red", high = "green") +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))
# clear increase in the number of bathrooms in dooradoyle and castletoyr, and outer city. 

# Plot of bedrooms - CIty 
map_beds_city <- map_city + 
  ggtitle("Bedrooms in Limerick City") +
  geom_sf(data = hd_sf[hd_sf$municipality=='Limerick City and Suburbs',], aes(color = beds), size = 1) +
  scale_color_gradient(name = "Bedrooms", low = "red", high = "green") +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))
# cluster of smaller properties in the city centre, similiar sizes in Dooradoye, Castletroy (some), and North

# Plot of baths - All 
map_baths <- map_urban_area +
  ggtitle("Bathrooms in Limerick") +
  geom_sf(data = hd_sf, aes(color = baths), size = 1) +
  scale_color_gradient(name = "Bathrooms)", low = "red", high = "green") +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))
# clear increase in the number of bathrooms in dooradoyle and castletoyr, and outer city. 

# Plot of baths - City 
map_baths_city <- map_city +
  ggtitle("Bathrooms in Limerick City") +
  geom_sf(data = hd_sf[hd_sf$municipality=='Limerick City and Suburbs',], aes(color = baths), size = 1) +
  scale_color_gradient(name = "Bathrooms", low = "red", high = "green") +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))
# clear increase in the number of bathrooms in dooradoyle and castletoyr, and outer city. 

# Plot of cul de sac - All
map_cds <- map_urban_area +
  ggtitle("Cul De Sac in Limerick") +
  geom_sf(data = hd_sf, aes(color = cul_de_sac.f), size = 0.8) +
  scale_color_manual(name = "Cul De Sac", values = c("TRUE" = "green", "FALSE" = "grey")) +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))
# very few cul de sac's in the county. - investigate the city 

# Plot of cul de sac - City 
map_cds_city <- map_city +
  ggtitle("Cul De Sac in Limerick City") +
  geom_sf(data = hd_sf[hd_sf$municipality=='Limerick City and Suburbs',], aes(color = cul_de_sac.f), size =1) +
  scale_color_manual(name = "Cul De Sac", values = c("TRUE" = "green", "FALSE" = "grey")) +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))
# more cul de sacs in the surrounding city, less in the outer areas (east, west, north) possibly due to housing estates in suburbs 

# Plot of fireplace - All
map_fireplace <- map_urban_area +
  ggtitle("Fireplaces in Limerick") +
  geom_sf(data = hd_sf, aes(color = fireplace), size = 0.8) +
  scale_color_manual(name = "Fireplace", values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))

# Plot of fireplace - City 
map_fireplace_city <- map_city +
  ggtitle("Fireplaces in Limerick City") +
  geom_sf(data = hd_sf[hd_sf$municipality=='Limerick City and Suburbs',], aes(color = fireplace), size =1) +
  scale_color_manual(name = "Fireaplce", values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))

# Plot of garage - All
map_garage <- map_urban_area + 
  ggtitle("Garages in Limerick") +
  geom_sf(data = hd_sf, aes(color = garage), size = 0.8) +
  scale_color_manual(name = "Garage", values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))

# Plot of garage - City 
map_garage_city <- map_city  +
  ggtitle("Garagesin in Limerick City") +
  geom_sf(data = hd_sf[hd_sf$municipality=='Limerick City and Suburbs',], aes(color = garage), size =1) +
  scale_color_manual(name = "Garage", values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_map() + theme(plot.title = element_text(hjust = 0.5))




# Plot of property types - All
map_urban_area + 
  ggtitle("Apartmens in Limerick") +
  geom_sf(data = hd_sf[hd_sf$property_type == 'Apartment',], aes(color = property_type), size = 0.8) + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_map()


# Plot of property types - All
map_urban_area + 
  ggtitle("Apartmens in Limerick") +
  geom_sf(data = hd_sf, aes(color = property_type), size = 0.8) + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_map()


# select the popular property types and group aprt and duplex 
table(hd$property_type)
# det, semi-d, terrace, apt, other
prop_type_map_data <- hd_sf[hd_sf$property_type %in% c('Detached', 'Semi-Detached', 'Apartment', 'Duplex'), ]
value_mapping <- c('Duplex'= 'Apartment')
prop_type_map_data <- prop_type_map_data %>%
  mutate(property_type = recode_factor(property_type, !!!value_mapping))
table(prop_type_map_data$property_type)

# Plot of property types - All
property_type_map <- map_urban_area_labels + 
  ggtitle("Popular Property Types in Limerick") +
  geom_sf(data = prop_type_map_data, aes(color = property_type), size = 1.5) + labs(color = 'Property Type') + theme_map() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -4, size = 18))+   theme(legend.position = "right", 
                                                                                legend.justification = 'center', legend.text = element_text(size = 14),
                                                                                legend.title = element_text(size = 14))  
property_type_map + geom_segment(aes(x = -8.7, y = 52.73, xend = -8.625, yend = 52.669),
             arrow = arrow(length = unit(0.1, "cm")), #2
             color = "black") + 
  geom_text(color = 'black', family = "Helvetica", data = city_centre, aes(x = longitude, y = latitude, label = label), 
            size = 6)

ggsave(filename = 'property_type_map.png', path = './Figures', dpi = 600, width = 30, height = 15, units = 'cm')











##########################################################
## Polygon Maps 
##########################################################


# Urban area polygon maps
# using limerick shapefile (list of regions) and summary stats per region. 
urban_area_summary
limerick_sf_polygon_map <- merge(limerick_sf ,urban_area_summary, by = 'urban_area')
limerick_sf_polygon_map <- limerick_sf_polygon_map[limerick_sf_polygon_map$municipality_label == 'LCS', ]

ggplot(data = limerick_sf_polygon_map) +
  geom_sf(aes(fill = cul_de_sac_prop)) + theme_map() +
  scale_fill_gradient(name = "Proportion", low = "#558B2F", high = "#FFEB3B", breaks = c(0,0.1,0.3)) + 
  urban_area_labels +
geom_segment(aes(x = -8.63, y = 52.707-0.0035, xend = -8.63, yend = 52.676),
             arrow = arrow(length = unit(0.1, "cm")), #2
             color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.0035, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.60, y = 52.698-0.0035, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") 



garage_map<- ggplot(data = limerick_sf_polygon_map) +
  geom_sf(aes(fill = garage_prop)) + theme_map() +
  scale_fill_gradient(name = "Proportion \nwith Garages", low = "#558B2F", high = "#FFEB3B", breaks = c(0.15,0.3, 0.45), labels = c("15%", "30%", '45%')) + 
  urban_area_labels +
  geom_segment(aes(x = -8.63, y = 52.707-0.0035, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.0035, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.60, y = 52.698-0.0035, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black")  + theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
                                        legend.justification = 'center', legend.text = element_text(size = 12),
                                        legend.title = element_text(size = 14))
garage_map
cul_de_sac_map <- ggplot(data = limerick_sf_polygon_map) +
  geom_sf(aes(fill = cul_de_sac_prop)) + theme_map() +
  scale_fill_gradient(name = "Proportion on\nCul-de-sac's", low = "#558B2F", high = "#FFEB3B", breaks = c(0.1, 0.2,0.3), labels = c('10%', '20%', '30%')) + 
  urban_area_labels +
  geom_segment(aes(x = -8.63, y = 52.707-0.0035, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.0035, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.60, y = 52.698-0.0035, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black")  + theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
                                        legend.justification = 'center', legend.text = element_text(size = 12),
                                        legend.title = element_text(size = 14))
cul_de_sac_map
title <- ggdraw() + 
  draw_label(
    "Proportions of Property Features in LCS",
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) +
  theme(
  ) + theme(plot.margin = margin(0,0,0,0))
proportions_map <-plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.1, 1, 0.1)),
  plot_grid(garage_map, cul_de_sac_map, nrow = 1, ncol = 2, labels = c('A', 'B'), 
            label_size = 14),   rel_heights = c(1,15), nrow = 2)
proportions_map
ggsave(filename = 'garage_cul_de_sac_map.png', path = './Figures', dpi = 600, width = 30, height = 15, units = 'cm')



# title for grid property prices map \
title <- ggdraw() + 
  draw_label(
    "Property Price in Limerick",
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) + theme(plot.margin = margin(0,0,0,0))
# legend for grid 
legend_price <- get_legend(map_city_price) 
# grid of proeprty prices 
price_map_city_county <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.4, 1, 0.1)),
  plot_grid(map_municipality_price + theme(legend.position = 'none'), map_city_price + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2, vjust = 2),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(0.05,1, 0.1),
  nrow = 3 
)
price_map_city_county
ggsave(filename = 'map_price_county_city.png', path = './Figures', dpi = 600, width = 30, height = 15, units = 'cm')



# Property Price Map for Poster 

p1 <- map_municipality_price + ggtitle('Property Price: Limerick County') + theme(plot.title = element_text(hjust = 0.1, vjust = -10, color = 'black', size = 16)) 
p2 <- map_city_price + ggtitle('Property Price: Limerick \nCity and Suburbs')+ theme(plot.title = element_text(hjust = 0.1, vjust = -25, color = 'black', size = 16)) 
poster_price_grid <- plot_grid(
  plot_grid(p1 + theme(legend.position = 'none'), p2 + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = NA, 
            ncol = 2),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(1, 0.1),
  nrow = 2 
)
poster_price_grid
ggsave(filename = 'poster_price_grid.png', path = '../../Poster/Figures', dpi = 600, width = 30, height = 15, units = 'cm')


