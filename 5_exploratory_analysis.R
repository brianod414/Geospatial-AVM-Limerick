###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Exploratory Analysis
## Date: 19/10/2023
## Author: BOD
###########################################################################


hd <- readr::read_csv("./Cleaned Data/houses_apartments_limerick_complete.csv")

my_libraries()

##########################################################
## Factor Levels 
##########################################################

# Convert relevant categorical variables to factors, and months to months.f, years to integer 
hd$region <- as.factor(hd$region)
hd$ber <- as.factor(hd$ber)
hd$property_condition <- as.factor(hd$property_condition)
hd$property_category <- as.factor(hd$property_category)
hd$property_type_original <- as.factor(hd$property_type_original)
hd$property_type <- as.factor(hd$property_type)
hd$years.f <- factor(hd$years, labels = c('2017', '2018'))
hd$months.f <- factor(hd$months, labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
hd$month_year.f <- factor(hd$month_year)
hd$cbd_range.f <- factor(hd$cbd_range, levels = c('in5', 'in5to10', 'in10to20', 'over20'))
## Set aesthetics 
region_colours <- scale_color_manual(values = c('Limerick City and Suburbs' = '#46B8DA', 'Limerick County' = '#509851'))
region_colours_fill <- scale_fill_manual(values = c('Limerick City and Suburbs' = '#46B8DA', 'Limerick County' = '#509851'))
municipality_colours_vec <-  c('Newcastle West' = '#9467BD', 'Adare — Rathkeale' = '#CD534C', 'Limerick City and Suburbs' = '#46B8DA', 'Cappamore — Kilmallock' = '#FF7F0E' )
municipality_colours <- scale_color_manual(values = municipality_colours_vec)
year_colours <- scale_fill_manual(values = c("2017" = "#6A951F", "2018" = "#EEC925"))
#
# create data for exploration with no outliers 
dim(hd)
hd_no_outliers <- hd[hd$price < 600000,]
dim(hd_no_outliers)

# city data
limerick_city <- hd[hd$municipality=='Limerick City and Suburbs', ]

##########################################################
## Categorical Variables - Single 
##########################################################

### year
hd %>% 
  ggplot(aes(x = years)) + ggtitle("Years") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$years.f, useNA = "ifany")

### month_year
hd %>% 
  ggplot(aes(x = month_year.f)) + ggtitle("Monthly") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$month_year, useNA = "ifany")

### region
hd %>% 
  ggplot(aes(x = region)) + ggtitle("Region") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$region, useNA = "ifany")

### municipality
hd %>% 
  ggplot(aes(x = municipality)) + ggtitle("Municipality") + 
  geom_bar(color = 'black', fill = 'steelblue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(hd$municipality, useNA = "ifany")

### urban_area
hd %>% 
  ggplot(aes(x = urban_area)) + ggtitle("urban_area") + 
  geom_bar(color = 'black', fill = 'steelblue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(hd$urban_area, useNA = "ifany")

### property_category
hd %>% 
  ggplot(aes(x = property_category)) + ggtitle("property_category") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$property_category, useNA = "ifany")

###  property_size_description
table(hd$property_size_description, useNA = 'ifany')

### property_condition
hd %>% 
  ggplot(aes(x = property_condition)) + ggtitle("property_condition") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$property_condition, useNA = "ifany")

### property_type
hd %>% 
  ggplot(aes(x = property_type)) + ggtitle("property_type") + 
  geom_bar(color = 'black', fill = 'steelblue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(hd$property_type, useNA = "ifany")

### ber 
hd %>% 
  ggplot(aes(x = ber)) +
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$ber, useNA = "ifany")
hd %>%
  filter(!is.na(ber)) %>%
  ggplot(aes(x = ber)) +
  geom_bar(color = 'black', fill = 'steelblue') +
  labs(x = "BER", y = "Count", title = "Bar Plot of BER (excluding NA)")

### fireplace
hd %>% 
  ggplot(aes(x = fireplace)) + ggtitle("fireplace") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$fireplace, useNA = "ifany")

### cul_de_sac
hd %>% 
  ggplot(aes(x = cul_de_sac)) + ggtitle("cul_de_sac") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$cul_de_sac, useNA = "ifany")

### period
hd %>% 
  ggplot(aes(x = period)) + ggtitle("period") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$period, useNA = "ifany")

### garage
hd %>% 
  ggplot(aes(x = garage)) + ggtitle("garage") + 
  geom_bar(color = 'black', fill = 'steelblue')
table(hd$garage, useNA = "ifany")




##########################################################
## Categorical Variables - Multi
##########################################################

table(hd$property_category, hd$property_condition)
table(hd$region, hd$property_condition)
table(hd$years, hd$property_category)
table(hd$months, hd$years)


# Bar charts of property catergories and regions for each year. 
years_v_region <- ggplot(hd, aes(x = region, fill = years.f)) + 
  ggtitle("Properties by Regions across Years") +
  geom_bar(position = "dodge", width = 0.9) + 
  year_colours+ labs(x = "Region", y='Count')+
  theme_half_open(font_size = 20)  + theme(plot.title = element_text(hjust = 0.5))
years_v_category <- ggplot(hd, aes(x = property_category, fill = years.f)) +
  ggtitle("Properties by Categories across Years") + 
  geom_bar(position = "dodge", width = 0.9) + 
  year_colours+ xlab("Property Category")+ labs(fill = 'Year', y='Count')+
  theme_half_open(font_size = 20) + theme(plot.title = element_text(hjust = 0.5))
legend <- get_legend(
  years_v_category + theme(legend.box.margin = margin(0, 0, 0, 12)) 
)
years_bar <- plot_grid(years_v_region + theme(legend.position = 'none'), years_v_category + theme(legend.position = 'none'), labels = c('A', 'B'), label_size = 16)
year_region_bar <-plot_grid(years_bar, legend, rel_widths = c(0.9, 0.1))
year_region_bar
ggsave(filename = 'year_region_bar.png', path = './Figures', width = 40, height = 20, units = 'cm')



##########################################################
## Quantitaive Variables - Single
##########################################################

### price Apartment vs Houses 
summary(hd$price)
summary(hd$price[hd$property_category=='House'])
summary(hd$price[hd$property_category=='Apartment'])
summary(hd$price[hd$region=='Limerick City and Suburbs'])
summary(hd$price[hd$region=='Limerick County'])

### histogram of log transformed price 
pre_log_hist <- ggplot(hd, aes(x = price))  + geom_histogram(color = 'darkgrey', fill = '#1aa7ec', bins = 30) +
  ggtitle("Histogram of Price") +
  labs(x = "Price (thousands €)", y = "Count") + 
  scale_x_continuous(breaks = c(250000, 500000,750000,1000000,1250000), labels =c("250", "500", "750", "1,000", "1,250")) + 
  theme_cowplot(font_size = 12)+theme(plot.title = element_text(hjust = 0.5)) 
hd$price_log <- log(hd$price)
after_log_hist <- ggplot(hd, aes(x = price_log)) + geom_histogram(color = 'darkgrey', fill = '#1aa7ec', bins = 30)+ ggtitle("Histogram of log(Price)") + 
  labs(x = "log(Price)", y = "Count")  +  
  theme_cowplot(font_size = 12) + theme(plot.title = element_text(hjust = 0.5))
log_transform_comparison <- plot_grid(pre_log_hist, after_log_hist, labels = c('A', 'B'), label_size = 12)
log_transform_comparison
ggsave(filename = 'log_transform_comparison.png', path = './Figures', width = 24, height = 12, units = 'cm')


### advertised price 
summary(hd$advertised_price)
hd$price_change <- hd$price - hd$advertised_price
summary(hd$price_change)
hd <- hd %>%
  mutate(sale_outcome = case_when(
    price_change < 0 ~ 'below',
    price_change > 0 ~ 'above',
    TRUE ~ 'equal'
  ))
freq_table <- table(hd$sale_outcome, hd$property_category)
prop_table <- round(prop.table(freq_table),2)
summary(hd$advertised_price)
hd %>% 
  ggplot(aes(x = advertised_price)) +
  geom_histogram(color = 'black', fill = 'steelblue')#

### log transformation required
hd$advertised_price_log <- log(hd$advertised_price)
hd %>% 
  ggplot(aes(x = advertised_price_log)) +
  geom_histogram(color = 'black', fill = 'steelblue')#
hd %>% 
  ggplot(aes(x = price_change)) +
  geom_histogram(color = 'black', fill = 'steelblue')#
summary(hd$price_change)


### days_to_sell
summary(hd$days_to_sell)
hd %>% 
  ggplot(aes(x = days_to_sell)) +
  geom_histogram(color = 'black', fill = 'steelblue')#


### size
table(hd$property_size_description, hd$size)
summary(hd$size)
hd %>% 
  ggplot(aes(x = size)) +
  geom_histogram(color = 'black', fill = 'steelblue')#


### beds
summary(hd$beds)
table(hd$beds)
hd %>% 
  ggplot(aes(x = beds)) +
  geom_bar(color = 'black', fill = 'steelblue')#

### baths
summary(hd$baths) 
table(hd$baths)
hd %>% 
  ggplot(aes(x = baths)) +
  geom_bar(color = 'black', fill = 'steelblue')#

# Change 0 bath, updating the property condition to derelict. 
hd$property_condition[hd$baths==0] <- 'Derelict'


##########################################################
## Descriptive Analysis (EDA)
##########################################################
# 
# Quantitative and Price
# - beds
# - baths
# - size
# - days_to_sell
# - price change (cannot use since not ind)
# - cbd_distance
# - cartesian.x 
# - cartesian.y
# 
# Categorical and Price 
# - property_category
# - property_type
# - property_condition
# - municipality 
# - region
# - urban_area
# - sale_outcome
# - cul_de_sac
# - fireplace
# - garage 
# - years
# - fireplace
# - month_year
# 
##########################################################

complete_correlation_data <- select(hd, c("price_log", "beds", "baths", "cbd_distance", "years", "price_change", "cartesian_x", "cartesian_y"))
correlations <- cor(complete_correlation_data)
correlations
corrplot(correlations, method = "number")
# price has correlation of 0.46 wtih bed, 0.51 with bath, and moderately low with the rest. Only negative is cbd_distance. 
# colinearity between beds and bath (0.45), cartesian with cbd_distance


# Create a scatterplot of price vs. beds, baths
# investigated 7 bedroom houses - large houses in city
# investigates 1 bedroom, apartments, cottage, terrace
# investigated outliers at 2, actually 8x2 bedroom apartments 
# investigated outlier at 4, Edwardian home in expensive area  
# investigated outlier at 3, terrace 'in need of refurbishment' 
hd_no_outliers$beds_jitter <-ifelse(hd_no_outliers$region=='Limerick County', hd_no_outliers$beds+0.05, hd_no_outliers$beds-0.05)
bed_price_scatter <-ggplot(hd_no_outliers, aes(x = beds_jitter, y = price, color = region)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+ ylab('Price (thousands €)')+
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) + 
  xlab("Number of Bedrooms")+ region_colours +
  geom_point(size = 2) + 
  #scale_y_continuous(breaks = c(10, 11,12,13,14), labels =c("22,000", "60,000", "160,000", "440,000", "1,200,000")) + 
  theme_cowplot(font_size = 14)+ xlim(0, 7) 
bed_price_scatter


# scatterplot fo price vs baths
# invetigate the 4 outliers are derlict cotages, 'in need of refurbishment'
# invetigate outlier at 7, georgian country home
# outliers at 6 are large family homes 
# outlier at 3 is the same edwardian home as before 
hd_no_outliers$baths_jitter <-ifelse(hd_no_outliers$region=='Limerick County', hd_no_outliers$baths+0.05, hd_no_outliers$baths-0.05)
bath_price_scatter <-ggplot(hd_no_outliers, aes(x = baths_jitter, y = price, color = region)) +  
  theme(plot.title = element_text(hjust = 0.5))+ ylab('Price (thousands €)')+xlab("Number of Bathrooms")+
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) + 
  geom_point(size = 2) + region_colours+
  theme_cowplot(font_size = 14)+ xlim(0, 7)  + theme(axis.text.y = element_blank(), 
                                                           axis.title.y = element_blank())
# extract a legend that is laid out horizontally
legend_b <- get_legend(
  bed_price_scatter + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.justification="center", legend.box.just = "bottom") +
    labs(color = "Region")
)
title <- ggdraw() + 
  draw_label(
    "Price and Number of Bedrooms across Regions",
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 16
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )  

# combine bath and bed scatterplots with title and legend 
scatter_bed_bath <- plot_grid(bed_price_scatter + theme(legend.position = 'none'), NULL, bath_price_scatter + theme(legend.position = 'none'), nrow = 1, labels = c('A', 'B', ''), label_size = 14, rel_widths = c(1, 0.05, 1))
scatter_bed_bath
scatter_bed_bath_title_legend <-plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.2, 1, 1)),
  scatter_bed_bath,
  legend_b,
  ncol = 1,
  rel_heights = c(0.09, 1, 0.09)
)
scatter_bed_bath_title_legend
ggsave(filename = 'scatter_bed_bath.png', path = './Figures', width = 30, height = 15, units = 'cm')


    #### CBD Distance 
# CBD distance scatterplot 
hd_no_outliers$municipality <- factor(hd_no_outliers$municipality, levels = c("Limerick City and Suburbs", "Adare — Rathkeale","Cappamore — Kilmallock","Newcastle West" ))
cbd_dist_scatter <-ggplot(hd_no_outliers, aes(x = cbd_distance, y = price, color = municipality)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+ 
  labs(x = "CBD Distance (km)", y = 'Price (thousands €)', color = 'Municipality', title = 'Distance to CBD and Price by Municipality')+  municipality_colours + 
  geom_point(size = 2) + 
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "darkgrey") + 
  geom_vline(xintercept = 20, linetype = "dashed", color = "darkgrey")+   
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45, 50, 55)) + 
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) + 
  theme_cowplot(font_size = 14) + theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))
cbd_dist_scatter

# cbd distance range boxplot 
boxplot_cbd_dist <- ggplot(hd_no_outliers, aes(x = cbd_range.f, y = price)) + 
  geom_boxplot(fill = 'grey80') + labs( title = "Price Across CBD Ranges", x = 'CBD Range (km)', y = 'Price (thousands €)') + 
  theme_cowplot(font_size = 14) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) + 
  scale_x_discrete(labels = c('< 5', '5 to 10', '10 to 20', '> 20')) + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank())
boxplot_cbd_dist
cbd_dist_scatter_box <- plot_grid(cbd_dist_scatter + theme(legend.position = 'none'), boxplot_cbd_dist, rel_widths  = c(1, 0.6), nrow =1, labels = c("A", "B"), label_size = 14)
legend <- get_legend(cbd_dist_scatter)
scatter <- plot_grid(cbd_dist_scatter_box, plot_grid(NULL, legend, NULL, rel_widths = c(0.05, 1, 1)), nrow = 2, rel_heights = c(1, 0.1))
scatter
ggsave(filename = 'cbd_dist_scatter_box.png', path = './Figures', width = 30, height = 15, units = 'cm')

# days_to_sell
# price does not change much for longer 
cor(hd$price_log, hd$days_to_sell, use = 'complete.obs') # weak linear relationship 
ggplot(hd, aes(x = (days_to_sell), y = price_log)) +
  geom_point()



### property region 
# boxplot of price by region and category 
box_price_regions <- ggplot(hd_no_outliers, aes(x = property_category, y = price, fill = region)) +
  geom_boxplot()   +
  labs(title = "Price of Properties Across Regions", x = 'Property Category', y = 'Price (thousands €)') + theme_cowplot(font_size = 14)   +
  region_colours_fill+ theme(axis.text.x = element_text(angle = 10, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) 

# boxplot of houses price by property type and region 
box_price_houses_regions <- ggplot(hd_no_outliers[hd_no_outliers$property_category == 'House',], aes(x = property_type, y = price, fill = region)) + 
  geom_boxplot() + labs( title = "Price of Houses Across Regions", x = 'Property Type', y = 'Price (thousands €)', fill = 'Region') + 
  theme_cowplot(font_size = 14) + theme(axis.text.x = element_text(angle = 10, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  region_colours_fill + theme(legend.position = 'bottom',legend.justification="center", legend.box.just = "bottom") + 
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank())

# extract legend for plot 
legend_region <- get_legend(
  box_price_houses_regions + 
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", legend.justification="center", legend.box.just = "bottom")
)

# plot grids of two boxplots and legend 
grid1 <- plot_grid(box_price_regions + theme(legend.position = 'none'), box_price_houses_regions+ theme(legend.position = 'none'), labels = c('A', 'B'), label_size = 14, align = 'h')
box_price_regions_legend <-plot_grid(
  grid1,
  legend_region,
  ncol = 1,
  rel_heights = c(1, 0.09)
)
box_price_regions_legend
ggsave(filename = 'boxplot_region_property_type.png', path = './Figures', width = 30, height = 15, units = 'cm')




### Dummy Variables 
# garage 
box_plot_garage <- ggplot(hd_no_outliers, aes(x = garage, y = price, fill = region)) +
  geom_violin()   + region_colours +
  labs(title = "Price (€) of Properties with Garages Across Regions", x = 'Garage', y = 'Price (thousands €)') + theme_cowplot(font_size = 16)   +
  theme(plot.title = element_text(hjust = 0.5))
box_plot_garage

# period 
box_plot_period <- ggplot(hd_no_outliers, aes(x = period, y = price, fill = region)) +
  geom_violin()   +
  labs(title = "Price (€) of Period Properties Across Regions", x = 'Period Property', y = 'Price (thousands €)') + theme_cowplot(font_size = 16)   +
  region_colours  + theme(plot.title = element_text(hjust = 0.5))
box_plot_period

table(hd$fireplace, hd$property_category)

# fireplace
box_plot_fireplace <- ggplot(hd_no_outliers, aes(x = fireplace, y = price, fill = property_category)) +
  geom_violin()   +
  labs(title = "Price (€) of Properties with Fireplace Across Regions", x = 'Fireplace', y = 'Price (thousands €)') + theme_cowplot(font_size = 16)   +
   theme(plot.title = element_text(hjust = 0.5))
box_plot_fireplace 
# increase in property price for apartments with fireaplce. less of an increase vs hosues 

# cul de sac 
box_plot_cul_de_sac <- ggplot(hd_no_outliers[hd_no_outliers$property_category == 'House',], aes(x = cul_de_sac, y = price, fill = region)) + 
  geom_violin() + labs( title = "Price (€) of Properties with Cul de sac Across Regions", x = 'Cul-de-sac', y = 'Price (thousands €)', fill = 'Region') + 
  theme_cowplot(font_size = 16) + theme(plot.title = element_text(hjust = 0.5)) +
   theme(legend.position = 'bottom',legend.justification="center", legend.box.just = "bottom")
box_plot_cul_de_sac


legend_region <- get_legend(
  box_plot_cul_de_sac + 
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", legend.justification="center", legend.box.just = "bottom")
)
grid1 <- plot_grid(box_plot_garage + theme(legend.position = 'none'), box_plot_period+ theme(legend.position = 'none'), box_plot_fireplace+ theme(legend.position = 'none'), box_plot_cul_de_sac+ theme(legend.position = 'none'), labels = c('A', 'B', 'C', 'D'), label_size = 18)
box_price_dummy_regions <-plot_grid(
  grid1,
  legend_region,
  ncol = 1,
  rel_heights = c(1, 0.09)
)
box_price_dummy_regions





##########################################################
## Summary Statistics 
##########################################################

# summary stats by region 
region_summary <- hd %>%
  group_by(region) %>%
  summarise(property_count = length(id),
            min_price = min(price),
            mean_price = mean(price),
            median_price = median(price),
            max_price = max(price),
            IQR = IQR(price),
            mean_bed = mean(beds),
            mean_bath = mean(baths),
            house_count = sum(property_category == "House"),
            apartment_count = sum(property_category == "Apartment"),
            house_prop = house_count/property_count,
            apartment_prop = apartment_count/property_count,
            mean_cbd_distance = mean(cbd_distance),
            garage_prop = sum(garage)/property_count,
            cul_de_sac_prop = sum(cul_de_sac)/property_count, 
            fireplace_prop = sum(fireplace)/property_count,
            period_prop = sum(period)/property_count)

table_region_summary<- region_summary %>%
  gt() %>%
  tab_spanner(
    label = "Summary Statistics",
    columns = c(min_price, mean_price, median_price, max_price, IQR)
  ) %>%
  fmt_number(
    columns = c(
      min_price, mean_price, median_price, max_price, IQR,
      mean_bed, mean_bath, house_count, apartment_count,
      house_prop, apartment_prop, mean_cbd_distance,
      garage_prop, cul_de_sac_prop, fireplace_prop, period_prop
    ),
    decimals = 2
  )

# summary stats by municipality 
box_price_municipality <- ggplot(hd, aes(x = municipality, y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price Across Municipalities")
box_price_municipality
summary_by_municipality
municipality_summary <- hd %>%
  group_by(municipality) %>%
  summarise(property_count = length(id),
            min_price = min(price),
            mean_price = mean(price),
            median_price = median(price),
            max_price = max(price),
            IQR = IQR(price),
            mean_bed = mean(beds),
            mean_bath = mean(baths),
            house_count = sum(property_category == "House"),
            apartment_count = sum(property_category == "Apartment"),
            house_prop = house_count/property_count,
            apartment_prop = apartment_count/property_count,
            mean_cbd_distance = mean(cbd_distance),
            garage_prop = sum(garage)/property_count,
            cul_de_sac_prop = sum(cul_de_sac)/property_count, 
            fireplace_prop = sum(fireplace)/property_count,
            period_prop = sum(period)/property_count)
print(municipality_summary)

table_municipality_summary<- municipality_summary %>%
  gt() %>%
  tab_spanner(
    label = "Summary Statistics",
    columns = c(min_price, mean_price, median_price, max_price, IQR)
  ) %>%
  fmt_number(
    columns = c(
      min_price, mean_price, median_price, max_price, IQR,
      mean_bed, mean_bath, house_count, apartment_count,
      house_prop, apartment_prop, mean_cbd_distance,
      garage_prop, cul_de_sac_prop, fireplace_prop, period_prop
    ),
    decimals = 2
  )

hd_table <- hd %>%
  mutate(tableid = case_when(
    municipality == 'Limerick City and Suburbs' ~ 1,
    TRUE ~ 0
  )) 


# summary stats by urban area 
urban_area_summary <- hd %>%
  group_by(region, urban_area) %>%
  summarise(
            property_count = length(id),
            min_price = min(price),
            mean_price = mean(price),
            median_price = median(price),
            max_price = max(price),
            IQR = IQR(price),
            mean_bed = mean(beds),
            mean_bath = mean(baths),
            house_count = sum(property_category == "House"),
            apartment_count = sum(property_category == "Apartment"),
            #house_prop = house_count/property_count,
            #apartment_prop = apartment_count/property_count,
            mean_cbd_distance = mean(cbd_distance),
            garage_prop = sum(garage)/property_count,
            cul_de_sac_prop = sum(cul_de_sac)/property_count, 
            fireplace_prop = sum(fireplace)/property_count, 
            period_prop = sum(period)/property_count)
urban_area_summary <- urban_area_summary %>% arrange(desc(region))

# variable summmary across municiapltieis 
municipal_variable_sum <- hd %>%
  group_by(municipality) %>%
  summarise(
    property_count = length(id),
    mean_bed = round(mean(beds),2),
    mean_bath = round(mean(baths),2),
    mean_cbd_distance = round(mean(cbd_distance),2),
    house_count = sum(property_category == "House"),
    house_prop = round(house_count/property_count,2),
    apartment_count = sum(property_category == "Apartment"),
    apartment_prop = paste0("(",round(apartment_count/property_count,2),")"),
    garage_count = sum(garage),
    garage_prop = paste0("(",round(sum(garage)/property_count,2),")"),
    cul_de_sac_count = sum(cul_de_sac),
    cul_de_sac_prop = paste0("(",round(sum(cul_de_sac)/property_count,2),")"),
    fireplace_count = sum(fireplace),
    fireplace_prop = paste0("(",round(sum(fireplace)/property_count,2),")"),
    period_count = sum(period),
    period_prop = paste0("(",round(sum(period)/property_count,2),")"))
colnames(municipal_variable_sum)[colnames(municipal_variable_sum) == "municipality"] <- "area"

# variable summmary across urban areas 
urban_area_variable_sum <- hd %>%
  group_by(urban_area) %>%
  summarise(
    property_count = length(id),
    mean_bed = round(mean(beds),2),
    mean_bath = round(mean(baths),2),
    mean_cbd_distance = round(mean(cbd_distance),2),
    house_count = sum(property_category == "House"),
    house_prop = round(house_count/property_count,2),
    apartment_count = sum(property_category == "Apartment"),
    apartment_prop = paste0("(",round(apartment_count/property_count,2),")"),
    garage_count = sum(garage),
    garage_prop = paste0("(",round(sum(garage)/property_count,2),")"),
    cul_de_sac_count = sum(cul_de_sac),
    cul_de_sac_prop = paste0("(",round(sum(cul_de_sac)/property_count,2),")"),
    fireplace_count = sum(fireplace),
    fireplace_prop = paste0("(",round(sum(fireplace)/property_count,2),")"),
    period_count = sum(period),
    period_prop = paste0("(",round(sum(period)/property_count,2),")"))
colnames(urban_area_variable_sum)[colnames(urban_area_variable_sum) == "urban_area"] <- "area"
variable_summaries <- rbind(municipal_variable_sum, urban_area_variable_sum)
kbl(variable_summaries, booktabs = T, format = 'latex') %>% kable_styling(latex_options = c("scale_down"))




# property condition 

       
box_price_condition <- ggplot(hd, aes(x = property_condition, y = price_log)) +
  geom_boxplot() 
box_price_condition
summary_by_condition <- hd %>%
  group_by(property_condition) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_condition

# property category 
box_price_category <- ggplot(hd, aes(x = property_category, y = price_log)) +
  geom_boxplot() 
box_price_category
summary_by_category <- hd %>%
  group_by(property_category) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_category

# property type 


hd$property_type <- factor(hd$property_type , levels=c("Apartment", "Duplex", "Detached", "Semi-Detached", "Terraced", "End-of-Terrace", "Townhouse"))
box_price_type <- ggplot(hd_no_outliers, aes(x = property_type, y = price)) +
  geom_boxplot( fill = '#529AC6')   + ggtitle("Price for Property Types")+
  theme_cowplot(font_size = 18) + theme(axis.text.x = element_text(angle = 10, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  xlab("Property Type") + ylab("Price (thousands €)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
box_price_type
summary_by_type <- hd %>%
  group_by(property_type) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_type
kbl(summary_by_type, booktabs = T, format = 'latex')

# outliers in Apartment  
# upper = prestigrous apartment in centre of city for 360000

# outliers in Detached 
# lower = derilect cottages, homes in need of reservation 
# upper end = georgian, highly suaght after areas. 

# outliers in End of Terrace  
# upper = 6 bedroom house in centre for 900000



# sale_outcome
box_price_sale_outcome <- ggplot(hd, aes(x = sale_outcome, y = price_log)) +
  geom_boxplot() 
box_price_sale_outcome
summary_by_sale_outcome <- hd %>%
  group_by(sale_outcome) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_sale_outcome

# cul de sac 
box_price_cul_de_sac <- ggplot(hd, aes(x = cul_de_sac, y = price_log)) +
  geom_boxplot() 
box_price_cul_de_sac
summary_by_cul_de_sac <- hd %>%
  group_by(cul_de_sac) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_cul_de_sac

# fireplace
box_price_fireplace <- ggplot(hd, aes(x = fireplace, y = price_log)) +
  geom_boxplot() 
box_price_fireplace
summary_by_fireplace <- hd %>%
  group_by(fireplace) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_fireplace


# garage
box_price_garage <- ggplot(hd, aes(x = garage, y = price_log)) +
  geom_boxplot() 
box_price_garage
summary_by_garage <- hd %>%
  group_by(garage) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_garage

# garage
box_price_period <- ggplot(hd, aes(x = period, y = price_log)) +
  geom_boxplot() 
box_price_period
summary_by_period <- hd %>%
  group_by(period) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_period

##########################################################
## Temporal Analysis
##########################################################

box_plot_year <- ggplot(hd, aes(x = years.f, y = price_log)) +
  geom_boxplot() 
box_plot_year
summary_by_year <- hd %>%
  group_by(years.f) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_year

monthly_means <- hd %>% group_by(month_year.f, region) %>% summarise(monthly_mean = mean(price))

line_plot_region <- ggplot(monthly_means, aes(x = month_year.f, y = monthly_mean, color = region, group = region)) +
  geom_line() +
  labs(x = "Month", y = "Mean Price", title = "Mean Price Per Region Over Time") + 
  theme_minimal()
line_plot_region

table(hd$region, hd$month_year)

summary_by_year <- hd %>%
  group_by(years.f) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Sdev = sd(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    n = length(id)
  )
summary_by_year




##########################################################
## Geographical Area 
##########################################################

# creating codes for municipalities and regions 
value_mapper <- c("Adare — Rathkeale" = "AR",
                  "Cappamore — Kilmallock" = "CK",
                  "Newcastle West" = "NW",
                  "North" = "LCS-1",
                  "City North" = "LCS-2",
                  "Corbally" = "LCS-3",
                  "City South" = "LCS-4",
                  "West" = "LCS-5",
                  "Ballycummin" = "LCS-6",
                  "Outer City South" = "LCS-7",
                  "Ballysimon" = "LCS-8", 
                  "East" = "LCS-9"

                  )
order <- c()
for(i in 1:12){order[i] <- value_mapper[[i]]}
hd_no_outliers$codes <- value_mapper[match(hd_no_outliers$urban_area, names(value_mapper))]
hd_no_outliers$codes.f <- factor(hd_no_outliers$codes, levels = order)
hd_no_outliers$region <- factor(hd_no_outliers$region, levels = c('Limerick County', 'Limerick City and Suburbs'))

# Municipality prices boxplot and map of municipality locations 
region_box_plot <- ggplot(hd_no_outliers, aes(x = codes.f, y = price, fill = region)) + 
  ggtitle("Price across Limerick Areas")+ 
  geom_boxplot() + 
  labs(x = "Area", y = "Price (thousands €)", fill = "Region") + 
  theme_cowplot(font_size = 14) + 
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000), labels = c('100', '200', '300', '400', '500')) + 
  region_colours_fill + 
  theme(legend.position = "bottom", legend.justification = 'center', plot.title = element_text(hjust = 0.5))
region_box_plot
ggsave(filename = 'region_box_plot.png', path = './Figures', width = 30, height = 15, units = 'cm')


# Urban area boxplot 
table(hd$urban_area)
urban_area_box_plot <- ggplot(hd, aes(x = urban_area, y = price_log, fill = region)) + 
  ggtitle("ln(price) across Limerick Urban Areas")+ 
  geom_boxplot() + region_colours +
  xlab("Urban Area") + ylab('ln(price)') +theme_cowplot(font_size = 24) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 1), plot.title = element_text(hjust = 0.5))
urban_area_box_plot

# histograms by reginos 
ggplot(hd_no_outliers, aes(price, fill = region)) + geom_histogram(alpha = 0.3, position = 'identity', bins = 30) + theme_cowplot()


# boxplot of price by municipality and property_category 
ggplot(hd, aes(x = municipality, y = price_log, fill = property_category)) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# boxplot of price by urban_area and property_category 
ggplot(limerick_city, aes(x = urban_area, y = price_log, fill = property_category)) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################
## Spatial Stats
##########################################################

spatial_summary <- data.frame(hd %>% summarise(
  longitude_min = min(longitude),
  longitude_median = median(longitude),
  longitude_max = max(longitude),
  latitude_min = min(latitude),
  latitude_median = median(latitude),
  latitude_max = max(latitude),
  cartesian_x_min = min(cartesian_x),
  cartesian_x_median = median(cartesian_x),
  cartesian_x_max = max(cartesian_x),
  cartesian_y_min = min(cartesian_y),
  cartesian_y_median = median(cartesian_y),
  cartesian_y_max = max(cartesian_y),
  cbd_distance_min = min(cbd_distance),
  cbd_distance_median = median(cbd_distance),
  cbd_distance_max = max(cbd_distance)))

summary(hd$longitude)
summary(hd$latitude)
summary(hd$cartesian_x)
summary(hd$cartesian_y)
summary(hd$cbd_distance)




##########################################################
## Further Work
##########################################################


## Noteable Addresses 
table(hd$address_area, useNA = 'ifany')
hd <- hd %>%
  mutate(
    notable_address = case_when(
      address_area %in% c('Adare', 'Annacotty', 'Castleconnell', 'Rathkeale', 'Prospect', 'Castletroy', 'Caherdavin', 'Dooradoyle', 'North Circular Road', 'Ennis Road') ~ address_area,
      TRUE ~ "Limerick"
    )
  )

summary_notable_addresses <- hd %>%
  group_by(notable_address) %>%
  summarise(mean_price = mean(price),
            median = median(price))
ggplot(hd, aes(x = notable_address, y = price, fill = notable_address)) + geom_boxplot() + theme_cowplot()



## BER rating exploration 
hd %>% group_by(ber) %>% summarise(mean = mean(price), count = length(id), derelict_count = sum(property_condition=="Derelict"), new_count = sum(property_condition=="New"),second_count = sum(property_condition=="Second Hand") )
table(hd$ber, hd$property_condition, useNA = 'ifany')

ber_boxplot <- ggplot(hd, aes(y = price_log, x = ber)) + geom_boxplot() + theme_cowplot()
# we can see a reduction in median price for ratings in E, F, G

size_price_scatter <- ggplot(hd, aes(y = price_log, x = size)) + geom_point()
# there is a non-linear relationship between price_log and size.
size_price_scatter_log <- ggplot(hd, aes(y = price_log, x = log(size))) + geom_point()
# consider a log transformed size. 
summary(hd$size)

