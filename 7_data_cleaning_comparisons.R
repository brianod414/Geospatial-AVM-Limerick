###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Representing the improvements before and after data cleanign 
## Date: 19/10/2023
## Author: BOD
###########################################################################
library(gt)
old_data <- readr::read_csv("./Raw Data/pre_cleaning_data.csv")

dim(old_data)
colnames(old_data)
colSums(is.na(old_data))




# Present vs missing - old data 

old_data_key <- dplyr::select(old_data, c("price", "location", "baths", "beds", "property_type", "ber","area"  ,"size",'description'))

old_data_key <- old_data_key %>%
  rename("property type"= property_type,
         'BER' = 'ber')


missing.values <- old_data_key %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100, key = str_to_title(key)) 

missing.values$key[missing.values$key == 'Ber'] <- c('BER', 'BER')


# Extract levels with missing values
levels_with_missing <- ((missing.values  %>% filter(isna == TRUE) %>% arrange(desc(pct)))$key)

# Extract levels with no missing values
levels_no_missing <- (setdiff(unique(missing.values$key), levels_with_missing))

# Create a new variable 'present_category' to distinguish between missing and non-missing values
missing.values <- missing.values %>%
  mutate(present_category = ifelse(!isna, "Present", "Missing"))

# Change the order of levels for present_category
missing.values$present_category <- factor(missing.values$present_category, levels = c("Missing", "Present"))

# Create additional rows for 'Present' for 'price' and 'location'
additional_present_rows <- data.frame(
  key = c("Price", "Location"),
  total = c(12435, 12435),
  isna = c(TRUE, TRUE),
  num.isna = c(0, 0),
  pct = c(0, 0),
  present_category = "Missing"
)

# Combine the original data with the additional rows
missing.values <- bind_rows(missing.values, additional_present_rows)

# Filter only the relevant levels for the plot
filtered_missing.values <- filter(missing.values, key %in% c(levels_with_missing, levels_no_missing))

percentage.plot.old <- ggplot(filtered_missing.values, aes(x = reorder(key, desc(total - num.isna)), y = total - num.isna, fill = present_category)) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  scale_x_discrete(limits = c(levels_with_missing, levels_no_missing)) +
  scale_fill_manual(name = "Value", values = c('#7AD1EC', '#8a8a8a'), labels = c("Present", "Missing")) +
  coord_flip() + theme_cowplot(14) + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Key Variables: Missing vs Present Values", y = "Number of Missing Values", x = "Variable")+ theme(axis.title.y = element_blank())
print(percentage.plot.old)

ggsave(filename = 'missing_vs_present.png', path = './Figures', width = 20, height = 10, units = 'cm')





cleaned_data <- readr::read_csv("./Cleaned Data/houses_apartments_limerick_complete.csv")

dim(cleaned_data)
colnames(cleaned_data)
colSums(is.na(cleaned_data))

cleaned_data_key <- select(cleaned_data, c("price", "location", "baths", "beds", "property_type", "ber","area"  ,"size", "description"))

missing.values <- cleaned_data_key %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

# Extract levels with missing values
levels_with_missing <- (missing.values  %>% filter(isna == TRUE) %>% arrange(desc(pct)))$key

# Extract levels with no missing values
levels_no_missing <- setdiff(unique(missing.values$key), levels_with_missing)

# Create a new variable 'present_category' to distinguish between missing and non-missing values
missing.values <- missing.values %>%
  mutate(present_category = ifelse(!isna, "Present", "Missing"))

# Change the order of levels for present_category
missing.values$present_category <- factor(missing.values$present_category, levels = c("Missing", "Present"))

# Create additional rows for 'Present' for 'price' and 'location'
additional_present_rows <- data.frame(
  key = c("price", "location", "description", 'baths', 'property_type', 'beds'),
  total = c(1520, 1520, 1520,1520,1520,1520),
  isna = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  num.isna = c(0, 0, 0, 0, 0, 0),
  pct = c(0, 0, 0, 0, 0, 0),
  present_category = "Missing"
)

# Combine the original data with the additional rows
missing.values <- bind_rows(missing.values, additional_present_rows)

# Define the order of the variables
desired_order <- c("price", "location", "area", "beds", "property_type", "baths", "description", "ber", "size")

# Convert 'key' to a factor with the desired order
missing.values <- missing.values %>%
  mutate(key = factor(key, levels = desired_order))

# Filter only the relevant levels for the plot
filtered_missing.values <- filter(missing.values, key %in% c(levels_with_missing, levels_no_missing))

percentage.plot <- ggplot(filtered_missing.values, aes(x = key, y = total - num.isna, fill = present_category)) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  scale_fill_manual(name = "Value", values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  theme_cowplot() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Key Variables Missing vs Present values.", x = 'Variable.', y = "Number of Missing Values.")

print(percentage.plot)



















summary(old_data$price)





####### Analyse price spread 
price_old <- ggplot(old_data, aes(y = price)) + geom_boxplot(fill = '#1aa7ec')  + 
  theme_cowplot(font_size = 14) + labs(y = 'Price (thousands €)') + ggtitle('Price Before Data Cleaning') + 
  theme( plot.title = element_text(hjust = 0.5) )+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(breaks = c(1000000, 2000000,3000000,4000000,5000000), labels =c("1,000", "2,000", "3,000", "4,000", "5,000"))
price_new <- ggplot(cleaned_data, aes(y = price)) + geom_boxplot(fill = '#1aa7ec')+ 
  theme_cowplot(font_size = 14)+ labs(y = 'Price (thousands €)') + ggtitle('Price After Data Cleaning') + 
  theme( plot.title = element_text(hjust = 0.5) )+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous(breaks = c(250000, 500000,750000,1000000, 1250000), labels =c("250", "500", "750", "1,000", "1,250"))
price_before_after_box <- plot_grid(price_old, price_new, labels = c("A","B" ), label_size = 14)
price_before_after_box
ggsave(filename = 'price_before_after_box.png', path = './Figures', width = 24, height = 12, units = 'cm')








old_data[old_data$property_type=='Land', ] %>% summarise(
  Mean = mean(price, na.rm = TRUE),
  Sdev = sd(price, na.rm = TRUE),
  Q1 = quantile(price, 0.25, na.rm = TRUE),
  Median = median(price, na.rm = TRUE),
  Q3 = quantile(price, 0.75, na.rm = TRUE),
  Min = min(price, na.rm = TRUE),
  Max = max(price, na.rm = TRUE)
)

new_data %>% summarise(
  Mean = mean(price, na.rm = TRUE),
  Sdev = sd(price, na.rm = TRUE),
  Q1 = quantile(price, 0.25, na.rm = TRUE),
  Median = median(price, na.rm = TRUE),
  Q3 = quantile(price, 0.75, na.rm = TRUE),
  Min = min(price, na.rm = TRUE),
  Max = max(price, na.rm = TRUE)
)


####### Analyse property types 
table(old_data$property_type_original[old_data$property_type=="House" ])



####### Geography 


limerick_county_map <- ggplot(limerick_county_border) + geom_sf() +
  scale_fill_viridis() + theme_bw()
ggplot(data = old_data, aes(x = longitude, y = latitude)) + geom_point(size = 2)

ggplot(data = cleaned_data, aes(x = longitude, y = latitude)) + geom_point(size = 2)


limerick_county_map +
  geom_point(data = old_data, aes(x = longitude, y = latitude), size = 2)
