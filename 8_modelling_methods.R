###########################################################################
## Project: MS4037 Final Thesis 
## Script purpose: Modelling
## Date: 19/10/2023
## Author: BOD
###########################################################################

##########################################################
## Load Packages 
##########################################################

my_libraries()
# Remove scientific Notation 
options(scipen = 999)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

#####################################################################################################################################
## Functions
#####################################################################################################################################


my_libraries <- function() {
  library(tidyverse)
  library(datawizard)
  library(xtable)
  library(emmeans)
  library(car)
  library(janitor)
  library(cowplot) # has a nice theme for ggplot and allows you to combine multiple
  library(leaflet)
  library(xtable)
  library(osmdata)
  library(ggmap)
  library(stringr)
  library(geosphere)
  library(sf)
  library(terra)
  library(tmap)
  library(dplyr)
  library(spData)
  library(viridis)
  library(tidyverse)
  library(rstatix)
  library(ggpubr)
  library(tidygeocoder)
  library(kableExtra)
  library(kableExtra)
  library(knitr)
  library(corrplot)
  library(zeallot)
  library(car)
  library(jtools)
  library(tmap)
  library(tmaptools)
  library(GWmodel)
  library(grid)
  library(Metrics)
  library(caret)
  library(MASS)
  library(gridExtra)
  library(gam)
  library(spdep)
  library(raster)
  library(ape)
  library(spatialreg)
}

my_my_cooksd_calc_plot <- function(model) {
  n <- length(model$residuals)
  # Plot Cook's Distance
  plot(cooks.distance(model), main = "Cook's Distance for Influential Obs", 
       xlab = "Observation", ylab = "Cook's Distance", pch = 19)
  # Add a horizontal line at 4/n
  abline(h = 4/n, lty = 2, col = "red")
}


my_gwr_map <- function(map_data, color_by){
  return(map_urban_area_labels +
  geom_sf(data = map_data, aes(color = color_by), size = 1) +
  scale_color_gradient(low = "red", high = "green") +
  theme_map())}

my_model_metrics <- function(model, model_data, fitted = exp(model$fitted.values)) {
  # Extract actual and predicted values
  actual_val <- model_data$price
  # Calculate percentage within 10% and 20%
  percentage_within_10_percent <- sum(abs(fitted - actual_val) / actual_val <= 0.1) / length(actual_val)
  percentage_within_20_percent <- sum(abs(fitted - actual_val) / actual_val <= 0.2) / length(actual_val)
  # Calculate 50 and 95% prediction interval
  PI_95 <- exp(predict(model, newdata = model_data, interval = "predict")) %>% as.data.frame()
  # Calculate percentage within 95% prediction interval
  percentage_within_95_PI <- sum(model_data$price >= PI_95$lwr & model_data$price <= PI_95$upr) / length(actual_val)
  # Calculate various metrics and store in a data frame
  ols_metrics <- c(
    Adj_R2 = summary(model)$adj.r.squared,
    AIC = AIC(model),
    RMSE = rmse(actual_val, fitted),
    MedianAPE = (cognitiveutils::MDAPE(actual_val, fitted))*100,
    # MeanAPE = mape(actual_val, fitted),
    # MeanAE = mae(actual_val, fitted),
    Within10_True = (percentage_within_10_percent)*100,
    Within20_True = (percentage_within_20_percent)*100,
    Within95_PredInt = (percentage_within_95_PI)*100
  ) 
  return(round(ols_metrics,3))
}


my_gwr_map_theme <-   theme(
  legend.position = "bottom",
  legend.key.width = unit(1.5, 'cm'),
  legend.spacing.x = unit(1, 'cm'),
  legend.justification = 'center',
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  plot.title = element_text(hjust = 0.5)
)

my_cross_validation <- function(model_formula, fold_list = fold_list){
  metrics_list <- list()
  for (i in 1:10){
    train_set <- hd[!(hd$id %in% fold_list[[i]]$id), ]
    test_set <- fold_list[[i]]
    fitted_model <- lm(model_formula, data = train_set) 
    metrics_list[[i]] <- my_model_metrics(fitted_model, test_set, exp(predict(newdata = test_set, fitted_model)))
  }
  metrics_list_df <- do.call(rbind, metrics_list)
  mean_metrics <- as.data.frame(colMeans(metrics_list_df))
  return(mean_metrics)
}


my_gwr.collin.diagno <- function(formula, bw, kernel){
  return(gwr.collin.diagno(formula, model_data_sp, bw, kernel=kernel,
                           adaptive=T, p=2, theta=0, longlat=T,model_dist_matrix))
}


my_third_degree_poly <- function(B1, B2, B3, X){
  poly <- B1 + B2 + B3 + (2*B2 + 3*B3)*X + 3*B3*X^2
  value <- exp(poly)
  return(value)
}

my_bed_urban_area <- function(B1, B2, BD, X){
  poly <- B1 + B2 + BD + 2*B2*X
  value <- exp(poly)
  return(value)
}

my_loglinear_coeff <- function(ols_model){
  ols_coeff <- round(as.data.frame(cbind(estimate = ols_model$coefficients, lower = confint(ols_model)[,1], upper = confint(ols_model)[,2])) %>% 
                       mutate(expest = exp(estimate), explower = exp(lower), expupper = exp(upper), 
                              percentage_change = (exp(estimate)-1)*100),3) 
  ols_coeff <- cbind(label = rownames(ols_coeff), ols_coeff) 
  rownames(ols_coeff) <- NULL
  return(ols_coeff)
}



##########################################################
## Prepare hd for modelling 
##########################################################

hd <- readr::read_csv("./Cleaned Data/houses_apartments_limerick_complete.csv")
# write.csv(hd, "Cleaned Data/houses_apartments_limerick_complete.csv", row.names = FALSE)
hd <- dplyr::select(hd, c("id", "price", "latitude", "longitude", "baths", "beds", "property_category", "property_type", "property_condition", "years", "month_year", "quarter", "price_log", "municipality", "region", "urban_area", "cbd_distance", "cul_de_sac", "fireplace", "garage", "period", "cartesian_x", "cartesian_y"))
limerick_city <- hd[hd$municipality=='Limerick City and Suburbs', ]
limerick_co <- hd[hd$municipality!='Limerick City and Suburbs', ]


# municipality factor 
# property_type factor 
# month_year factor 

hd$cbd_range <- cut(hd$cbd_distance,
                    breaks = c(-Inf, 5, 10, 20, Inf),
                    labels = c("in5", "in5to10", "in10to20", "over20"),
                    right = FALSE)
hd$cbd_range.f <- factor(hd$cbd_range)


hd$property_category.f <- relevel(as.factor(hd$property_category), ref = "House")
hd$property_type.f <- relevel(as.factor(hd$property_type), ref = "Detached")
hd$urban_area.f <- relevel(as.factor(hd$urban_area), ref = "Outer City South")
hd$municipality.f <- relevel(as.factor(hd$municipality), ref = "Newcastle West")


hd$municipality.f <- relevel(as.factor(hd$municipality), ref = 'Newcastle West')
hd$property_condition.f <- relevel(as.factor(hd$property_condition), ref = "Second Hand")
hd$period.f <- as.factor(hd$period)
hd$month_year.f <- relevel(as.factor(hd$month_year), ref = "1")

hd$year.f <- relevel(as.factor(hd$years), ref = "2017")
hd$quarter.f <- relevel(as.factor(hd$quarter), ref = "1")
hd$period.f <- factor(hd$period)
hd$garage.f <- factor(hd$garage)
hd$fireplace.f <- factor(hd$fireplace)
hd$cul_de_sac.f <- factor(hd$cul_de_sac)


# contrasts 
contrasts(hd$property_category.f) <- contr.sum(2)
contrasts(hd$property_type.f) <- contr.sum(7)
contrasts(hd$urban_area.f)<- contr.sum(12)
contrasts(hd$municipality.f)<- contr.sum(4)
contrasts(hd$property_condition.f)<- contr.sum(4)
contrasts(hd$year.f) <- contr.sum(2)
contrasts(hd$month_year.f) <- contr.sum(23)
contrasts(hd$quarter.f)<- contr.sum(levels(hd$quarter.f))
contrasts(hd$cbd_range.f) <- contr.sum(4)


# fireplace T/F
# garage T/F
# cul de sac T/F

# cbd_distance (linear)
# beds_c_c (linear and polynomial)
# baths_c (linear and polynomial)

# shapefile 
hd_sf <- st_as_sf(hd, coords = c("longitude", "latitude"), crs = 4326)

# Merge limerick urban area shapefile with hd
hd_urban_area <- merge(hd, limerick_sf, by = 'urban_area')
hd_urban_area_sf <- st_as_sf(hd_urban_area)

# Merge limerick municipality shapefile with hd
hd_municipality <- merge(hd, limerick_municipalities_sf, by = 'municipality')
hd_municipality_sf <- st_as_sf(hd_municipality)

# Limerick city with urban areas
hd_city_urban_area <- hd_urban_area[hd_urban_area$region.x == 'Limerick City and Suburbs',]
hd_city_urban_area_sf <- st_as_sf(hd_city_urban_area)

# relevel the beds_c and baths_c for better itnercept interpretability 
hd$beds_c <- hd$beds - 3
hd$baths_c <- hd$baths - 2
hd$cartesian_x_c <- hd$cartesian_x - mean(hd$cartesian_x)
hd$cartesian_y_c <- hd$cartesian_y - mean(hd$cartesian_y)

summary(hd$baths_c)
summary( hd$baths)
summary(hd$cartesian_x_c)
summary( hd$cartesian_x)

### Train and Test Data 
# Train Data 80% of the sample size
# Test Data 20% of the sample size 
smp_size <- floor(dim(hd)[1]*0.8)
set.seed(17)
train_ind <- sample(seq_len(nrow(hd)), size = smp_size)

train_data80 <- hd[train_ind, ]
test_data20 <- hd[-train_ind, ]

# check values are sufficient
dim(train_data80)
table(train_data80$urban_area.f)
table(train_data80$property_condition.f)
table(train_data80$property_type)
table(train_data80$fireplace)
table(train_data80$cul_de_sac)
table(train_data80$year.f)
table(train_data80$month_year)
table(train_data80$quarter)
table(train_data80$period.f)




## Cross Validation Sets 
# Set seed for reproducibility, 5 fold validation 
set.seed(2)
num_folds <- 10
fold_size <- ceiling(nrow(hd) / num_folds)

# Create a vector indicating which chunk each row belongs to
split_indeces <- rep(1:num_folds, each = fold_size)
split_indeces <-  split_indeces[1:1497]
# Split the data into chunks of 20% of the training data. 
random_hd <- hd[sample(nrow(hd)), ]
fold_list <- split(random_hd, split_indeces)


# Checking if sufficient values within each variable for training the model. 
for (i in 1:10){
  print(" ----------------- -----------------this is a new dataset  ----------------- -----------------" )
  validation_set <- hd[!(hd$id %in% fold_list[[i]]$id), ]
  print(dim(validation_set))
  print(table(validation_set$urban_area.f))
  print(table(validation_set$property_condition.f))
  print(table(validation_set$property_type))
  print(table(validation_set$fireplace))
  print(table(validation_set$cul_de_sac))
  print(table(validation_set$year.f))
  print(table(validation_set$month_year))
  print(table(validation_set$quarter))
  print(table(validation_set$period.f))
}



model_data <- hd
model_data_sf <- st_as_sf(model_data, coords = c("longitude", "latitude"), crs = 4326)



#####################################################################################################################################
## Spatial Autocorrelation
#####################################################################################################################################

#### Test for Price 
# Morans I: H0: the spatial process undering x is a random process 
# Mornas I >0 means high vales cluster with high values.. etc (spatial values more clustered than would be if underlying spatial processes were random)
# pvalues: is the difference between observed and expected significant 

# test stat. # Mornans I > test = positive spatial autocorrealtion, Morans I < test = negative spatial autocorrelation
morans_expected <- -1/(dim(hd)[1]-1)

## Using distance matrix approach 
# creating distance matrix 
longlat_hd <- cbind(long = hd$longitude, lat = hd$latitude) %>% as.data.frame()
dist_matrix <- as.matrix(dist(longlat_hd))

dist_matrix <- (as.matrix(distm(longlat_hd, fun = distHaversine))/1000)
inv_dist_matrix <- 1/dist_matrix
diag(inv_dist_matrix) <- 0


max(inv_dist_matrix)
sum(is.infinite(inv_dist_matrix)) # no duplicate points. 

# standardise rows and columns. 
row_sum <- rowSums(inv_dist_matrix)
rnormalised <- t(t(inv_dist_matrix) / row_sum)

inv_dist_matrix <- prop.table(rnormalised, margin = 1)

# Morans I for distance. 
price_morans <- Moran.I(hd$price, inv_dist_matrix)
price_morans


#### model_data dist matrix 
longlat_model <- cbind(long = model_data$longitude, lat = model_data$latitude) %>% as.data.frame()
model_df_dist <- (as.matrix(distm(longlat_hd, fun = distHaversine))/1000)
model_df_inv_dist <- 1/model_df_dist
diag(model_df_inv_dist) <- 0
row_sum <- rowSums(model_df_inv_dist)
rnormalised <- t(t(model_df_inv_dist) / row_sum)
model_df_inv_dist <- prop.table(rnormalised, margin = 1)

#### model_data neighbor matrix 
model_knn <- knearneigh(longlat_model, k=100, longlat = TRUE, use_kd_tree=FALSE)
model_nb_list <- knn2nb(model_knn)
model_nb_listw <- nb2listw(model_nb_list)
model_nb_weights <- nb2listw(model_nb_list, style="W")

plot(st_geometry(limerick_county_border), border="grey")
plot(knn2nb(model_knn), longlat_model, add=TRUE)

#####################################################################################################################################
## Simple Linear Regressions 
#####################################################################################################################################

### beds_c
#Linear model 
bed_linear_model <- lm(price_log ~ beds_c, data = model_data)
summary(bed_linear_model)
(exp(bed_linear_model$coefficients[1])) # price of a 3 bed house is 155k
(exp(bed_linear_model$coefficients[2]) -1 )*100 # for every additional bedroom, 31% av increase in property price 

plot(bed_linear_model)
my_my_cooksd_calc_plot(bed_linear_model) # Nothing>0.5 no influential points


# beds_c - Polynomial model 
bed_polynomial_model <- lm(price_log~ poly(beds_c, 2, raw = T), data = model_data)
summary(bed_polynomial_model)
vif(bed_polynomial_model)

(exp(bed_polynomial_model$coefficients[1])) # price of a 3 bed house is 160k
(exp(bed_polynomial_model$coefficients[2] + 2*(bed_polynomial_model$coefficients[3])*(0)-bed_polynomial_model$coefficients[3])-1)*100 # increase from 3 to 4 bedrooms has 43.1% av increase 
(exp(bed_polynomial_model$coefficients[2] + 2*(bed_polynomial_model$coefficients[3])*(-2)-bed_polynomial_model$coefficients[3])-1)*100 # increase from 1 to 2 bedrooms has 74% av increase 
(exp(bed_polynomial_model$coefficients[2] + 2*(bed_polynomial_model$coefficients[3])*(3)-bed_polynomial_model$coefficients[3])-1)*100 # increase from 6 to 7 bedrooms has 7% av increase 
my_my_cooksd_calc_plot(bed_polynomial_model)



# Cross Validation for polynomial fit 
var <- mse <- numeric(6)
for (p in 1:6){
  m <- lm(price_log ~ poly(beds_c, p, raw = T), data=model_data)
  mse[p] <- cv::mse(model_data$price_log, fitted(m))
  var[p] <- summary(m)$sigma^2
}

plot(c(1, 6), range(mse, var), type="n",
     xlab="Degree of polynomial, p",
     ylab="Estimated Squared Error")
lines(1:6, mse, lwd=2, lty=1, col=2, pch=16, type="b")
lines(1:6, var, lwd=2, lty=2, col=3, pch=17, type="b")
legend("topright", inset=0.02,
       legend=c(expression(hat(sigma)^2), "MSE"),
       lwd=2, lty=2:1, col=3:2, pch=17:16)


dfbed <- data.frame(degree = 1:6, mse, var)

bed_polynomial_degree <- ggplot(dfbed, aes(x = degree)) + ggtitle("MSE Score for Beds Polynomial Fit") + 
  geom_line(aes(y = mse, color = "MSE"), linetype = "solid", size = 1) +
  geom_point(aes(y = mse, color = "MSE"), shape = 16, size = 4) +
  geom_line(aes(y = var, color = "Variance"), linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = var, color = "Variance"), shape = 17, size = 4) +
  labs(x = "Degree of polynomial, p", y = "Estimated Squared Error") +
  scale_color_manual(name = "Legend", values = c("MSE" = "steelblue", "Variance" = "#CD534C"),
                     labels = c(expression(hat(sigma)^2), "MSE")) +
  theme_cowplot(font_size = 14) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x)) + 
  theme(legend.position = c(0.87, 0.87), plot.title = element_text(hjust = 0.5))

# plotting the different polynomial degrees. 
plot(price_log ~ beds_c, data=model_data, xlab = "Beds", ylab = "log(Price)", main = "Beds and log(Price): Polynomial Fits")
beds_c <- with(model_data, 
                   seq(min(beds_c), max(beds_c), 
                       length=1000))
colvec <- c('grey','blue','lightblue')
i = 1
for (p in c(1,2,6)){
  m <- lm(price_log ~ poly(beds_c,p, raw = T), data=model_data)
  price_log <- predict(m, newdata=data.frame(beds_c=beds_c))
  lines(beds_c, price_log, col=colvec[i], lty=p, lwd=2)
  i = i+1
}
legend("topleft", legend=c(1,2,6), col=colvec, lty=3, lwd=2,
       title="Degree", inset=0.02)
# comparing 2 and 6, minor reduction in RMSE at the risk of overfitting. power of 2 is more parsimonious. 
# additionally poly6 is insignificant. 


poly_df <- data.frame(beds_c = rep(beds_c, 3),
                      price_log = c(predict(lm(price_log ~ poly(beds_c,1, raw = T), data=model_data), newdata=data.frame(beds_c=beds_c)),
                                    predict(lm(price_log ~ poly(beds_c,2, raw = T), data=model_data), newdata=data.frame(beds_c=beds_c)),
                                    predict(lm(price_log ~ poly(beds_c,6, raw = T), data=model_data), newdata=data.frame(beds_c=beds_c))),
                      degree = factor(rep(c(1,2,6), each = length(beds_c))))

# Plot using ggplot2
bed_polynomial_plot <- ggplot() +
  geom_point(data = model_data, aes(x = beds_c, y = price_log), color = "black") +
  geom_line(data = poly_df, aes(x = beds_c, y = price_log, color = degree, linetype = degree), size = 1) +
  labs(x = "Beds", y = "log(Price)", title = "Beds and log(Price): Polynomial Fits") +
  scale_color_manual(values = c('grey', 'blue', 'lightblue'), name = "Degree") +
  scale_linetype_discrete(name = "Degree") +
  theme_cowplot(font_size = 14) +
  theme(legend.position = c(0.87, 0.25), plot.title = element_text(hjust = 0.5))

### baths_c
# linear Model 
bath_linear_model <- lm(price_log ~ baths_c, data = model_data)
summary(bath_linear_model)
(exp(bath_linear_model$coefficients[1])) # price of a 2 bath house is 165k
(exp(bath_linear_model$coefficients[2]) -1 )*100 # for every additional bathroom, 33% av increase in property price 

plot(bath_linear_model)
my_cooksd_calc_plot(bath_linear_model)

# polynomial model
bath_polynomial_model <- lm(price_log ~ poly(baths_c, 3, raw = T), data = model_data)
summary(bath_polynomial_model)
plot(bath_polynomial_model)
(exp(bath_polynomial_model$coefficients[1])) # price of a 2 bed house is 177k
my_cooksd_calc_plot(bath_polynomial_model)


# Cross Validation for polynomial fit 
var <- mse <- numeric(7)
for (p in 1:7){
  m <- lm(price_log ~ poly(baths_c, p, raw = T), data=model_data)
  mse[p] <- mse(model_data$price_log, fitted(m))
  var[p] <- summary(m)$sigma^2
}

plot(c(1, 7), range(mse, var), type="n",
     xlab="Degree of polynomial, p",
     ylab="Estimated Squared Error")
lines(1:7, mse, lwd=2, lty=1, col=2, pch=16, type="b")
lines(1:7, var, lwd=2, lty=2, col=3, pch=17, type="b")
legend("topright", inset=0.02,
       legend=c(expression(hat(sigma)^2), "MSE"),
       lwd=2, lty=2:1, col=3:2, pch=17:16)

dfbath <- data.frame(degree = 1:7, mse, var)
bath_polynomial_degree <- ggplot(dfbath, aes(x = degree)) + ggtitle("MSE Score for Baths Polynomial Fit") + 
  geom_line(aes(y = mse, color = "MSE"), linetype = "solid", size = 1) +
  geom_point(aes(y = mse, color = "MSE"), shape = 16, size = 4) +
  geom_line(aes(y = var, color = "Variance"), linetype = "dashed", size = 1) +
  geom_point(aes(y = var, color = "Variance"), shape = 17, size = 4) +
  labs(x = "Degree of polynomial, p", y = "Estimated Squared Error") +
  scale_color_manual(name = "Legend", values = c("MSE" = "steelblue", "Variance" = "#CD534C"),
                     labels = c(expression(hat(sigma)^2), "MSE")) +
  theme_cowplot(font_size = 14) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x)) + 
  theme(legend.position = c(0.87, 0.87), plot.title = element_text(hjust = 0.5))

# plotting the different polynomial degrees. 
plot(price_log ~ baths_c, data=model_data)
baths_c <- with(model_data, 
               seq(min(baths_c), max(baths_c), 
                   length=1000))
colvec <- c("grey", "lightblue", "blue")
for (p in c(1,2,3)){
  m <- lm(price_log ~ poly(baths_c, p, raw = T), data=model_data)
  price_log <- predict(m, newdata=data.frame(baths_c=baths_c))
  lines(baths_c, price_log, col=colvec[p], lty=p, lwd=2)
}
legend("topleft", legend=c(1,2,3), col=colvec, lty=3, lwd=2,
       title="Degree", inset=0.02)
# 7 is overfitting, 3 seems to be rising at the 7th bathroom, but it is an improvement compared to the linear model. Similary, the 2nd degree does not fit as well as the linear. 
# all insignificant after 3 save for 7.

# Compute polynomial fits for baths_c
poly_df_baths <- data.frame(baths_c = rep(baths_c, 4),
                            price_log = c(predict(lm(price_log ~ poly(baths_c,1, raw = T), data=model_data), newdata=data.frame(baths_c=baths_c)),
                                          predict(lm(price_log ~ poly(baths_c,2, raw = T), data=model_data), newdata=data.frame(baths_c=baths_c)),
                                          predict(lm(price_log ~ poly(baths_c,3, raw = T), data=model_data), newdata=data.frame(baths_c=baths_c)),
                                          predict(lm(price_log ~ poly(baths_c,7, raw = T), data=model_data), newdata=data.frame(baths_c=baths_c))),
                            degree = factor(rep(c(1,2,3, 7), each = length(baths_c))))

# Plot for baths_c
bath_polynomial_plot <- ggplot() +
  geom_point(data = model_data, aes(x = baths_c, y = price_log), color = "black") +
  geom_line(data = poly_df_baths, aes(x = baths_c, y = price_log, color = degree, linetype = degree), size = 1) +
  labs(x = "Baths", y = "log(Price)", title = "Baths and log(Price): Polynomial Fits") +
  scale_color_manual(values = c('grey', 'lightblue', 'blue', 'steelblue'), name = "Degree") +
  scale_linetype_discrete(name = "Degree") +
  theme_cowplot(font_size = 14) +
  theme(legend.position = c(0.87, 0.25), plot.title = element_text(hjust = 0.5))



combined_plots <- plot_grid(bath_polynomial_plot, bath_polynomial_degree, bed_polynomial_plot, bed_polynomial_degree, nrow = 2, align = "hv", labels = c("A", "B", "C", "D"), label_size = 14)
combined_plots
ggsave(filename = 'bed_bath_polynomial.png', path = './Figures', width = 32, height = 30, units = 'cm')










## cbd_distance
# Linear 
cbd_linear_model <- lm(price_log ~ cbd_distance, data = model_data)
summary(cbd_linear_model)
(exp(cbd_linear_model$coefficients) -1 )*100 #every km further from city centre, 1% av decrease in price 
plot(cbd_linear_model)
my_cooksd_calc_plot(cbd_linear_model)

cbd_polynomial_model <- lm(price_log ~ poly(cbd_distance, 2, raw = T), data = model_data)
summary(cbd_polynomial_model)
plot(cbd_polynomial_model)
# predicted change in price when moving IQR of cbd_dist
quart3 <- 12.13106532-0.00032687*19.52158-0.00022026*(19.52158^2)
quart1 <- 12.13106532-0.00032687*2.15119-0.00022026*(2.15119^2)
exp(quart3)-exp(quart1)

plot(model_data$cbd_distance, model_data$price_log, main = "Scatter Plot with Regression Line", xlab = "CBD Distance (Km)", ylab = "Log Price")
lines(sort(model_data$cbd_distance),                 # Draw polynomial regression curve
      fitted(cbd_linear_model)[order(model_data$cbd_distance)],
      col = "green",
      type = "l")
lines(sort(model_data$cbd_distance),                 # Draw polynomial regression curve
      fitted(cbd_polynomial_model)[order(model_data$cbd_distance)],
      col = "red",
      type = "l")
# Minor improvement with cbd polynomial, and no apparent quadratic relationship. 



#####################################################################################################################################
## 1 - Multiple Linear Regressions - No Location 
#####################################################################################################################################

# show heteroscedasticity of residuals 
price_model <- lm(price ~ poly(beds_c, 2, raw =T) + poly(baths_c, 3, raw = T) + property_type.f + property_condition.f + period + garage + cul_de_sac + fireplace + period + month_year.f , data = model_data)
plot(price_model)

price_model_df <- 
residual_price_plot <- ggplot(data = as.data.frame(cbind(fitted = price_model$fitted.values, residuals = price_model$residuals)), aes(x =fitted, y = residuals)) +
  geom_point() + theme_cowplot(font_size = 14) + theme(plot.title = element_text(hjust = 0.5))  + 
  scale_x_continuous(breaks = c(200000,400000,600000),labels = c('200', '400', '600')) + 
  scale_y_continuous(breaks = c(-250000,0,250000, 500000, 750000),labels = c('-250', '0', '250', '500', '750')) + 
  labs(x = "Fitted Values (thousands €)", y = "Residual Values (thousands €)", title = "Residual Plot: Price OLS") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
residual_price_plot

# Residual plot for log(price) OLS
residual_logprice_plot <- ggplot(data = as.data.frame(cbind(fitted = ols_model$fitted.values, residuals = ols_model$residuals)), aes(x = fitted, y = residuals)) +
  geom_point() + theme_cowplot(font_size = 14) +theme(plot.title = element_text(hjust = 0.5))  + 
  labs(x = "Fitted Values", y = "Residual Values", title = "Residual Plot: log(Price) OLS") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
residual_log_transform <- plot_grid(residual_price_plot, residual_logprice_plot)
residual_log_transform
ggsave(filename = 'residual_log_transform.png', path = './Figures', width = 30, height = 15, units = 'cm')





# check additive model for VIFS 
full_additive_ols_model <- lm(price_log ~ baths_c + beds_c + property_type.f + property_condition.f + period + garage + cul_de_sac + fireplace + period + month_year.f , data = model_data)
vif(full_additive_ols_model)
vif(full_additive_ols_model)
# none > 5. 

full_model <- lm(price_log ~ poly(beds_c, 2, raw =T) + poly(baths_c, 3, raw = T) + property_type.f + property_condition.f + period + garage + cul_de_sac + fireplace + period + month_year.f , data = model_data)

summary(lm(price_log ~ poly(beds_c,2,raw=T), data = model_data))
summary(lm(price_log ~ poly(baths_c,3,raw=T), data = model_data))
summary(lm(price_log ~ property_type.f, data = model_data))
summary(lm(price_log ~ property_condition.f, data = model_data))

# MLR with no location
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3), data = model_data)
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds + I(beds^2), data = model_data)
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds + I(beds^2) + month_year.f, data = model_data)
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds + I(beds^2) + month_year.f + property_condition.f, data = model_data)
# property condition has lowest RMSE compared to other structural elements 
# after adding property conditoin. 3rd order bath is insignificant. 
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + beds_c + I(beds_c^2) + month_year.f + property_condition.f, data = model_data)
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f, data = model_data)
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period, data = model_data)
ols_model <- lm(price_log ~ baths_c + I(baths_c^2) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac, data = model_data)
ols_formula <- price_log ~ baths_c + I(baths_c^2) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + garage
ols_model <- lm(ols_formula, data = model_data)
# firelpace is not significant .

summary(ols_model)
Anova(ols_model, type = "III")
vif(ols_model)
plot(ols_model)
my_cooksd_calc_plot(ols_model)
# interpretation 
my_loglinear_coeff(ols_model)


# plotting y = x 
scatterplot_ols_model <- ggplot(data.frame(actual_price = model_data$price, predicted_price = pred_price), aes(x = actual_price, y = predicted_price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Price (€)", y = "Predicted Price", title = "OLS Model: Actual vs Predicted Price") + 
  scale_x_continuous(breaks = c(0, 500000,1000000), labels =c("0", "500,000", "1,000,000")) + 
  scale_y_continuous(breaks = c(0, 200000, 400000, 600000), labels =c("0", "200,000", "400,000", "600,000")) + 
  theme_cowplot(font_size = 18) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
scatterplot_ols_model
# strongly underestimating the outliers - Mainly Ennis Road, North Circular, Castletroy

# # plotting the coefficients of property condition
# condition_coefficients <- rbind(ols_coeff[28:30,], c('Second Hand', 1, 1, 1, 1, 1, 1, 1)
# # Plot
# points_plot_condition <- ggplot(condition_coefficients, aes(x = label, y = expest)) +
#   geom_point(size = 3) + theme_cowplot(font_size = 18) +
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   geom_hline(yintercept = 1) + 
#   labs(x = "Categories", y = "Values", title = "Points Plot") + 
#   ylim(0, 1.1)
# points_plot_condition
# 
# type_coefficients <- data.frame(
#   labels = c("detached", "semi-detached", "terrace", "end of terrace", "townhouse", "duplex", "apartment"),
#   values = c(1, 0.9320751,0.7316184,0.8284110,0.8414782,0.7945003, 0.7512064))
# points_plot_type <- ggplot(type_coefficients, aes(x = labels, y = values)) +
#   geom_point(size = 3) + theme_cowplot(font_size = 18) +
#   theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 10, hjust = 1)) + 
#   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   labs(x = "Categories", y = "Values", title = "Points Plot") + 
#   ylim(0, 1.1)
# points_plot_type

prediction <- exp(ols_model$fitted.values)
actual <- model_data$price
residual_price <- actual-prediction
summary(residual_price)

##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, residuals = ols_model$residuals) 

# map of property residuals county 
map1 <-  map_urban_area_labels +
  geom_sf(data = map.resids_sf, aes(color = residuals), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.5)) + theme(plot.margin = margin(0,0,0,0, 'cm'))
map1

# map of residuals in city 
map2 <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = residuals), size =1) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))  + 
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") + 
  geom_segment(aes(x = -8.63, y = 52.707-0.003, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.003, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.595, y = 52.698-0.003, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") + theme(plot.margin = margin(0,0,0,0, 'cm'))
map2

# title for grid property prices map \
title <- ggdraw() + 
  draw_label(
    'Basic OLS: Residual Map',
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) + theme(plot.margin = margin(0,0,0,0))
# legend for grid 
legend_price <- get_legend(map2) 
# grid of proeprty prices 
basic_ols_residual_map <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.4, 1, 0.1)),
  plot_grid(map1 + theme(legend.position = 'none'), map2 + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2, vjust = 4),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(0.05,1, 0.1),
  nrow = 3 
)
basic_ols_residual_map
ggsave(filename = 'basic_ols_residual_map.png', path = './Figures', width = 30, height = 15, units = 'cm')









# binary residuals 
binary_residuals <- data.frame(binary_residuals = ifelse(ols_model$residuals >= 0, 1, 0))
map.resids_sf <- cbind(map.resids_sf, binary_residuals)

# Plot Binary Residuals 
ols_residuals_binary_plot <- map_urban_area_labels + 
  ggtitle("ols Binary Residuals") + 
  geom_sf(data = map.resids_sf, aes(color = binary_residuals), size = 1/2) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
ols_residuals_binary_plot
# Properties underestimated in Adare Rathkeale, and Surrounding City. Overestimated in the inner City (North), clusters of underestimated properties in NCW and botder properties. 

# Plot Binary Residuals in City 
ols_residuals_binary_city_plot <- map_city +
  ggtitle("ols Model Reisuals - Limerick City (Binary)") + 
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = binary_residuals), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
ols_residuals_binary_city_plot
# overestimate in the south inner city, underestimating homes in housing surrounding areas of dooradoyle and castletroy. 


# Mapping the residuals outside the IQR. 
map.resids_sf <- map.resids_sf %>%
  mutate(cores = case_when(
    ols_model.residuals > 0.21307 ~ 1,
    ols_model.residuals < -0.21087 ~ 2,
    TRUE ~ 3
  ))

# Plotting
ols_residuals_IQR_plot <- map_city + 
  ggtitle("ols Model Residuals - Limerick City (Binary)") + 
  geom_sf(data = map.resids_sf[map.resids_sf$municipality == 'Limerick City and Suburbs',], 
          aes(color = as.factor(cores)), size = 1) +
  scale_color_manual(name = "Residual Value",
                     values = c("2" = "red", "3" = "grey", "1" = "green")) +
  theme_map()
ols_residuals_IQR_plot
# there are many properties underestimated in the regions of North and Castletroy. Overestimated in the city centre. 

# Percentage Error
pred <- exp(ols_model$fitted.values)
actual <- (model_data$price)
error_df <- cbind(pred, actual) %>% as.data.frame()
error_df <- error_df %>% mutate(percentage_error = (actual-pred)/actual,abs_percentage_error = abs(percentage_error))
summary(error_df$percentage_error)*100
error_sf <- cbind(model_data_sf, error_df) 

# Plotting Percentage Error
ols_percent_error_plot <- map_urban_area_labels +
  geom_sf(data = error_sf, aes(color = percentage_error*100), size = 1/2) +
  scale_color_gradient(name = "Percentage Error", low = "red", high = "green") +
  theme_map()
ols_percent_error_plot
# ols_model overestimated values in the inner city, and city north, We see some outlers on the Ennis road and Castletroy. Complies with price_log graph. 


##########################
# Model Metrics   
# Morans I

ols_moransI_residuals <- Moran.I(ols_model$residuals, model_df_inv_dist)
# training data metrics
my_model_metrics(ols_model, model_data)
# 10 fold CV
ols_metrics <- rbind(my_cross_validation(ols_formula, fold_list = fold_list), "Moran's I" = round(ols_moransI_residuals$observed,3)) %>% rename('Basic OLS' = 'colMeans(metrics_list_df)')
ols_model_results <- ols_metrics



#####################################################################################################################################
## 2 - Multiple Linear Regressions - Municipality Dummy 
#####################################################################################################################################


olsm_model <- lm(price_log ~ baths_c + I(baths_c^2) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + municipality.f + garage, data = model_data)
olsm_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + municipality.f + garage, data = model_data)
olsm_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + municipality.f + garage + fireplace, data = model_data)
olsm_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + municipality.f + fireplace, data = model_data)
olsm_formula <- price_log ~ baths_c + I(baths_c^2) +  I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + municipality.f + fireplace
olsm_model <- lm(olsm_formula, data = model_data)

# aliased coefficients when interacting beds_c, baths_c with property_type/property_condition/municipality, municipality with property type 
# more significant with bed and bath polynomial (2)
# removing cul_de_sac then garage
# 3rd degree bath polynomial does not reduce MSE 
# year has reduced RMSE compared to the month_year.f
summary(olsm_model)
Anova(olsm_model, type = "III")
vif(olsm_model)
plot(olsm_model)
olsm_coeff <- my_loglinear_coeff(olsm_model)
# coefficients for Limerick county align with the median prices


##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, residuals = olsm_model$residuals) 
olsm_residuals_plot <- map_urban_area_labels + ggtitle("OLSM Residuals Limerick") +
  geom_sf(data = map.resids_sf, aes(color = residuals), size = 1.3) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
olsm_residuals_plot
# culsters in Adare, Newcastle west, east, and the city - No apparent difference from OLS Model 

# Plotting Residuals in Limerick City 
olsm_residuals_city_plot <- map_city + ggtitle("OLSM Residuals Limerick City") +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = residuals), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
olsm_residuals_city_plot
# olsm_model overestimated values in the inner city, and city north, We see some outlers on the Ennis road and Castletroy. Complies with price_log graph. 

# binary residuals 
binary_residuals <- data.frame(binary_residuals = ifelse(map.resids_sf$residuals >= 0, 1, 0))
map.binary_resids_df <- cbind(model_data_sf, binary_residuals)

# Plot Binary Residuals 
olsm_residuals_binary_plot <- map_urban_area + ggtitle("OLSM Binary Residuals") + 
  geom_sf(data = map.binary_resids_df, aes(color = binary_residuals), size = 1.3) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
olsm_residuals_binary_plot


##########################
# Model Metrics  
# Moran's I
olsm_moransI_residuals <- Moran.I(olsm_model$residuals, model_df_inv_dist)
# training data metrics
olsm_training_metrics <- my_model_metrics(olsm_model, model_data)
# 10 fold CV
olsm_metrics <- rbind(my_cross_validation(olsm_formula, fold_list), "Moran's I" = round(olsm_moransI_residuals$observed,3)) %>% rename('OLS-m' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, olsm_metrics)



#####################################################################################################################################
## 3 - Multiple Linear Regressions - Urban Area Dummy 
#####################################################################################################################################


# MLR with urban_area dummy 
olsu_model <- lm(price_log~ beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths^3) + urban_area.f + property_condition.f + property_type.f + period + fireplace + cul_de_sac + month_year.f, data = model_data)
# remove garage 
olsu_model <- lm(price_log~ beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths^3) + urban_area.f + property_condition.f + property_type.f + cul_de_sac + period + fireplace + month_year.f, data = model_data)
# check for interactions 
olsu_formula <- price_log~ beds_c + I(beds_c^2) + baths_c + I(baths_c^2) +  I(baths^3) + urban_area.f + property_condition.f + property_type.f + period + fireplace  + month_year.f + urban_area.f*fireplace + urban_area.f*baths_c
# aliased coefficients when interacting beds_c, baths_c with property_type/property_condition/municipality, municipality with property type 
# more significant with bed and bath polynomial (2)
# fireplace and urban_area have a significant interaction but increases VIF. However we see little differences in SE. 
# removing cul_de_sac then garage
# no significant interaction (cul_de_sac, urban_area) (garage, urban_area)


olsu_model <- lm(olsu_formula, data = model_data)


summary(olsu_model)
Anova(olsu_model, typex = "III")
vif(olsu_model)
plot(olsu_model)
olsu_coeff <- my_loglinear_coeff(olsu_model)


##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, residuals = olsu_model$residuals) 
# map of property residuals county 
map1 <-  map_urban_area_labels +
  geom_sf(data = map.resids_sf, aes(color = residuals), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.5)) + theme(plot.margin = margin(0,0,0,0, 'cm'))
map1

# map of residuals in city 
map2 <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = residuals), size =1) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))  + 
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") + 
  geom_segment(aes(x = -8.63, y = 52.707-0.003, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.003, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.59, y = 52.698-0.003, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") + theme(plot.margin = margin(0,0,0,0, 'cm'))
map2

# title for grid property prices map \
title <- ggdraw() + 
  draw_label(
    'OLS-u: Residual Map',
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) + theme(plot.margin = margin(0,0,0,0))
# legend for grid 
legend_price <- get_legend(map2) 
# grid of proeprty prices 
olsu_residual_map <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.4, 1, 0.1)),
  plot_grid(map1 + theme(legend.position = 'none'), map2 + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2, vjust = 4),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(0.05,1, 0.1),
  nrow = 3 
)
olsu_residual_map
ggsave(filename = 'olsu_residual_map.png', path = './Figures', width = 30, height = 15, units = 'cm')


# culsters in Adare, Newcastle west, east, and the city. 
# Model overestimated values in the inner city, and city north, We see some outlers on the Ennis road and Castletroy. Complies with price_log graph. 


##########################
# Model Metrics  
# Morans I
olsu_moransI_residuals <- Moran.I(olsu_model$residuals, model_df_inv_dist)
# training data metrics
olsu_training_metrics <- my_model_metrics(olsu_model, model_data)
#10 fold CV
olsu_metrics <- rbind(my_cross_validation(olsu_formula, fold_list), "Moran's I" = round(olsu_moransI_residuals$observed,3)) %>% rename('OLS-u' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, olsu_metrics)



#####################################################################################################################################
## 4 - Multiple Linear Regressions - CBD Distance 
#####################################################################################################################################


# MLR with cbd_distance
ols_cbd_model <- lm(price_log~ beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + cbd_distance + property_condition.f + property_type.f + period + fireplace + garage + cul_de_sac + month_year.f, data = model_data)
ols_cbd_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + garage + fireplace + cbd_distance, data = model_data)
ols_cbd_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + fireplace + cbd_distance + period*cbd_distance +fireplace*cbd_distance, data = model_data)

summary(ols_cbd_model)
Anova(ols_cbd_model, type = "III")
my_cooksd_calc_plot(ols_cbd_model)
vif(ols_cbd_model)
plot(ols_cbd_model)



ols_cbd_coeff <- my_loglinear_coeff(ols_cbd_model)
# expected inverse relationship between cbd_dist and price, 


##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, resids = ols_cbd_model$residuals) 
ols_cbd_residuals_plot <- map_urban_area_labels + ggtitle("OLS CBD Residuals") +
  geom_sf(data = map.resids_sf, aes(color = resids), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") 
ols_cbd_residuals_plot
# Plotting Residuals in Limerick City 
ols_cbd_residuals_city_plot <- map_city +ggtitle("OLS CBD City Residuals") +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = resids), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") 
ols_cbd_residuals_city_plot
# outliers still apparent, cluster in the inner. reduction in magnitude of residuals in inner city and outer county, accounted for my distance to cirty. 



##########################
# Model Metrics   
# Morans I

ols_cbd_moransI_residuals <- Moran.I(ols_cbd_model$residuals, model_df_inv_dist)
# training data metrics
ols_cbd_training_metrics <- my_model_metrics(ols_cbd_model, model_data)
# 5 fold CV 
ols_cbd_metrics <- rbind(my_cross_validation(ols_cbd_formula, fold_list), "Moran's I" = round(ols_cbd_moransI_residuals$observed,3)) %>% rename('OLS-cbd' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, ols_cbd_metrics)


#####################################################################################################################################
## 4 - Multiple Linear Regressions - CBD Distance + Urban Area
#####################################################################################################################################

olsu_cbd_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + garage + fireplace + cbd_distance + urban_area.f, data = model_data)
# grarge is not significant 
olsu_cbd_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + fireplace + cbd_distance + urban_area.f, data = model_data)
olsu_cbd_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + fireplace + cbd_distance + urban_area.f + fireplace*urban_area.f , data = model_data)
# reduced RMSE when including interaction between fireplace nad urban+area, and removing insignificant cul_de_sac 
olsu_cbd_formula <- price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + fireplace + cbd_distance + urban_area.f + fireplace*urban_area.f
olsu_cbd_formula <- price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period.f + fireplace.f + cbd_distance + urban_area.f + fireplace.f*urban_area.f + beds_c*urban_area.f 
olsu_cbd_model <- lm(olsu_cbd_formula, data = model_data)



summary(olsu_cbd_model2)


# significant interaction between fireplace and urban area, all other urban_area interactions are not important. 
# significant interaction between bathrooms and urban area - then aliased 
# bed^2 not significant 
summary(olsu_cbd_model)
Anova(olsu_cbd_model, type = "III")
vif(olsu_cbd_model)
plot(olsu_cbd_model)
my_loglinear_coeff(olsu_cbd_model)

# coefficients - property type 
grid <- emmeans(olsu_cbd_model, ~ property_type.f, nuisance = c("month_year", "baths_c", "property_condition.f" ))
coeff <- summary(contrast(grid, method= 'eff'))
type_coeff_df <- data.frame(contrast = coeff$contrast, coeff = exp(coeff$estimate), lower = exp(coeff$estimate - 1.96*coeff$SE), upper = exp(coeff$estimate + 1.96*coeff$SE)) %>% arrange(coeff)
type_coeff_df$contrast <- c("Apartment", "Duplex", "Terraced", "End-of-Terrace", "Townhouse", "Semi-Detached", "Detached")

scalings_prop_type <- ggplot(type_coeff_df, aes(reorder(contrast, coeff), coeff)) + 
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper))  + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot() +theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
  labs(title = "Relative Scalings of Property Types", x = "Property Type", y = "Multiplicative Scaling") 



# coefficients - property condition 
grid <- emmeans(olsu_cbd_model, ~ property_condition.f, nuisance = c("month_year", "baths_c", "property_type.f" ))
coeff <- summary(contrast(grid, method= 'eff'))
condition_coeff_df <- data.frame(contrast = coeff$contrast, coeff = exp(coeff$estimate), lower = exp(coeff$estimate - 1.96*coeff$SE), upper = exp(coeff$estimate + 1.96*coeff$SE)) %>% arrange(coeff)
condition_coeff_df$contrast <- gsub(" effect", "", condition_coeff_df$contrast)

scalings_prop_cond <- ggplot(condition_coeff_df, aes(reorder(contrast, coeff), coeff)) + 
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper))  + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot() +theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
  labs(title = "Relative Scalings of Property Condition", x = "Property Condition", y = "Multiplicative Scaling") 
plot_grid(scalings_prop_type, NULL,  scalings_prop_cond, nrow = 3, rel_heights = c(1, 0.2, 1))


# coefficients -urban area 
grid <- emmeans(olsu_cbd_model, ~ urban_area.f, nuisance = c("month_year", "baths_c", "property_type.f" ))
coeff <- summary(contrast(grid, method= 'eff'))
area_coeff_df <- data.frame(contrast = coeff$contrast, coeff = exp(coeff$estimate), lower = exp(coeff$estimate - 1.96*coeff$SE), upper = exp(coeff$estimate + 1.96*coeff$SE)) %>% arrange(coeff)
area_coeff_df$contrast <- gsub(" effect", "", area_coeff_df$contrast)


# coefficients - urban area:fireplace
grid <- emmeans(olsu_cbd_model, ~ urban_area.f:fireplace.f, nuisance = c("month_year", "baths_c", "property_condition.f" ))
grid
coeff <- summary(contrast(grid, method= 'eff'))
coeff_df <- data.frame(contrast = coeff$contrast, coeff = exp(coeff$estimate), lower = exp(coeff$estimate - 1.96*coeff$SE), upper = exp(coeff$estimate + 1.96*coeff$SE)) %>% arrange(coeff)
coeff <- my_loglinear_coeff(olsu_cbd_model)
coeff <- coeff %>% filter(grepl("fireplace",label ))
coeff_df <- data.frame(coeffient = coeff$label, coeff = coeff$expest, lower = coeff$explower, upper = coeff$expupperper) %>% arrange(coeff)
ggplot(coeff_df, aes(reorder(contrast, coeff), coeff)) + 
  # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))  + 
  geom_abline(intercept = 1, slope = 0, color = "grey") +
  theme_cowplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# coefficient table - all 
term_coeff <- my_loglinear_coeff(olsu_cbd_model)[c(1:6, 38:40), 1:4]
colnames(term_coeff) <- c('contrast','coeff', 'lower','upper')

coeff_df <- bind_rows(term_coeff, area_coeff_df, condition_coeff_df, type_coeff_df)
coeff_df <- coeff_df %>%
  mutate(
    lower = paste("[", round(lower, 2), sep = ""),
    upper = paste(round(upper, 2), "]", sep = ""),
    confidence_interval = paste(lower, upper, sep = ", "),
    coeff = round(coeff, 2))
coeff_df <- select(coeff_df, -c(lower, upper))

colnames(coeff_df) <- c('Variable','Estimate', '95% Condifidence Interval')
kbl(coeff_df, booktabs = T, 'latex')




# bath polynomial 
from1to2 <-  c(my_third_degree_poly(0.156, -0.085, 0.004, -1), my_third_degree_poly(0.179956, -0.060734, 0.011306, -1), my_third_degree_poly(0.204, -0.037, 0.019, -1))
from2to3 <- c(my_third_degree_poly(0.156, -0.085, 0.011306, 0), my_third_degree_poly(0.179956, -0.060734, 0.011306, 0),my_third_degree_poly(0.204, -0.037, 0.019, 0))
from4to5 <-  c(my_third_degree_poly(0.156, -0.085, 0.004, 2), my_third_degree_poly(0.179956, -0.060734, 0.011306, 2), my_third_degree_poly(0.204, -0.037, 0.019, 2))
from6to7 <-  c(my_third_degree_poly(0.156, -0.085, 0.004, 4), my_third_degree_poly(0.179956, -0.060734, 0.011306, 4), my_third_degree_poly(0.204, -0.037, 0.019, 4))

# bed and urban area interaction 
from1to2CitySouth <- c(my_bed_urban_area(0.095, -0.039, -0.177,-2 ), my_bed_urban_area(0.124, -0.024, -0.117,-2 ), my_bed_urban_area(0.153, -0.009, 0.057,-2 ))
from3to4CitySouth <- c(my_bed_urban_area(0.095, -0.039, -0.177,0 ), my_bed_urban_area(0.124, -0.024, -0.117,0 ), my_bed_urban_area(0.153, -0.009, 0.057,0 ))
from3to4OuterSouth <- c(my_bed_urban_area(0.095, -0.039, 0.005,0 ), my_bed_urban_area(0.124, -0.024, 0.069,0 ), my_bed_urban_area(0.153, -0.009, 0.134,0 ))
from3to4NewCastleWest <- c(my_bed_urban_area(0.095, -0.039, -0.006,0 ), my_bed_urban_area(0.124, -0.024, 0.048,0 ), my_bed_urban_area(0.153, -0.009, 0.101,0 ))

# fireplace and urban area coeff 
fp <- c( 0.017,    0.059,  0.100 )
fpd <-  c(-0.361,   -0.192, -0.023)
fpCityNorth <- c(exp(fp[1] + fpd[1]), exp(fp[2] + fpd[2]), exp(fp[3] + fpd[3]))
fpd <- c(-0.054,    0.010,  0.074)
fpNorth <- c(exp(fp[1] + fpd[1]), exp(fp[2] + fpd[2]), exp(fp[3] + fpd[3]))

##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, residuals = olsu_cbd_model$residuals) 

olsu_cbd_residuals_plot <-  map_urban_area_labels +
  geom_sf(data = map.resids_sf, aes(color = residuals), size = 1.5) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.3))
olsu_cbd_residuals_plot
olsu_cbd_residuals_city_plot <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = residuals), size = 1.3) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))  + 
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") + 
  geom_segment(aes(x = -8.63, y = 52.7-0.0026, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.0026, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.60, y = 52.695-0.0026, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") 
olsu_cbd_residuals_city_plot
title <- ggdraw() + 
  draw_label(
    "OLS with Distance and Uban Areas: Residual Values",
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 20
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )  
residual_legend <- get_legend(olsu_cbd_residuals_city_plot) 

olsu_cbd_residual_city_county_plot <- plot_grid(
  plot_grid(title, nrow = 1),
  plot_grid(olsu_cbd_residuals_plot + theme(legend.position = 'none'), olsu_cbd_residuals_city_plot + theme(legend.position = 'none'), nrow = 1,   
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2),
  plot_grid(NULL, residual_legend, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(1,15, 1),
  nrow = 3
)
olsu_cbd_residual_city_county_plot


# plotting y = x 
pred_price <- predict(olsu_cbd_model, newdata = model_data)
scatterplot_ols_cbd_u_model <- ggplot(data.frame(actual_price = model_data$price, predicted_price = exp(pred_price)), aes(x = actual_price, y = predicted_price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000), labels =c("0", "200", "400", "600", "800", "1")) + 
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1200000), labels =c("0", "200", "400", "600", "800", "1,000", "1,200")) + 
  labs(x = "Actual Price in Thousands (€)", y = "Predicted Price in Thousands (€)", title = "OLS-cbd-u Model: Actual vs Predicted Price") + 
  theme_cowplot(font_size = 18) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
scatterplot_ols_cbd_u_model


##########################
# Model Metrics  
# Morans I
olsu_cbd_moransI_residuals <- Moran.I(olsu_cbd_model$residuals, model_df_inv_dist)
# training data metrics
olsu_cbd_training_metrics <- my_model_metrics(olsu_cbd_model, model_data)
# 5 fold CV 

olsu_cbd_metrics <- rbind(olsu_cbd_metrics <- my_cross_validation(olsu_cbd_formula, fold_list), "Moran's I" = round(olsu_cbd_moransI_residuals$observed,3)) %>% rename('OLS-cbd-u' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, olsu_cbd_metrics)







#####################################################################################################################################
## 4a - Multiple Linear Regressions - CBD Range + Urban Area
#####################################################################################################################################

ols_cbdr_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + fireplace + cbd_range.f, data = model_data)
ols_cbdr_formula <- price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + fireplace + cbd_range.f
ols_cbdr_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + fireplace + cbd_range.f, data = model_data)
summary(ols_cbdr_model)
Anova(ols_cbdr_model, type = 'III')



olsu_cbdr_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + garage + fireplace + cbd_range.f + urban_area.f, data = model_data)
# grarge and cul_de_sac is not significant 
olsu_cbdr_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + cul_de_sac + fireplace + cbd_range.f + urban_area.f, data = model_data)
olsu_cbdr_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + fireplace + cbd_range.f + urban_area.f, data = model_data)
olsu_cbdr_model <- lm(price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + fireplace + cbd_range.f + urban_area.f + fireplace*urban_area.f , data = model_data)
# reduced RMSE when including interaction between fireplace nad urban+area, and removing insignificant cul_de_sac 
olsu_cbdr_formula <- price_log ~ baths_c + I(baths_c^2) + I(baths_c^3) + beds_c + I(beds_c^2) + month_year.f + property_condition.f + property_type.f + period + fireplace + cbd_range.f + urban_area.f  + beds_c*urban_area.f + fireplace*urban_area.f
olsu_cbdr_model <- lm(olsu_cbdr_formula, data = model_data)



summary(olsu_cbdr_model)


# significant interaction between fireplace and urban area, all other urban_area interactions are not important. 
# significant interaction between bathrooms and urban area - then aliased 
# bed^2 not significant 
Anova(olsu_cbdr_model, type = "III")
vif(olsu_cbdr_model)
plot(olsu_cbdr_model)
my_loglinear_coeff(olsu_cbdr_model)

# test the residual for normaity 
ks_test <- olsu_cbdr_model$residuals
ks.test(ks_test, "pnorm", mean=mean(ks_test), sd=sd(ks_test)) # normally distributed 
shapiro.test(ks_test)
qqnorm(ks_test):qqline(ks_test)

# coefficients - property type 
grid <- emmeans(olsu_cbdr_model, ~ property_type.f, nuisance = c("month_year", "baths_c", "property_condition.f", "cbd_range.f"))
coeff <- summary(contrast(grid, method= 'eff'))
type_coeff_df <- data.frame(contrast = coeff$contrast, estimate = coeff$estimate, lower = coeff$estimate - 1.96*coeff$SE, upper =coeff$estimate + 1.96*coeff$SE) %>% arrange(coeff)
type_coeff_df <- type_coeff_df %>% mutate(expest = exp(estimate), explower = exp(lower), expupper = exp(upper))
type_coeff_df$contrast <- c("Semi-Detached", "Apartment", "Detached", "Duplex", "End-of-Terrace", "Terraced", "Townhouse")

scalings_prop_type <- ggplot(type_coeff_df, aes(reorder(contrast, expest), expest)) + 
  geom_point() +
  geom_linerange(aes(ymin = explower, ymax = expupper))  + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot(font_size = 15) +theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
  labs(title = "Relative Scalings of Property Types", x = "Property Type", y = "Multiplicative Scaling") 
scalings_prop_type+ theme_cowplot(font_size = 18) +theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("poster_relative_scalings.png", path = '../../Poster/Figures', width = 30, height=13, dpi = 700, units = "cm")



# coefficients - property condition 
grid <- emmeans(olsu_cbdr_model, ~ property_condition.f, nuisance = c("month_year", "baths_c", "property_type.f", "cbd_range.f" ))
coeff <- summary(contrast(grid, method= 'eff'))
condition_coeff_df <- data.frame(contrast = coeff$contrast, estimate = coeff$estimate, lower = coeff$estimate - 1.96*coeff$SE, upper = coeff$estimate + 1.96*coeff$SE) %>% arrange(coeff)
condition_coeff_df <- condition_coeff_df %>% mutate(expest = exp(estimate), explower = exp(lower), expupper = exp(upper))
condition_coeff_df$contrast <- gsub(" effect", "", condition_coeff_df$contrast)
condition_coeff_df$contrast <- c("Derelict", "New", "Rennovation", "Second-Hand")

scalings_prop_cond <- ggplot(condition_coeff_df, aes(reorder(contrast, expest), expest)) + 
  geom_point() +
  geom_linerange(aes(ymin = explower, ymax = expupper))  + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot(font_size = 14) +theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
  labs(title = "Relative Scalings of Property Condition", x = "Property Condition", y = "Multiplicative Scaling") 


# coefficients - cbd_range
grid <- emmeans(olsu_cbdr_model, ~ cbd_range.f, nuisance = c("month_year", "baths_c", "property_type.f", "property_condition.f" ))
coeff <- summary(contrast(grid, method= 'eff'))
range_coeff_df <- data.frame(contrast = coeff$contrast, estimate = coeff$estimate, lower = coeff$estimate - 1.96*coeff$SE, upper = coeff$estimate + 1.96*coeff$SE) %>% arrange(coeff)
range_coeff_df <- range_coeff_df %>% mutate(expest = exp(estimate), explower = exp(lower), expupper = exp(upper))
range_coeff_df$contrast <- c("10km to 20km", "< 5km","5km to 10km", "> 20km")

scalings_range <- ggplot(range_coeff_df, aes(reorder(contrast, expest), expest)) + 
  geom_point() +
  geom_linerange(aes(ymin = explower, ymax = expupper))  + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot(font_size = 14) +theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Relative Scalings of CBD Distance Range", x = "Distance to CBD", y = "Multiplicative Scaling") 
scaling_grid <- plot_grid(scalings_prop_type, NULL,  scalings_prop_cond, NULL, scalings_range, nrow = 5, rel_heights = c(1, 0.2, 1, 0.2, 1),  labels = c("A", NA, "B", NA, "C"), hjust = -4)
scaling_grid
ggsave("relative_scalings.png", path = './Figures', width = 30, height=30,units = "cm")



# coefficients -urban area 
grid <- emmeans(olsu_cbdr_model, ~ urban_area.f, nuisance = c("month_year", "baths_c", "property_type.f", "cbd_range.f"))
coeff <- summary(contrast(grid, method= 'eff'))
area_coeff_df <- data.frame(contrast = coeff$contrast, estimate = coeff$estimate, lower = coeff$estimate - 1.96*coeff$SE, upper = coeff$estimate + 1.96*coeff$SE) %>% arrange(coeff)
area_coeff_df <- area_coeff_df %>% mutate(expest = exp(estimate), explower = exp(lower), expupper = exp(upper))
area_coeff_df$contrast <- gsub(" effect", "", area_coeff_df$contrast)



# coefficient table - all 
term_coeff <- my_loglinear_coeff(olsu_cbdr_model)[c(1:6, 38:39),c(1:7)] %>%
  rename(contrast = label)

interaction_coeff <- my_loglinear_coeff(olsu_cbdr_model)[c(54:75),c(1:7)] %>%
  rename(contrast = label)




coeff_df <- bind_rows(term_coeff, area_coeff_df, condition_coeff_df, type_coeff_df, range_coeff_df, interaction_coeff)
coeff_df <- coeff_df %>%
  mutate(
    lower = paste("[", round(lower, 2), sep = ""),
    upper = paste(round(upper, 2), "]", sep = ""),
    CI = paste(lower, upper, sep = ", "),
    estimate = round(estimate, 2),
    explower = paste("[", round(explower, 2), sep = ""),
    expupper = paste(round(expupper, 2), "]", sep = ""),
    e_CI = paste(explower, expupper, sep = ", "),
    expest = round(expest, 2))
coeff_df <- dplyr::select(coeff_df, c('contrast', 'estimate', 'CI', 'expest', 'e_CI'))

colnames(coeff_df) <- c('Variable','Estimate', '95% Condifidence Interval', 'LOG_Estimate', 'LOG_95% Condifidence Interval')
kbl(coeff_df, longtable = T, booktabs = T, 'latex') %>%
  kable_styling(latex_options = c("repeat_header"), 'latex')



term_coeff[4,]
interaction_coeff

# bath polynomial 
from1to2 <-  c(my_third_degree_poly(term_coeff[2,'lower'], term_coeff[3,'lower'], term_coeff[4,'lower'], -1), my_third_degree_poly(term_coeff[2,'estimate'], term_coeff[3,'estimate'], term_coeff[4,'estimate'], -1), my_third_degree_poly(term_coeff[2,'upper'], term_coeff[3,'upper'], term_coeff[4,'upper'], -1))
from2to3 <- c(my_third_degree_poly(term_coeff[2,'lower'], term_coeff[3,'lower'], term_coeff[4,'lower'], 0), my_third_degree_poly(term_coeff[2,'estimate'], term_coeff[3,'estimate'], term_coeff[4,'estimate'], 0),my_third_degree_poly(term_coeff[2,'upper'], term_coeff[3,'upper'], term_coeff[4,'upper'], 0))
from4to5 <-  c(my_third_degree_poly(term_coeff[2,'lower'], term_coeff[3,'lower'], term_coeff[4,'lower'], 2), my_third_degree_poly(term_coeff[2,'estimate'], term_coeff[3,'estimate'], term_coeff[4,'estimate'], 2), my_third_degree_poly(term_coeff[2,'upper'], term_coeff[3,'upper'], term_coeff[4,'upper'], 2))
from6to7 <-  c(my_third_degree_poly(term_coeff[2,'lower'], term_coeff[3,'lower'], term_coeff[4,'lower'], 4), my_third_degree_poly(term_coeff[2,'estimate'], term_coeff[3,'estimate'], term_coeff[4,'estimate'], 4), my_third_degree_poly(term_coeff[2,'upper'], term_coeff[3,'upper'], term_coeff[4,'upper'], 4))

term_coeff[6,'lower']
area_coeff_df[6,'lower']

interaction_coeff
interaction_coeff[7, 'lower']

# bed and urban area interaction 
# city south = 7, outer south = 1, nw = 10
from1to2CitySouth <- c(my_bed_urban_area(term_coeff[5,'lower'], term_coeff[6,'lower'], interaction_coeff[7,'lower'],-2 ), my_bed_urban_area(term_coeff[5,'estimate'], term_coeff[6,'estimate'], interaction_coeff[7,'estimate'],-2 ), my_bed_urban_area(term_coeff[5,'upper'], term_coeff[6,'upper'], interaction_coeff[7,'upper'],-2 ))
from3to4CitySouth <- c(my_bed_urban_area(term_coeff[5,'lower'], term_coeff[6,'lower'], interaction_coeff[7,'lower'],0 ), my_bed_urban_area(term_coeff[5,'estimate'], term_coeff[6,'estimate'], interaction_coeff[7,'estimate'],0 ), my_bed_urban_area(term_coeff[5,'upper'], term_coeff[6,'upper'], interaction_coeff[7,'upper'],0 ))
from3to4OuterSouth <- c(my_bed_urban_area(term_coeff[5,'lower'], term_coeff[6,'lower'], interaction_coeff[1, 'lower'],0 ), my_bed_urban_area(term_coeff[5,'estimate'], term_coeff[6,'estimate'], interaction_coeff[1, 'estimate'],0 ), my_bed_urban_area(term_coeff[5,'upper'], term_coeff[6,'upper'], interaction_coeff[1, 'upper'],0 ))
from3to4NewCastleWest <- c(my_bed_urban_area(term_coeff[5,'lower'], term_coeff[6,'lower'], interaction_coeff[10, 'lower'],0 ), my_bed_urban_area(term_coeff[5,'estimate'], term_coeff[6,'estimate'], interaction_coeff[10, 'estimate'],0 ), my_bed_urban_area(term_coeff[5,'upper'], term_coeff[6,'upper'], interaction_coeff[10, 'upper'],0 ))

# fireplace and urban area coeff 

fp <- c( term_coeff[8,'lower'],    term_coeff[8,'estimate'],  term_coeff[8,'upper'] )
fpd <-  c(interaction_coeff[17,'lower'],   interaction_coeff[17,'estimate'], interaction_coeff[17,'upper'])
fpCityNorth <- c(exp(fp[1] + fpd[1]), exp(fp[2] + fpd[2]), exp(fp[3] + fpd[3]))

interaction_coeff[22,]
fpd <- c(interaction_coeff[22,'lower'],    interaction_coeff[22,'estimate'], interaction_coeff[22,'upper'])
fpNorth <- c(exp(fp[1] + fpd[1]), exp(fp[2] + fpd[2]), exp(fp[3] + fpd[3]))

##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, residuals = olsu_cbdr_model$residuals) 
# map of property residuals county 
map1 <-  map_urban_area_labels +
  geom_sf(data = map.resids_sf, aes(color = residuals), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.5)) + theme(plot.margin = margin(0,0,0,0, 'cm'))
map1

# map of residuals in city 
map2 <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = residuals), size =1) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))  + 
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") + 
  geom_segment(aes(x = -8.63, y = 52.707-0.003, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.003, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.59, y = 52.698-0.003, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") + theme(plot.margin = margin(0,0,0,0, 'cm'))
map2

# title for grid property prices map \
title <- ggdraw() + 
  draw_label(
    'OLS-cbdr-u: Residual Map',
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) + theme(plot.margin = margin(0,0,0,0))
# legend for grid 
legend_price <- get_legend(map2) 
# grid of proeprty prices 
ols_cbdru_residual_map <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.4, 1, 0.1)),
  plot_grid(map1 + theme(legend.position = 'none'), map2 + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2, vjust = 4),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(0.05,1, 0.1),
  nrow = 3 
)
ols_cbdru_residual_map
ggsave(filename = 'ols_cbdr_u_residual_map.png', path = './Figures', width = 30, height = 15, units = 'cm')


# plotting residuals
ols_cbdr_u_residual_plot <- ggplot(data = as.data.frame(cbind(fitted = olsu_cbdr_model$fitted.values, residuals = olsu_cbdr_model$residuals)), aes(x = fitted, y = residuals)) +
  geom_point() + theme_cowplot(font_size = 14) +theme(plot.title = element_text(hjust = 0.5))  + 
  labs(x = "Fitted Values", y = "Residual Values", title = "Residual Plot: OLS-cbdr-u") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
ols_cbdr_u_residual_plot
ggsave(filename = 'ols_cbdr_u_residual_plot.png', path = './Figures', width = 15, height = 15, units = 'cm')

ols_cbdr_u_residual_hist <- ggplot(data = as.data.frame(cbind(fitted = olsu_cbdr_model$fitted.values, residuals = olsu_cbdr_model$residuals)), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1,  fill = "#1aa7ec", color = "darkgrey") +  # Customize histogram appearance
  labs(x = "Residual Value", y = "Count") +   ggtitle("Histogram of OLS-cbdr-u Residuals") +  theme_cowplot(font_size = 14)+theme(plot.title = element_text(hjust = 0.5)) 
ols_cbdr_u_residual_grid <- plot_grid(ols_cbdr_u_residual_plot, ols_cbdr_u_residual_hist, nrow = 1, labels = c("A", "B"))
ols_cbdr_u_residual_grid
ggsave(filename = 'ols_cbdr_u_residual_grid.png', path = './Figures', width = 30, height = 15, units = 'cm')



# plotting y = x 
pred_price <- predict(olsu_cbdr_model, newdata = model_data)
scatterplot_ols_cbdr_u_model <- ggplot(data.frame(actual_price = model_data$price, predicted_price = exp(pred_price)), aes(x = actual_price, y = predicted_price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000), labels =c("0", "200", "400", "600", "800", "1")) + 
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1200000), labels =c("0", "200", "400", "600", "800", "1,000", "1,200")) + 
  labs(x = "Actual Price (thousands €)", y = "Predicted Price (thousands €)", title = "OLS-cbdr-u Model: Actual vs Predicted Price") + 
  theme_cowplot(16) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
scatterplot_ols_cbdr_u_model
ggsave(filename = 'scatterplot_ols_cbdr_u_model.png', path = './Figures', width = 15, height = 15, units = 'cm')



##########################
# Model Metrics  

ols_cbdr_moransI_residuals <- Moran.I(ols_cbdr_model$residuals, model_df_inv_dist)
# training data metrics
ols_cbdr_training_metrics <- my_model_metrics(ols_cbdr_model, model_data)
# 5 fold CV 
ols_cbdr_metrics <- rbind(ols_cbdr_metrics <- my_cross_validation(ols_cbdr_formula, fold_list), "Moran's I" = round(ols_cbdr_moransI_residuals$observed,3)) %>% rename('OLS-cbdr' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, ols_cbdr_metrics)


olsu_cbdr_moransI_residuals <- Moran.I(olsu_cbdr_model$residuals, model_df_inv_dist)
# training data metrics
olsu_cbdr_training_metrics <- my_model_metrics(olsu_cbdr_model, model_data)
# 5 fold CV 

olsu_cbdr_metrics <- rbind(olsu_cbdr_metrics <- my_cross_validation(olsu_cbdr_formula, fold_list), "Moran's I" = round(olsu_cbdr_moransI_residuals$observed,3)) %>% rename('OLS-cbdr-u' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, olsu_cbdr_metrics)




#####################################################################################################################################
## 5 - Multiple Linear Regression - Cartesian 
#####################################################################################################################################

olsxy_model <- lm(price_log ~ cartesian_x_c + cartesian_y_c + cartesian_x_c*cartesian_y_c + I(cartesian_x_c^2) + I(cartesian_y_c^2) + beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths_c^3) + 
                    property_type.f + property_condition.f + month_year.f + garage + cul_de_sac + fireplace + period, model_data)
# garage not significant,  aliased coeff in polynomials 2
olsxy_model <- lm(price_log ~ cartesian_x_c + cartesian_y_c + cartesian_x_c*cartesian_y_c + I(cartesian_x_c^2) + I(cartesian_y_c^2) + beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths_c^3) + 
                    property_type.f + property_condition.f + month_year.f + cul_de_sac + fireplace + period, model_data)
olsxy_formula <- price_log ~ cartesian_x_c + cartesian_y_c + cartesian_x_c*cartesian_y_c + I(cartesian_x_c^2) + I(cartesian_y_c^2) + beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths_c^3) + 
                    property_type.f + property_condition.f + month_year.f + cul_de_sac.f + fireplace.f + period.f
olsxy_model <- lm(olsxy_formula, data = model_data)

summary(olsxy_model)
Anova(olsxy_model, type = "III")
vif(olsxy_model)
plot(olsxy_model)
olsxy_coeff <- my_loglinear_coeff(olsxy_model)

##########################
# Plotting Residuals 
map.resids_sf <- cbind(model_data_sf, resids = olsxy_model$residuals) 
olsxy_residuals_plot <- map_urban_area_labels + ggtitle("OLSXY Residuals") +
  geom_sf(data = map.resids_sf, aes(color = resids), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
olsxy_residuals_plot
ols_residuals_plot

# Plotting Residuals in Limerick City 
olsxy_residuals_city_plot <- map_city + ggtitle("OLSXY City Residuals") +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = resids), size = 1) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map()
olsxy_residuals_city_plot

##########################
# Model Metrics  
# Morans I
olsxy_moransI_residuals <- Moran.I(olsxy_model$residuals, model_df_inv_dist)
# training data metrics
olsxy_training_metrics <- my_model_metrics(olsxy_model, model_data)
# 5 fold CV 
olsxy_metrics <- rbind(olsu_cbd_metrics <-  my_cross_validation(olsxy_formula, fold_list), "Moran's I" = round(olsxy_moransI_residuals$observed,3)) %>% rename('OLS-xy' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, olsxy_metrics)





#####################################################################################################################################
## 6 - Multiple Linear Regression - Cartesian + Urban Area 
#####################################################################################################################################

olsu_xy_formula <- price_log ~ cartesian_x_c + cartesian_y_c + cartesian_x_c*cartesian_y_c + I(cartesian_x_c^2) + I(cartesian_y_c^2) + beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths_c^3) + 
  property_type.f + property_condition.f + month_year.f + cul_de_sac + fireplace + period + urban_area.f + urban_area.f*fireplace 
# significant interaction between urban area and fireplace, cul_de_sac insignificant 
olsu_xy_formula <- price_log ~ cartesian_x_c + cartesian_y_c + cartesian_x_c*cartesian_y_c + I(cartesian_x_c^2) + I(cartesian_y_c^2) + beds_c + I(beds_c^2) + baths_c + I(baths_c^2) + I(baths_c^3) + 
  property_type.f + property_condition.f + month_year.f  + fireplace + period + urban_area.f + urban_area.f*fireplace
olsu_xy_model <- lm(olsu_xy_formula, data = model_data)

summary(olsu_xy_model)
Anova(olsu_xy_model, type = "III")
vif(olsu_xy_model)
plot(olsu_xy_model)
olsu_xy_coeff <- my_loglinear_coeff(olsu_xy_model)

##########################
# Plotting Residuals 

map.resids_sf <- cbind(model_data_sf, residuals = olsu_xy_model$residuals) 
# Plotting Residuals 
olsu_xy_residuals_plot <-  map_urban_area_labels +
  geom_sf(data = map.resids_sf, aes(color = residuals), size = 1.5) +
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.3))
olsu_xy_residuals_plot
olsu_xy_residuals_city_plot <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = map.resids_sf[map.resids_sf$municipality=='Limerick City and Suburbs',], aes(color = residuals), size = 1.3) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))  + 
  scale_color_gradient(name = "Residual Value", low = "red", high = "green") + 
  geom_segment(aes(x = -8.63, y = 52.7-0.0026, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.0026, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.60, y = 52.695-0.0026, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") 
olsu_xy_residuals_city_plot
title <- ggdraw() + 
  draw_label(
    "OLS with Cartesian Coordinates and Uban Areas: Residual Values",
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 20
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )  
residual_legend <- get_legend(olsu_xy_residuals_city_plot) 

olsu_xy_residual_city_county_plot <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(-0.6, 1, 3)),
  plot_grid(olsu_xy_residuals_plot + theme(legend.position = 'none'), olsu_xy_residuals_city_plot + theme(legend.position = 'none'), nrow = 1,   
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2),
  plot_grid(NULL, residual_legend, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(1,15, 1),
  nrow = 3
)
olsu_xy_residual_city_county_plot

##########################
# Model Metrics 
# Morans I
olsu_xy_moransI_residuals <- Moran.I(olsu_xy_model$residuals, model_df_inv_dist)
# training data metrics
olsu_xy_training_metrics <- my_model_metrics(olsu_xy_model, model_data)
# 5 fold CV 
olsu_xy_metrics <- rbind(olsu_cbd_metrics <-  my_cross_validation(olsu_xy_formula,  fold_list), "Moran's I" = round(olsu_xy_moransI_residuals$observed,3)) %>% rename('OLS-xy-u' = 'colMeans(metrics_list_df)')
ols_model_results <- cbind(ols_model_results, olsu_xy_metrics)

# LATEX Model results 
ols_model_results <- data_rotate(ols_model_results)

kbl(ols_model_results, booktabs = T, 'latex', digits = c(2, 0, 0, 2, 2, 2, 2, 3))






#####################################################################################################################################
## GWR Model 
#####################################################################################################################################

### Model Info 
gwr1_formula <-price_log ~ baths_c  + beds_c + year.f + property_condition.f + property_type.f + period + cul_de_sac + garage + fireplace
model_dist_matrix <- gw.dist(dp.locat=cbind(model_data$longitude, model_data$latitude), longlat = T)
model_data_sp <- SpatialPointsDataFrame(cbind(model_data$longitude, model_data$latitude), model_data)


### Model Selection 
DeVar <- 'price_log' 
InDeVars <- c('baths_c', 'beds_c', 'year.f', 'property_condition.f', 'property_type.f', 'period', 'cul_de_sac', 'garage', 'fireplace')

model_selection <- gwr.model.selection(DeVar,InDeVars, data=model_data_sp,bw=200,approach="CV",
                    adaptive=T,kernel="gaussian",model_dist_matrix, longlat=T)
# applying model selection to an arbitrary 200 NN bandwidth with gaussian kernel 

# plotting model selection 
model_selection_df <- as.data.frame(model_selection[[2]]) 
model_selection_index <- c(1, 13, 20, 27, 31, 37, 42, 43, 45)
model_selection_df<-model_selection_df[rownames(model_selection_df) %in% model_selection_index, ]
names(model_selection_df) <- c('bandwidth','AIC','AICc','RSS')
model_selection_models <- (model_selection[[1]])
model_selection_variables <- model_selection_models[model_selection_index] %>% sapply(function(x) x[[2]])
added_variable <- c()
for(i in 1:9){
  added_variable[i] <- model_selection_variables[[i]][i] 
}
model_selection_df <- cbind(model_selection_df, added_variable)
model_selection_df$added_variable <- factor(model_selection_df$added_variable, levels = unique(model_selection_df$added_variable))
model_selection_plot <- ggplot(model_selection_df, aes(x = added_variable, y = AICc, group = 1))+
  geom_point(col = "black", shape = 20, size = 3) + geom_line(linetype = "dotted") +
  labs(
    title = "Alternative view of GWR model selection procedure",
    y = "AICc",
    x = "Model number"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels if needed
  )


## Global Model All Variables 
ols_model <- lm(gwr1_formula, data = model_data)
summary(ols_model)
Anova(ols_model, type = 'III')
# garage and fireplace are insignificant at a global level, 
vif(ols_model)
Moran.I(ols_model$residuals, inv_dist_matrix)
# significant autocorrelation

# condition number of global model - boxcar of all observations 
nobs <- dim(model_data_sp)[1]
boxcar_all <- gwr.lcr(gwr1_formula, data = model_data_sp, bw = nobs,
                 kernel = "boxcar", adaptive=TRUE)
global_cn <- summary(boxcar_all$SDF$Local_CN)


### GWR 1 - All Variables 
### Model Calibration - Bandwidth AIC Approach 
bw1_box <- bw.gwr(gwr1_formula,
         data=model_data_sp, approach="CV", kernel="boxcar",
         adaptive=TRUE,dMat=model_dist_matrix)
bw1_bisqu <- bw.gwr(gwr1_formula,
         data=model_data_sp, approach="CV", kernel="bisquare",
         adaptive=TRUE,dMat=model_dist_matrix)
bw1_gas <- bw.gwr(gwr1_formula,
        data=model_data_sp, approach="CV", kernel="gaussian",
        adaptive=TRUE,dMat=model_dist_matrix)
bw1_expon <- bw.gwr(gwr1_formula,
        data=model_data_sp, approach="CV", kernel="exponential",
        adaptive=TRUE,dMat=model_dist_matrix)

### Model Training 
box1_model <- gwr.basic(gwr1_formula, data =model_data_sp, bw = bw1_box, kernel = "boxcar", adaptive = T, longlat = T, dMat = model_dist_matrix)
bisq1_model <- gwr.basic(gwr1_formula, data =model_data_sp, bw = bw1_bisqu, kernel = "bisquare", adaptive = T, longlat = T, dMat = model_dist_matrix)
gas1_model <- gwr.basic(gwr1_formula, data =model_data_sp, bw = bw1_gas, kernel = "gaussian", adaptive = T, longlat = T, dMat = model_dist_matrix)
expon1_model <- gwr.basic(gwr1_formula, data =model_data_sp, bw = bw1_expon, kernel = "exponential", adaptive = T, longlat = T, dMat = model_dist_matrix)

gwr1_metrics <- as.data.frame(rbind(c('boxcar', bw1_box, box1_model$GW.diagnostic$gwR2.adj, box1_model$GW.diagnostic$AICc, box1_model$GW.diagnostic$AIC), 
      c('bisq', bw1_bisqu, bisq1_model$GW.diagnostic$gwR2.adj, bisq1_model$GW.diagnostic$AICc,bisq1_model$GW.diagnostic$AIC), 
      c('gas', bw1_gas, gas1_model$GW.diagnostic$gwR2.adj, gas1_model$GW.diagnostic$AICc,gas1_model$GW.diagnostic$AIC), 
      c('exp', bw1_expon, expon1_model$GW.diagnostic$gwR2.adj, expon1_model$GW.diagnostic$AICc,expon1_model$GW.diagnostic$AIC))) %>% 
  setNames(c('Kernel', 'bandwidth','Adj r2', 'AICc', 'AIC')) 
gwr1_metrics$Kernel <- paste("GWR-1", gwr1_metrics$Kernel, sep = " ")
gwr1_metrics <- as.numeric(gwr1_metrics)


box1_diag <- my_gwr.collin.diagno(gwr1_formula, bw1_box, 'boxcar')
bisq1_diag <- my_gwr.collin.diagno(gwr1_formula, bw1_bisqu, 'bisquare')
gas1_diag <- my_gwr.collin.diagno(gwr1_formula, bw1_gas, 'gaussian')
exp1_diag <- my_gwr.collin.diagno(gwr1_formula, bw1_expon, 'exponential')

summary(box1_diag$SDF$local_CN)
summary(bisq1_diag$SDF$local_CN)
summary(gas1_diag$SDF$local_CN)
summary(exp1_diag$SDF$local_CN)


# plotting GWR1 CN over 30 
gwr1map_data <- model_data_sf %>% mutate(
  lcn = bisq1_diag$SDF$local_CN,
  over30 = ifelse(lcn > 30, 1, 0))

map_gwr1_CN <- map_urban_area_labels +
  geom_sf(data = gwr1map_data, aes(color = as.factor(over30)), size = 1) + ggtitle("GWR-1: Condition Numbers") + 
  scale_color_manual(values = c("darkgrey", "green"), labels = c("0" = "CN < 30", "1" = "CN > 30"), name = 'Condition Number (CN)') +  
  theme_map() +   my_gwr_map_theme
map_gwr1_CN









### GWR 2 - Parsimonious Model 
gwr2_formula <- price_log ~ baths_c + property_condition.f + property_type.f  + year.f

### Model Calibration - Bandwidth 
bw2_box <- bw.gwr(gwr2_formula,
                 data=model_data_sp, approach="AIC", kernel="boxcar",
                 adaptive=TRUE,dMat=model_dist_matrix)
bw2_bisqu <- bw.gwr(gwr2_formula,
                   data=model_data_sp, approach="AIC", kernel="bisquare",
                   adaptive=TRUE,dMat=model_dist_matrix)
bw2_gas <- bw.gwr(gwr2_formula,
                 data=model_data_sp, approach="AIC", kernel="gaussian",
                 adaptive=TRUE,dMat=model_dist_matrix)
bw2_expon <- bw.gwr(gwr2_formula,
                   data=model_data_sp, approach="AIC", kernel="exponential",
                   adaptive=TRUE,dMat=model_dist_matrix)

### Model Training 
box2_model <- gwr.basic(gwr2_formula, data =model_data_sp, bw = bw2_box, kernel = "boxcar", adaptive = T, longlat = T, dMat = model_dist_matrix)
bisq2_model <- gwr.basic(gwr2_formula, data =model_data_sp, bw = bw2_bisqu, kernel = "bisquare", adaptive = T, longlat = T, dMat = model_dist_matrix)
gas2_model <- gwr.basic(gwr2_formula, data =model_data_sp, bw = bw2_gas, kernel = "gaussian", adaptive = T, longlat = T, dMat = model_dist_matrix)
expon2_model <- gwr.basic(gwr2_formula, data =model_data_sp, bw = bw2_expon, kernel = "exponential", adaptive = T, longlat = T, dMat = model_dist_matrix)

gwr2_metrics <- as.data.frame(rbind(c('boxcar', bw2_box, box2_model$GW.diagnostic$gwR2.adj, box2_model$GW.diagnostic$AICc, box2_model$GW.diagnostic$AIC), 
                                   c('bisq', bw2_bisqu, bisq2_model$GW.diagnostic$gwR2.adj, bisq2_model$GW.diagnostic$AICc,bisq2_model$GW.diagnostic$AIC), 
                                   c('gas', bw2_gas, gas2_model$GW.diagnostic$gwR2.adj, gas2_model$GW.diagnostic$AICc,gas2_model$GW.diagnostic$AIC), 
                                   c('exp', bw2_expon, expon2_model$GW.diagnostic$gwR2.adj, expon2_model$GW.diagnostic$AICc,expon2_model$GW.diagnostic$AIC))) %>% 
  setNames(c('Kernel', 'bandwidth', 'Adj r2', 'AICc', 'AIC')) 
gwr2_metrics$Kernel <- paste("GWR-2", gwr2_metrics$Kernel, sep = " ")


gwr1_2_table <- rbind(gwr1_metrics, gwr2_metrics)
kbl(gwr1_2_table, booktabs = T, 'latex', digits = c(0, 1, 1, 1, 1))



box2_diag <- my_gwr.collin.diagno(gwr2_formula, bw2_box, 'boxcar')
bisq2_diag <- my_gwr.collin.diagno(gwr2_formula, bw2_bisqu, 'bisquare')
gas2_diag <- my_gwr.collin.diagno(gwr2_formula, bw2_gas, 'gaussian')

summary(box2_diag$SDF$local_CN)
summary(bisq2_diag$SDF$local_CN)
summary(gas2_diag$SDF$local_CN)

# VIF values 
bisq2_vif_values <- c()
for(i in 1:11){
  nameb= names(bisq2_diag$SDF)[i]
  countb = sum(bisq2_diag$SDF[[i]] > 10)
  bisq2_vif_values <- rbind(bisq2_vif_values, c(nameb, countb))
}
bisq2_vif_values
bisq2_cn <- sum(bisq2_diag$SDF[[12]] > 30)

# bandwidths for
model_fits <- c()
diagnostics_listb <- list()
i=1
for(b in c(669, 700, 800, 1000, 1200, 1400)){
  model <- gwr.basic(gwr2_formula, data =model_data_sp, bw = b, kernel = "bisquare", adaptive = T, longlat = T, dMat = model_dist_matrix)
  diagnostics <- my_gwr.collin.diagno(gwr2_formula, b, 'bisquare')
  model_fits <- rbind(model_fits, c('bisq', b, model$GW.diagnostic$gwR2.adj, model$GW.diagnostic$AICc, model$GW.diagnostic$AIC))
  diagnostics_listb[[i]]  <- diagnostics
  i <- i+1
}


diag_results <- c()
for(i in 1:6){
  diag_results <- as.data.frame(rbind(diag_results, 
  c(condition_30 = sum(diagnostics_listb[[i]]$local_CN > 30),
  baths_c_VIF_10 = sum(diagnostics_listb[[i]]$SDF$baths_c_VIF > 10),
  property_codition.f1_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_condition.f1_VIF > 10),
  property_codition.f2_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_condition.f2_VIF > 10),
  property_codition.f3_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_condition.f3_VIF > 10),
  property_tye.f1_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_type.f1_VDP > 10),
  property_tye.f2_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_type.f2_VIF > 10),
  property_tye.f3_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_type.f3_VIF > 10),
  property_tye.f4_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_type.f4_VIF > 10),
  property_tye.f5_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_type.f5_VIF > 10),
  property_tye.f6_VIF_10 = sum(diagnostics_listb[[i]]$SDF$property_type.f6_VIF > 10),
  year.f1_VIF_10 = sum(diagnostics_listb[[i]]$SDF$year.f1_VIF > 10))))

}
rownames(diag_results) <- c(669, 700, 800, 1000, 1200, 1400)
names(diag_results) <- c("CN30", "BATHS", "PC.1", "PC.2", "PC.3", "PT.1", "PT.2", "PT.3", "PT.4", "PT.5", "PT.6", "YEAR")
kbl(diag_results, booktabs = T, 'latex')

# plotting GWR2 CN over 30 
gwr2map_data <- model_data_sf %>% mutate(
  lcn = bisq2_diag$SDF$local_CN,
  pt1 = bisq2_diag$SDF$property_type.f6_VIF,
  over30 = ifelse(lcn > 30, 1, 0),
  over10 = ifelse(pt1 > 10, 1, 0))

gwr2map_data <- gwr2map_data %>%
  arrange((over30))
map_gwr2_CN <- map_urban_area_labels +
  geom_sf(data = gwr2map_data, aes(color = as.factor(over30)), size = 1) + ggtitle("GWR-2: Condition Numbers") + 
  scale_color_manual(values = c("darkgrey", "red"), labels = c("0" = "CN < 30", "1" = "CN > 30"), name = 'Condition Number (CN): ') +  
  theme_map() +  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    legend.justification = 'center',
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
#+my_gwr_map_theme + theme(legend.position = c(.15, .9), legend.title.align=0.5) 
map_gwr2_CN





## GWR 3 - Parsimonous 2 
DeVar <- 'price_log' 
InDeVars <- c('baths_c', 'beds_c', 'year.f', 'property_category.f', 'cul_de_sac', 'garage', 'fireplace')

model_selection <- gwr.model.selection(DeVar,InDeVars, data=model_data_sp,bw=500,approach="AIC",
                                       adaptive=T,kernel="bisquare",model_dist_matrix, longlat=T)
# applying model selection to an arbitrary 500 NN bandwidth with bisquare kernel 

# plotting model selection 
model_selection_df <- as.data.frame(model_selection[[2]]) 
model_selection_index <- c(1, 8, 18, 20, 25, 26, 28)
model_selection_df<-model_selection_df[rownames(model_selection_df) %in% model_selection_index, ]
names(model_selection_df) <- c('bandwidth','AIC','AICc','RSS')
model_selection_models <- (model_selection[[1]])
model_selection_variables <- model_selection_models[model_selection_index] %>% sapply(function(x) x[[2]])
added_variable <- c()
for(i in 1:7){
  added_variable[i] <- model_selection_variables[[i]][i] 
}
model_selection_df <- cbind(model_selection_df, added_variable)

# Apply the mapping to the data
variable_names <- unique(model_selection_df$added_variable)
label_mapping <- setNames(c('Baths', 'Beds', 'Fireplace', 'Prop Category', 'Garage', 'Year', 'Cul-de-sac'), variable_names)
model_selection_df$added_variable_label <- label_mapping[model_selection_df$added_variable]
model_selection_df$added_variable_label <- factor(model_selection_df$added_variable_label, levels = unique(model_selection_df$added_variable_label))
model_selection_plot <- ggplot(model_selection_df, aes(x = added_variable_label, y = AICc, group = 1)) + 
  geom_point(col = "black", shape = 20, size = 3) + geom_line(linetype = "dotted")+ 
  labs(
    title = "GWR-3: Model Selection",
    y = "AICc",
    x = "Added Variable"
  ) +
  theme_cowplot(font_size = 14)+theme(plot.title = element_text(hjust = 0.5)) + 
  theme(
    axis.text.x = element_text(angle = 10, hjust = 1) # Rotate x-axis labels if needed
  )
model_selection_plot
ggsave(filename = 'gwr3_model_selection.png', path = './Figures', width = 20, height = 10, units = 'cm')




# choosing a parsimonious formula 
gwr3_formula <-price_log ~ baths_c  + beds_c + fireplace + year.f
### Model Calibration - Bandwidth 
bw3_box <- bw.gwr(gwr3_formula,
                  data=model_data_sp, approach="CV", kernel="boxcar",
                  adaptive=TRUE,dMat=model_dist_matrix)
bw3_bisqu <- bw.gwr(gwr3_formula,
                    data=model_data_sp, approach="CV", kernel="bisquare",
                    adaptive=TRUE,dMat=model_dist_matrix)
bw3_gas <- bw.gwr(gwr3_formula,
                  data=model_data_sp, approach="CV", kernel="gaussian",
                  adaptive=TRUE,dMat=model_dist_matrix)
bw3_expon <- bw.gwr(gwr3_formula,
                    data=model_data_sp, approach="CV", kernel="exponential",
                    adaptive=TRUE,dMat=model_dist_matrix)

### Model Training 
box3_model <- gwr.basic(gwr3_formula, data =model_data_sp, bw = bw3_box, kernel = "boxcar", adaptive = T, longlat = T, dMat = model_dist_matrix)
bisq3_model <- gwr.basic(gwr3_formula, data =model_data_sp, bw = bw3_bisqu, kernel = "bisquare", adaptive = T, longlat = T, dMat = model_dist_matrix)
gas3_model <- gwr.basic(gwr3_formula, data =model_data_sp, bw = bw3_gas, kernel = "gaussian", adaptive = T, longlat = T, dMat = model_dist_matrix)
expon3_model <- gwr.basic(gwr3_formula, data =model_data_sp, bw = bw3_expon, kernel = "exponential", adaptive = T, longlat = T, dMat = model_dist_matrix)

gwr3_metrics <- as.data.frame(rbind(c('boxcar', bw3_box, box3_model$GW.diagnostic$gwR2.adj, box3_model$GW.diagnostic$AICc, box3_model$GW.diagnostic$AIC), 
                                    c('bisq', bw3_bisqu, bisq3_model$GW.diagnostic$gwR2.adj, bisq3_model$GW.diagnostic$AICc,bisq3_model$GW.diagnostic$AIC), 
                                    c('gas', bw3_gas, gas3_model$GW.diagnostic$gwR2.adj, gas3_model$GW.diagnostic$AICc,gas3_model$GW.diagnostic$AIC), 
                                    c('exp', bw3_expon, expon3_model$GW.diagnostic$gwR2.adj, expon3_model$GW.diagnostic$AICc,expon3_model$GW.diagnostic$AIC))) %>% 
  setNames(c('Kernel', 'bandwidth','Adj r2', 'AICc', 'AIC')) 
gwr3_metrics
gwr3_metrics$Kernel <- paste("GWR-3", gwr3_metrics$Kernel, sep = " ")

gwr1_2_3_table <- rbind(gwr1_2_table, gwr3_metrics)

kbl(gwr1_2_3_table, booktabs = T, 'latex')

box3_diag <- my_gwr.collin.diagno(gwr3_formula, bw3_box, 'boxcar')
bisq3_diag <- my_gwr.collin.diagno(gwr3_formula, bw3_bisqu, 'bisquare')
gas3_diag <- my_gwr.collin.diagno(gwr3_formula, bw3_gas, 'gaussian')
exp3_diag <- my_gwr.collin.diagno(gwr3_formula, bw3_expon, 'exponential')

summary(box3_diag$SDF$local_CN)
summary(bisq3_diag$SDF$local_CN)
summary(gas3_diag$SDF$local_CN)
summary(exp3_diag$SDF$local_CN)

# VIF values
bisq3_vif_values <- c()
for(i in 1:4){
  nameb= names(bisq3_diag$SDF)[i]
  maxb = max(bisq3_diag$SDF[[i]])  
  meanb =   median(bisq3_diag$SDF[[i]])  
  countb = sum(bisq3_diag$SDF[[i]] > 10)
  bisq3_vif_values <- rbind(bisq3_vif_values, c(nameb, countb, maxb, meanb))
}
bisq3_vif_values
bisq3_cn <- sum(bisq2_diag$SDF[[5]] > 30)
median(bisq2_diag$SDF[[5]])
## GWR metrics 
gwr3_residuals_moransI <- Moran.I(map_gwr3_df$gwr_residual, inv_dist_matrix)

gwr3_metrics <- round(c(
  Adj_R2 = bisq3_model$GW.diagnostic$gwR2.adj,
  AICc = bisq3_model$GW.diagnostic$AICc,
  RMSE = rmse(gwr_actual, gwr_fitted),
  MedianAPE = cognitiveutils::MDAPE(gwr_actual, gwr_fitted),
  MeanAPE = mape(gwr_actual, gwr_fitted),
  MeanAE = mae(gwr_actual, gwr_fitted),
  Within10_True = sum(abs(gwr_fitted - gwr_actual) / gwr_actual <= 0.1) / length(gwr_actual),
  Within20_True = sum(abs(gwr_fitted - gwr_actual) / gwr_actual <= 0.2) / length(gwr_actual)
) %>% as.data.frame(),3)



##### Model Interpretation - GWR Property Category 

gwr3_coeff <- readxl::read_excel("./results/gwr3_coeff2.xlsx")

gwr3_coeff_exp <- data.frame()
for(i in 1:5){
  row <- gwr3_coeff[i,]
  exp_values <- exp(row[,2:5])
  exp_values <- round(exp_values, 2)
  exp_values <- paste0("(", exp_values, ")")
  name <- row[,1]
  gwr3_coeff_exp <- rbind(gwr3_coeff_exp, c(row), c(Term = '', exp_values, Global_p = '', MC_p = ''))
}
gwr3_coeff_exp <- gwr3_coeff_exp %>% mutate_if(is.numeric, round, digits = 2)
kbl(gwr3_coeff_exp, booktabs = T, 'latex')


## Mapping gwr3 Bisq
map_gwr3_df <- model_data_sf %>% mutate(
  gwr_intercept = exp(bisq3_model$SDF$Intercept),
  gwr_localR2 = bisq3_model$SDF$Local_R2,
  gwr_residual = bisq3_model$SDF$residual,
  gwr_bed_coeff = exp(bisq3_model$SDF$beds_c),
  gwr_bath_coeff = exp(bisq3_model$SDF$baths_c),
  gwr_fireplace_coeff = exp(bisq3_model$SDF$fireplaceTRUE),
  gwr_beds_tv = bisq3_model$SDF$beds_c,
  gwr_beds_ts = bisq3_model$SDF$beds_c/bisq3_model$SDF$beds_c_SE,
  gwr_bath_ts = bisq3_model$SDF$baths_c/bisq3_model$SDF$baths_c_SE,
  gwr_fireplaceTRUE_ts = bisq3_model$SDF$fireplaceTRUE/bisq3_model$SDF$fireplaceTRUE_SE,
  beds_sign = ifelse(gwr_beds_ts > -1.96 & gwr_beds_ts < 1.96, 1, 0),
  fp_sign = ifelse(gwr_fireplaceTRUE_ts > -1.96 & gwr_fireplaceTRUE_ts < 1.96, 1, 0),
  bath_sign = ifelse(gwr_bath_ts > -1.96 & gwr_bath_ts < 1.96, 1, 0)
)


map_gwr3_intercept <- map_urban_area_labels + ggtitle("GWR-3: Intercept") + 
  geom_sf(data = map_gwr3_df, aes(color = gwr_intercept), size = 1) +
  scale_color_gradient(name = 'Intercept Value \n(thousands €)', low = "red", high = "green", breaks = c(100000,150000, 200000,250000), labels = c("100", "150", "200", "250")) +
  theme_map() +  my_gwr_map_theme
map_gwr3_intercept

map_gwr3_localr2 <-  map_urban_area_labels  + ggtitle("GWR-3: Local R Squared") + 
  geom_sf(data = map_gwr3_df, aes(color = gwr_localR2), size = 1) +
  scale_color_gradient(name = 'Local R-Squared', low = "red", high = "green", breaks = c(0.4,0.6, 0.8), labels = c("40%", "60%", "80%")) +
  theme_map() +  my_gwr_map_theme
map_gwr3_localr2

map_gwr3_residuals <-  map_urban_area_labels  + ggtitle("GWR-3: Residuals") + 
  geom_sf(data = map_gwr3_df, aes(color = gwr_residual), size = 1) +
  scale_color_gradient(name = 'Residual Values', low = "red", high = "green") +
  theme_map()+ my_gwr_map_theme
map_gwr3_residuals

map_gwr3_bed_coeff <-  map_urban_area_labels  + ggtitle("GWR-3: Beds Coefficients") + 
  geom_sf(data = map_gwr3_df, aes(color = gwr_bed_coeff), size = 1) +
  scale_color_gradient(name = 'Bed Coefficient \n(exponentiated)', low = "red", high = "green") +
  theme_map()+  my_gwr_map_theme
map_gwr3_bed_coeff

map_gwr3_df$beds_sign <- factor(map_gwr3_df$beds_sign, levels = c("0", "1"))
map_gwr3_df <- map_gwr3_df %>%
  arrange((beds_sign))
map_gwr3_bed_sign <- map_urban_area_labels + ggtitle("GWR-3: Beds Coefficients Significance") + 
  geom_sf(data = map_gwr3_df, aes(color = (beds_sign))) + 
  scale_color_manual(values = c('darkgrey', "green"), labels = c('0' = "Non-Significant", '1'  = "Significant"), name = 'Exploratory t-test:') +  
  theme_map() + theme(
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    legend.justification = 'center',
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) 
# theme(legend.position = c(.15, .9), legend.title.align=0.5) 
map_gwr3_bed_sign

# filtered_points <- map_gwr3_df[map_gwr3_df$fp_sign == 1, ]
# filtered_points_sf <- st_as_sf(filtered_points)
map_gwr3_bath_coeff <-  map_urban_area_labels + ggtitle("GWR-3: Baths Coefficients") + 
  geom_sf(data = map_gwr3_df, aes(color = gwr_bath_coeff), size = 1) +
  scale_color_gradient(name = 'Bath Coefficient \n(exponentiated)', low = "red", high = "green") +
  theme_map()+ theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, 'cm'),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.justification = 'center',
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

# GWR-3 Bath coeff in Lmk
map1 <-  map_urban_area_labels +
  geom_sf(data = map_gwr3_df, aes(color = gwr_bath_coeff), size = 1) +
  scale_color_gradient(name = "Bath Coefficient \n(exponentiated)", low = "red", high = "green") +
  theme_map() + theme(legend.spacing.y = unit(0.5, 'cm'), plot.title = element_text(hjust = 0.5)) + theme(plot.margin = margin(0,0,0,0, 'cm'))
map1

# GWR-3 Bath coeff in LCS 
map2 <- map_city_labels +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = map_gwr3_df[map_gwr3_df$municipality=='Limerick City and Suburbs',], aes(color = gwr_bath_coeff), size =1) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.spacing.x = unit(1, 'cm'),
        legend.justification = 'center', legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))  + 
  scale_color_gradient(name = "Bath Coefficient \n(exponentiated)", low = "red", high = "green") + 
  geom_segment(aes(x = -8.63, y = 52.707-0.003, xend = -8.63, yend = 52.676),
               arrow = arrow(length = unit(0.1, "cm")), #2
               color = "black") +
  geom_segment(aes(x = -8.58, y = 52.685-0.003, xend = -8.60, yend = 52.67),
               arrow = arrow(length = unit(0.1, "cm")), #3
               color = "black") +
  geom_segment(aes(x = -8.595, y = 52.698-0.003, xend = -8.625, yend = 52.665),
               arrow = arrow(length = unit(0.1, "cm")), #4
               color = "black") + theme(plot.margin = margin(0,0,0,0, 'cm'))
map2


# title for grid property prices map \
title <- ggdraw() + 
  draw_label(
    'GWR-3: Bath Coefficient',
    fontface = 'bold',
    x = 0,
    hjust = -0.5,
    size = 18
  ) + theme(plot.margin = margin(0,0,0,0))
# legend for grid 
legend_price <- get_legend(map2) 
# grid of proeprty prices 
gwr3_bath_county_city <- plot_grid(
  plot_grid(NULL, title, NULL, nrow = 1, rel_widths = c(0.4, 1, 0.1)),
  plot_grid(map1 + theme(legend.position = 'none'), map2 + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = c('A', 'B'), 
            label_size = 14,
            ncol = 2, vjust = 4),
  plot_grid(NULL, legend_price, NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(0.05,1, 0.1),
  nrow = 3 
)
gwr3_bath_county_city
ggsave(filename = 'gwr3_bath_coeff.png', path = './Figures', width = 30, height = 15, units = 'cm')



# bath coeff map for postwer 
p1 <- map1 + ggtitle('GWR-3 Bath Coeffient:\nLimerick County') + theme(plot.title = element_text(hjust = 0.1, vjust = -10, color = 'black', size = 16)) 
p2 <- map2 + ggtitle('GWR-3 Bath Coeffient:\n Limerick City and Suburbs')+ theme(plot.title = element_text(hjust = 0.1, vjust = -25, color = 'black', size = 16)) 
poster_bath_gwr3_grid <- plot_grid(
  plot_grid(p1 + theme(legend.position = 'none'), p2 + theme(legend.position = 'none'), nrow = 1,  rel_widths = c(0.9, 1), 
            labels = NA, 
            ncol = 2),
  plot_grid(NULL, get_legend(map2), NULL, nrow = 1, rel_widths = c(0.05, 1, 0.05)),
  rel_heights = c(1, 0.1),
  nrow = 2 
)
poster_bath_gwr3_grid
ggsave(filename = 'poster_bath_gwr3_grid.png', path = '../../Poster/Figures', dpi = 600, width = 30, height = 15,  units = 'cm')




#geom_sf(data = filtered_points_sf,fill = NA, color = 'black', shape = 1, size = 1.5)
map_gwr3_bath_coeff

map_gwr3_fireplace_coeff <-  map_urban_area_labels  + ggtitle("GWR-3: Fireplace Coefficients") + 
  geom_sf(data = map_gwr3_df, aes(color = gwr_fireplace_coeff), size = 1) +
  scale_color_gradient(name = 'Fireplace Coefficient \n(exponentiated)', low = "red", high = "green") +
  theme_map()+   theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, 'cm'),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.justification = 'center',
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
map_gwr3_fireplace_coeff


gwr_maps1 <- plot_grid(map_gwr2_CN, map_gwr3_intercept, map_gwr3_bed_coeff,map_gwr3_bed_sign, labels = "AUTO", nrow = 2, ncol = 2, align = 'h')
gwr_maps1
ggsave("gwr_maps1.png", path = './Figures', width = 25, height = 30,  units = "cm")
gwr_maps2 <- plot_grid(map_gwr3_bath_coeff, map_gwr3_fireplace_coeff, map_gwr3_residuals, map_gwr3_localr2, labels = "AUTO", nrow = 2, ncol = 2, align = 'h')
gwr_maps2
ggsave("gwr_maps2.png", path = './Figures', width = 25, height = 30,  units = "cm")


plot_grid(map_gwr3_residuals, ols_residuals_plot)
# Plotting Residuals in Limerick City 
map_gwr_residuals_city <- map_city  + ggtitle("GWR Residuals - Limerick City")  + 
  geom_sf(data = map_gwr_df[map_gwr_df$region == "Limerick City and Suburbs",], aes(color = gwr_residual), size = 1) +
  scale_color_gradient(name = "GWR  Residuals - City", low = "red", high = "green") +
  theme_map()
map_gwr_residuals_city
ols_residuals_city_plot
plot_grid(map_gwr_residuals_city, ols_residuals_city_plot)


# reisdual plot 
gwr3_residual_plot <- ggplot(data = as.data.frame(cbind(fitted = bisq3_model$SDF$yhat, residuals = bisq3_model$SDF$residual)), aes(x = fitted, y = residuals)) +
  geom_point() + theme_cowplot(font_size = 14) +theme(plot.title = element_text(hjust = 0.5))  + 
  labs(x = "Fitted Values", y = "Residual Values", title = "Residual Plot: GWR-3") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") 
gwr3_residual_plot
ggsave(filename = 'gwr3_residual_plot.png', path = './Figures', width = 15, height = 15, units = 'cm')

gwr3_residual_hist <- ggplot(data = as.data.frame(cbind(fitted = bisq3_model$SDF$yhat, residuals = bisq3_model$SDF$residual)), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1,  fill = "#1aa7ec", color = "darkgrey") +  # Customize histogram appearance
  labs(x = "Residual Value", y = "Count") +   ggtitle("Histogram of GWR-3 Residuals") +  theme_cowplot(font_size = 14)+theme(plot.title = element_text(hjust = 0.5)) 
gwr3_residual_grid <- plot_grid(gwr3_residual_plot, gwr3_residual_hist, nrow = 1, labels = c("A", "B"))
gwr3_residual_grid
ggsave(filename = 'gwr3_residual_grid.png', path = './Figures', width = 30, height = 15, units = 'cm')
ks.test(bisq3_model$SDF$residual)

# test the reisuals for normality 
ks_test <- bisq3_model$SDF$residual
shapiro.test(ks_test)
ks.test(ks_test, "pnorm", mean=mean(ks_test), sd=sd(ks_test)) # normally distributed 
qqnorm(ks_test):qqline(ks_test)



## GWR metrics 
gwr3_residuals_moransI <- Moran.I(map_gwr3_df$gwr_residual, inv_dist_matrix)

gwr_fitted <- exp(bisq3_model$SDF$yhat)
gwr_actual <- model_data$price

# plot x = y for actual vs fitted 
pred_price <- gwr_fitted
scatterplot_gwr3_model <- ggplot(data.frame(actual_price = gwr_actual, predicted_price = (pred_price)), aes(x = actual_price, y = predicted_price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000), labels =c("0", "200", "400", "600", "800", "1")) + 
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1200000), labels =c("0", "200", "400", "600", "800", "1,000", "1,200")) + 
  labs(x = "Actual Price (thousands €)", y = "Predicted Price (thousands €)", title = "GWR-3 Model: Actual vs Predicted Price") + 
  theme_cowplot(font_size = 16) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
scatterplot_gwr3_model
ggsave(filename = 'scatterplot_gwr3_model.png', path = './Figures', width = 15, height = 15, units = 'cm')

ols_gwr_pred_actual <- plot_grid(scatterplot_ols_cbdr_u_model, scatterplot_gwr3_model, label_size = 14)
ggsave(filename = 'ols_gwr_pred_actual.png', path = './Figures', width = 35, height = 17, units = 'cm')

ols_gwr_pred_actual_poster <- plot_grid(scatterplot_ols_cbdr_u_model + labs(title = 'MLR Model: Actual vs Predicted Price'), scatterplot_gwr3_model + labs(title = 'GWR Model: Actual vs Predicted Price'), ncol = 2, nrow = 1, labels = NA)
ggsave(filename = 'poster_ols_gwr_pred_actual.png', path = '../../Poster/Figures', width = 35, height = 13, dpi = 700, units = 'cm')




# metrics 
gwr3_metrics <- round(c(
  Adj_R2 = bisq3_model$GW.diagnostic$gwR2.adj,
  AICc = bisq3_model$GW.diagnostic$AICc,
  RMSE = rmse(gwr_actual, gwr_fitted),
  MedianAPE = cognitiveutils::MDAPE(gwr_actual, gwr_fitted),
  MeanAPE = mape(gwr_actual, gwr_fitted),
  MeanAE = mae(gwr_actual, gwr_fitted),
  Within10_True = sum(abs(gwr_fitted - gwr_actual) / gwr_actual <= 0.1) / length(gwr_actual),
  Within20_True = sum(abs(gwr_fitted - gwr_actual) / gwr_actual <= 0.2) / length(gwr_actual)
) %>% as.data.frame(),3)



###  Monte Carlo significance test 
gwr1_bisqu_mc <- gwr.montecarlo(formula = gwr1_formula, data =model_data_sp,nsims=99, kernel="bisquare",adaptive=T, bw1_bisqu,
                                p=2, theta=0, longlat=T,dMat = model_dist_matrix)

gwr3_bisqu_mc <- gwr.montecarlo(formula = gwr3_formula, data =model_data_sp,nsims=99, kernel="bisquare",adaptive=T, bw3_bisqu,
                                p=2, theta=0, longlat=T,dMat = model_dist_matrix)




# Predictive Accuracy 

gwr_pred_formula <- price_log ~ baths_c + beds_c + year.f + fireplace.f
inde_var <- c('price_log','baths_c', 'beds_c', 'year.f', 'fireplace.f')


cv_col1 <- c()
for (i in 1:10){
  ids <- c(1:10)
  test <- fold_list[i]
  train <- bind_rows(fold_list[ids[-i]])
  
  write.table(test, "./Train+Test/test.csv", col.names = TRUE, row.names = FALSE,
              sep = ",")
  write.table(train, "./Train+Test/train.csv", col.names = TRUE, row.names = FALSE,
              sep = ",")
  
  ewhp_calib <- read.table("./Train+Test/train.csv", header = TRUE, sep = ",")
  contrasts(ewhp_calib$fireplace.f) <- contr.sum(2)
  
  
  ewhp_calib.spdf <- SpatialPointsDataFrame(ewhp_calib[, 3:4],
                                            as.data.frame(ewhp_calib))
  ewhp_calib.spdf$fireplace.f <- ifelse(ewhp_calib.spdf$fireplace.f==TRUE, 1, -1)
  dm.calib <- gw.dist(dp.locat = coordinates(ewhp_calib.spdf))
  gwr.bw.cv <- bw.gwr(price_log ~ baths_c + beds_c + fireplace.f + year.f,data = ewhp_calib.spdf,
                      approach = "CV", kernel = "bisquare", adaptive = TRUE, dMat = dm.calib)
  ewhp_valid <- read.table("./Train+Test/test.csv", header = TRUE, sep = ",")
  names(ewhp_valid) <- names(ewhp_calib)
  contrasts(ewhp_valid$fireplace.f) <- contr.sum(2)
  
  ewhp_valid.spdf <- SpatialPointsDataFrame(ewhp_valid[, 3:4],
                                            as.data.frame(ewhp_valid))
  ewhp_valid.spdf$fireplace.f <- ifelse(ewhp_valid.spdf$fireplace.f==TRUE, 1, -1)
  dm.valid <- gw.dist(dp.locat = coordinates(ewhp_valid.spdf),
                      rp.locat = coordinates(ewhp_calib.spdf), longlat = T)
  
  gwr.pred <- gwr.predict(gwr_pred_formula,data = ewhp_calib.spdf,
                          predictdata = ewhp_valid.spdf ,bw = gwr.bw.cv, kernel = "bisquare",
                          adaptive = TRUE, dMat1 = dm.valid, dMat2 = dm.calib, longlat = T)
  predicted <- gwr.pred$SDF$prediction
  fitted_price <- exp(gwr.pred$SDF$prediction)
  actual <- ewhp_valid.spdf$price_log
  actual_price <- ewhp_valid.spdf$price
  variance <- gwr.pred$SDF$prediction_var
  percentage_within_10_percent <- sum(abs(fitted_price - actual_price) / actual_price <= 0.1) / length(actual_price)
  percentage_within_20_percent <- sum(abs(fitted_price - actual_price) / actual_price <= 0.2) / length(actual_price)
  gwrPI_95 <- data.frame(lower = predicted - 1.96*sqrt(variance), upper = predicted + 1.96*sqrt(variance))
  percentage_within_95_PI <- sum(actual >= gwrPI_95$lower & actual <= gwrPI_95$upper) / length(actual)
  gwr_pred_metrics <- c(
    Adj_R2 = cor(fitted_price, actual_price),
    AIC = 826.6325, 
    RMSE = rmse(actual_price, fitted_price),
    MedianAPE = (cognitiveutils::MDAPE(actual_price, fitted_price))*100,
    Within10_True = (percentage_within_10_percent)*100,
    Within20_True = (percentage_within_20_percent)*100, 
    Within95_PredInt = percentage_within_95_PI*100
  ) 
  cv_col1 <- rbind(cv_col1, gwr_pred_metrics)
  print('iteration complete')
}

gwr3_global_model <- lm(gwr3_formula, data = model_data)
gwr3_global_moransI <- Moran.I(gwr3_global_model$residuals, model_df_inv_dist)
gwr3_global_metrics <- round(rbind(my_cross_validation(gwr3_formula, fold_list), "Moran's I" = round(gwr3_global_moransI$observed,3)) %>% rename('Global Model' = 'colMeans(metrics_list_df)'),2)
gwr3_cv <- round(rbind(as.data.frame(colMeans(cv_col1, na.rm = TRUE)), "Moran's I" = round(gwr3_residuals_moransI$observed, 3)) %>% rename('GWR Model' = 'colMeans(cv_col1, na.rm = TRUE)'),2)
gwr_cv_results <- data_rotate(cbind(gwr3_global_metrics, gwr3_cv))


kbl(gwr_cv_results, booktabs = T, 'latex')




# GWR4 - BED BATH 
gwr4_formula <-price_log ~ baths_c  + beds_c + year.f
### Model Calibration - Bandwidth 
bw4_box <- bw.gwr(gwr4_formula,
                  data=model_data_sp, approach="CV", kernel="boxcar",
                  adaptive=TRUE,dMat=model_dist_matrix)
bw4_bisqu <- bw.gwr(gwr4_formula,
                    data=model_data_sp, approach="CV", kernel="bisquare",
                    adaptive=TRUE,dMat=model_dist_matrix)
bw4_gas <- bw.gwr(gwr4_formula,
                  data=model_data_sp, approach="CV", kernel="gaussian",
                  adaptive=TRUE,dMat=model_dist_matrix)
bw4_expon <- bw.gwr(gwr4_formula,
                    data=model_data_sp, approach="CV", kernel="exponential",
                    adaptive=TRUE,dMat=model_dist_matrix)

### Model Training 
box4_model <- gwr.basic(gwr4_formula, data =model_data_sp, bw = bw4_box, kernel = "boxcar", adaptive = T, longlat = T, dMat = model_dist_matrix)
bisq4_model <- gwr.basic(gwr4_formula, data =model_data_sp, bw = bw4_bisqu, kernel = "bisquare", adaptive = T, longlat = T, dMat = model_dist_matrix)
gas4_model <- gwr.basic(gwr4_formula, data =model_data_sp, bw = bw4_gas, kernel = "gaussian", adaptive = T, longlat = T, dMat = model_dist_matrix)
expon4_model <- gwr.basic(gwr4_formula, data =model_data_sp, bw = bw4_expon, kernel = "exponential", adaptive = T, longlat = T, dMat = model_dist_matrix)

gwr4_metrics <- as.data.frame(rbind(c('boxcar', bw4_box, box4_model$GW.diagnostic$gwR2.adj, box4_model$GW.diagnostic$AICc, box4_model$GW.diagnostic$AIC), 
                                    c('bisq', bw4_bisqu, bisq4_model$GW.diagnostic$gwR2.adj, bisq4_model$GW.diagnostic$AICc,bisq4_model$GW.diagnostic$AIC), 
                                    c('gas', bw4_gas, gas4_model$GW.diagnostic$gwR2.adj, gas4_model$GW.diagnostic$AICc,gas4_model$GW.diagnostic$AIC), 
                                    c('exp', bw4_expon, expon4_model$GW.diagnostic$gwR2.adj, expon4_model$GW.diagnostic$AICc,expon4_model$GW.diagnostic$AIC))) %>% 
  setNames(c('Kernel', 'bandwidth','Adj r2', 'AICc', 'AIC')) 
gwr4_metrics

box4_diag <- my_gwr.collin.diagno(gwr4_formula, bw4_box, 'boxcar')
bisq4_diag <- my_gwr.collin.diagno(gwr4_formula, bw4_bisqu, 'bisquare')
gas4_diag <- my_gwr.collin.diagno(gwr4_formula, bw4_gas, 'gaussian')
exp4_diag <- my_gwr.collin.diagno(gwr4_formula, bw4_expon, 'exponential')

summary(box4_diag$SDF$local_CN)
summary(bisq4_diag$SDF$local_CN)
summary(gas4_diag$SDF$local_CN)
summary(exp4_diag$SDF$local_CN)

# VIF values
bisq4_vif_values <- c()
for(i in 1:4){
  nameb= names(bisq4_diag$SDF)[i]
  countb = sum(bisq4_diag$SDF[[i]] > 10)
  bisq4_vif_values <- rbind(bisq4_vif_values, c(nameb, countb))
}
bisq4_vif_values
bisq4_cn <- sum(bisq2_diag$SDF[[5]] > 40)


## GWR 4 in Limerick City 
city_data <- model_data[model_data$municipality=='Limerick City and Suburbs',]
model_data_city_sp <- SpatialPointsDataFrame(cbind(city_data$longitude, city_data$latitude), city_data)
model_dist_city_matrix <- gw.dist(dp.locat=cbind(model_data_city_sp$longitude, model_data_city_sp$latitude), longlat = T)



### GWR 1cty - All Variables 
### Model Calibration - Bandwidth AIC Approach 
bw4cty_box <- bw.gwr(gwr3_formula,adaptive = TRUE,
                     data=model_data_city_sp, dMat = model_dist_city_matrix)
bw4cty_bisqu <- bw.gwr(gwr3_formula,
                       data=model_data_city_sp, approach="CV", kernel="bisquare",
                       adaptive=TRUE,dMat=model_dist_city_matrix)
bw4cty_gas <- bw.gwr(gwr3_formula,
                     data=model_data_city_sp, approach="CV", kernel="gaussian",
                     adaptive=TRUE,dMat=model_dist_city_matrix)
bw4cty_expon <- bw.gwr(gwr3_formula,
                       data=model_data_city_sp, approach="CV", kernel="exponential",
                       adaptive=TRUE,dMat=model_dist_city_matrix)

### Model Training 
box4cty_model <- gwr.basic(gwr3_formula, data =model_data_city_sp, bw = bw4cty_box, kernel = "boxcar", adaptive = T, longlat = T, dMat = model_dist_city_matrix)
bisq4cty_model <- gwr.basic(gwr3_formula, data =model_data_city_sp, bw = bw4cty_bisqu, kernel = "bisquare", adaptive = T, longlat = T, dMat = model_dist_city_matrix)
gas4cty_model <- gwr.basic(gwr3_formula, data =model_data_city_sp, bw = bw4cty_gas, kernel = "gaussian", adaptive = T, longlat = T, dMat = model_dist_city_matrix)
expon4cty_model <- gwr.basic(gwr3_formula, data =model_data_city_sp, bw = 200, kernel = "exponential", adaptive = T, longlat = T, dMat = model_dist_city_matrix)

gwr4cty_metrics <- as.data.frame(rbind(c('boxcar', bw4cty_box, box4cty_model$GW.diagnostic$gwR2.adj, box4cty_model$GW.diagnostic$AICc, box4cty_model$GW.diagnostic$AIC), 
                                       c('bisq', bw4cty_bisqu, bisq4cty_model$GW.diagnostic$gwR2.adj, bisq4cty_model$GW.diagnostic$AICc,bisq4cty_model$GW.diagnostic$AIC), 
                                       c('gas', bw4cty_gas, gas4cty_model$GW.diagnostic$gwR2.adj, gas4cty_model$GW.diagnostic$AICc,gas4cty_model$GW.diagnostic$AIC), 
                                       c('exp', bw4cty_expon, expon4cty_model$GW.diagnostic$gwR2.adj, expon4cty_model$GW.diagnostic$AICc,expon4cty_model$GW.diagnostic$AIC))) %>% 
  setNames(c('Kernel', 'bandwidth','Adj r2', 'AICc', 'AIC')) 
gwr4cty_metrics

box4cty_diag <- my_gwr.collin.diagno(gwr3_formula, bw4cty_box, 'boxcar')
bisq4cty_diag <- my_gwr.collin.diagno(gwr3_formula, bw4cty_bisqu, 'bisquare')
gas4cty_diag <- my_gwr.collin.diagno(gwr3_formula, bw4cty_gas, 'gaussian')
exp4cty_diag <- my_gwr.collin.diagno(gwr3_formula,bw4cty_expon , 'exponential')

summary(box4cty_diag$SDF$local_CN)
summary(bisq4cty_diag$SDF$local_CN)
summary(gas4cty_diag$SDF$local_CN)
summary(exp4cty_diag$SDF$local_CN)

# VIF values
bisq4_vif_values <- c()
for(i in 1:3){
  nameb= names(bisq4cty_diag$SDF)[i]
  countb = sum(bisq4cty_diag$SDF[[i]] > 10)
  bisq4_vif_values <- rbind(bisq4_vif_values, c(nameb, countb))
}

bisq4_vif_values
bisq4_cn <- sum(bisq4cty_diag$SDF[[5]] > 30)






#### Predicitng Property Price- Heatmap -  with GWR-3

# creating a spatial grid in Liemrick county 
limerick_county_grid_df <- limerick_county_border %>% 
  st_make_grid(cellsize = 0.1, what = "centers") %>% 
  st_intersection(limerick_county_border)                     

# add 3 bed 2 bath 2017 proeprtied with fireplaces
limerick_county_grid_df <- as.data.frame(limerick_county_grid_df) %>% mutate(baths_c = 0, beds_c = 0, year.f = 2017, fireplace.f = 1)

# extract longitude and latitude from the data 
longitude <- numeric(nrow(limerick_county_grid_df))
latitude <- numeric(nrow(limerick_county_grid_df))
for (i in 1:nrow(limerick_county_grid_df)) {
  longitude[i] <- st_coordinates(limerick_county_grid_df$geometry[i])[1]
  latitude[i] <- st_coordinates(limerick_county_grid_df$geometry[i])[2]
}
limerick_county_grid_df$longitude <- longitude
limerick_county_grid_df$latitude <- latitude

# convert datafrome to grid 
limerick_county_grid_sf <- st_as_sf(limerick_county_grid_df)
limerick_county_grid_df <- dplyr::select(limerick_county_grid_df, -c('geometry'))

# gwr predictions on the grid 
gwr_pred_formula <- price_log ~ baths_c + beds_c + year.f + fireplace.f

test_county <- limerick_county_grid_df
train_county <- model_data
write.table(test_county, "./Train+Test/test_county.csv", col.names = TRUE, row.names = FALSE,
              sep = ",")
write.table(train_county, "./Train+Test/train_county.csv", col.names = TRUE, row.names = FALSE,
              sep = ",")
calib_county <- read.table("./Train+Test/train_county.csv", header = TRUE, sep = ",")
calib_county <- dplyr::select(calib_county, c('price_log', 'beds_c', 'baths_c', 'fireplace.f', 'year.f', 'longitude', 'latitude'))
calib_county.spdf <- SpatialPointsDataFrame(calib_county[, 6:7],as.data.frame(calib_county))
calib_county.spdf$fireplace.f <- ifelse(calib_county.spdf$fireplace.f==TRUE, 1, -1)
dm.calib_county <- gw.dist(dp.locat = coordinates(calib_county.spdf))
gwr.bw.cv <- bw.gwr(price_log ~ baths_c + beds_c + fireplace.f + year.f,data = calib_county.spdf,
                      approach = "CV", kernel = "bisquare", adaptive = TRUE, dMat = dm.calib_county)
valid_county <- read.table("./Train+Test/test_county.csv", header = TRUE, sep = ",")
valid_county.spdf <- SpatialPointsDataFrame(valid_county[, 5:6],
                                            as.data.frame(valid_county))
dm.valid <- gw.dist(dp.locat = coordinates(valid_county.spdf),
                      rp.locat = coordinates(calib_county.spdf), longlat = T)
gwr.pred <- gwr.predict(gwr_pred_formula,data = calib_county.spdf,
                          predictdata = valid_county.spdf ,bw = gwr.bw.cv, kernel = "bisquare",
                          adaptive = TRUE, dMat1 = dm.valid, dMat2 = dm.calib_county, longlat = T)

# predicted values 
predicted <- gwr.pred$SDF$prediction
fitted_price <- exp(gwr.pred$SDF$prediction)
variance <- gwr.pred$SDF$prediction_var

# add predicted price to the grid 
limerick_county_grid_sf$predicted_price <- fitted_price

map_urban_area +  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(data = limerick_county_grid_sf, aes(color = fitted_price), size =1)

xy_prediction_points <- data.frame(longitude = limerick_county_grid_df$longitude, 
                                   latitude = limerick_county_grid_df$latitude, 
                                   price = limerick_county_grid_sf$predicted_price)

# ggplot()+
#   geom_point(data = data_mapping, aes(x = longitude, y=latitude, colour = price))+
#   scale_color_gradientn(colours = topo.colors(20))+
#   coord_fixed()



# Interpolate the whole spatial area based on the values 
# get bounding box of polygon and estimate the grd
grd_extent <- st_bbox(limerick_municipalities_sf)
x.range <- as.numeric(c(grd_extent[1], grd_extent[3])) # min/max longitude of the interpolation area
y.range <- as.numeric(c(grd_extent[2], grd_extent[4])) # min/max latitude of the interpolation area
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.0001),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.0001)) # expand points to grid
coordinates(grd) <- ~x + y 
gridded(grd) <- TRUE

# turn our predicted values into a shapefile 
prediction_price_sf <- st_as_sf(xy_prediction_points, coords= c("longitude", "latitude"), crs = 4326)
prediction_price_sp <- as_Spatial(prediction_price_sf)
# turn the polygon into spatial points 
limerick_municipalities_sp <- as_Spatial(limerick_municipalities_sf)

# using inverse distance weighting to interpolate points between the prediction points 
idw_grid <- spsample(limerick_municipalities_sp, type = "regular", n = 5000000) 

# interpolate the values to create a grid 
interpolated_pred_price <- gstat::idw(price~1, prediction_price_sp, newdata = idw_grid, idp = 2)

interpolated_pred_price_df <- as.data.frame(interpolated_pred_price)

# rslt_grd_df <- rslt_df2 %>% filter(!is.na(var1.pred))

# Convert the spatial points to raster 
raster_pred_price <- interpolated_pred_price_df %>% 
  dplyr::select(-var1.var) %>% 
  raster::rasterFromXYZ(crs = 4326)
class(raster_pred_price)



# plot the raster of predicted price using leaflet 
# municipal labels 
limerick_sp <- as_Spatial(limerick_sf)
centers <- data.frame(centroid(limerick_sp), byid = TRUE)
centers$region <- limerick_sf$municipality_label





#create pal function for coloring the raster
palRaster <- colorNumeric('Spectral', reverse = TRUE, domain = interpolated_pred_price_df$var1.pred, na.color = "#00000000")
## Leaflet map with raster
leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>% addTiles() %>% 
  addRasterImage(raster_pred_price, 
                 color = palRaster, maxBytes = 8 * 1024 * 1024,
                 opacity = .6) %>%
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = ~region,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
  addLegend('topleft', pal = palRaster,
               values = interpolated_pred_price_df$var1.pred,
               title = "Price (€)"
  ) %>% 
  addPolygons(data = limerick_sf, color = "black", fillOpacity = 0.0, weight = 0.5)




































#### Spatial Lag model 
model_knn <- knearneigh(longlat_model, k=100, longlat = TRUE, use_kd_tree=FALSE)model_nb_list <- knn2nb(model_knn)
model_nb_listw <- nb2listw(model_nb_list)
model_nb_weights <- nb2listw(model_nb_list, style="W")


# define spatial weights for neighborhoods - listw object
meuse.lw <- nb2listw(meuse.nb, style = "W", zero.policy = TRUE)

# fit spatial lag moddel
zinc.slm <- lagsarlm(olsu_cbd_formula, data = model_data, listw = model_nb_listw, zero.policy = TRUE)
summary(zinc.slm)





