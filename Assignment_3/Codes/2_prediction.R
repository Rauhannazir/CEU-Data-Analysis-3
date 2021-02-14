#############################
##     Data analysis 3     ##
##                         ##
##     Assignment III.     ##
##                         ##
##       Prediction        ##
#############################


# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())

# Load packages
library(dplyr)
library(tidyverse)
library(stargazer)
library(lspline)
library(caret)
library(segmented) 

dir <- "C:/Users/MViki/Documents/CEU/Winter_semester/DA_3/Classes/Assignments/CEU-Data-Analysis-3/Assignment_3/"

source(paste0(dir,"Codes/da_helper_functions.R"))
source(paste0(dir,"Codes/theme_bg.R"))

#location folders
data_in <- paste0(dir,"/Data/Clean/")
data_out <- data_in
output <- paste0(dir,"Output/")

# load Vienna
data <- read_csv(paste0(data_in,"hotels-vienna.csv"))



####################################################################################
# Explanatory data analysis
####################################################################################

### Feature variables ----------------------------------------------------------------------------

# Accommodation type -> keep only Hotels
table(data$accommodation_type)

# Actual city -> keep only observation where the actual city is Vienna
table(data$city_actual)

# Number of stars -> keep hotels between 3-4 stars
ggplot(data = data ,aes(x=stars))+
  geom_bar(color = "gray25", fill = "cyan4", alpha=0.8, na.rm=T) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 2.5) +
  labs(x="Star rating (N. stars)", y="Frequency") +
  theme_bw() 

# Distance from city center
ggplot(data =  data, aes (x = distance)) +
  geom_histogram(color = "gray25", fill = "cyan4", alpha= 0.8 ,binwidth = 0.5)+
  labs(x = "Distance to city center (miles)", y = "Frequency") +
  geom_segment(aes(x = 8.2, y = 0, xend = 8.2, yend = 90), color = "deeppink4", alpha = 0.6, size=1) +
  annotate("text", x = 11, y = 29, label = "Too far out", size=2)+
  annotate("rect",xmin=8.2, xmax=14, ymin=0, ymax=90, fill="deeppink4", alpha=0.1)+
  theme_bw() 

###############################################
# Filter out observations
###############################################
data <- data %>% 
  filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>% 
  filter(distance <= 8)



### Price ------------------------------------------------------------------------------------------------------

# Price -> drop high prices above 300 USD
ggplot(data =  data, aes (x = price)) +
  geom_histogram(color = "gray25", fill = "cyan4",alpha = 0.8, binwidth = 20)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  geom_segment(aes(x = 300, y = 0, xend = 300, yend = 100), color = "deeppink4", alpha = 0.6, size=1) +
  annotate("text", x = 450, y = 30, label = "Too expensive", size=2)+
  annotate("rect",xmin=300, xmax=1100, ymin=0, ymax=100, fill="deeppink4", alpha=0.1)+
  theme_bw()


# Apply filters
data <- data %>% 
  filter(price<=300) 

###############################################
# Find variables with no variation
###############################################

for (i in colnames(data)){
  print(i)
  print(nrow(unique(data[,i])))
}

# Delete columns which has the same value for all observations
data$country <- NULL
data$city_actual <- NULL
data$center1label <- NULL
data$center2label <- NULL
data$city <- NULL
data$year <- NULL
data$month <- NULL
data$weekend <- NULL
data$holiday <- NULL
data$accommodation_type <- NULL
data$nnights <- NULL


###############################################
# Missing values
###############################################

# Missing values
count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}
count_missing_values(data)


# deal with missing values in TripAdvisor ratings and ratings count ---------------------------------------------
#### RATINGTA - Inpute 4 for the missing values
mean(data$ratingta, na.rm = T) # 3.97 
median(data$ratingta, na.rm = T) # 4

data %>%
  ggplot(aes(ratingta)) +
  geom_histogram(fill = "cyan4", color = "black", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bw()

#### RATINGTA COUNT - impute mean of all values
data %>%
  ggplot(aes(ratingta_count)) +
  geom_histogram(fill = "cyan4", color = "black", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bw()

# Impute chosen values and flag them!
data <- data %>%
  mutate(
    ratingta =  ifelse(is.na(ratingta), 4, ratingta),
    flag_ratingta=ifelse(is.na(ratingta),1, 0),
    ratingta_count =  ifelse(is.na(ratingta_count), mean(ratingta_count, na.rm = T), ratingta_count),
    flag_ratingta_count=ifelse(is.na(ratingta_count),1, 0))

###########################################################################
# Create factors and binary indicators for some variables
###########################################################################

# Stars: binary indicators - for regression in book's case study
data$star35 = ifelse(data$stars==3.5, 1, 0)
data$star4 = ifelse(data$stars==4, 1, 0)

# Transform neighborhood to factor
data$neighbourhood_f <- as.factor(data$neighbourhood)

# Offer to factor
data$offer_cat_f<- as.factor(data$offer_cat)

# Number of start to factor - 3 different values
data$stars_f <- as.factor(data$stars)

# ratingta to factor - 5 different values
data$ratingta_f <- as.factor(data$ratingta)



#####################################################################
# Look at potential functional forms
#####################################################################

# Y variable --------------------------------------------------------------------------------

### PRICE - skewed with long right tail
ggplot(data =  data, aes (x = price, y = ..density..)) +
  geom_histogram(color = "gray25", fill = "cyan4",alpha = 0.8)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  theme_bw()

### LNPRICE - closer  to normal distribution
ggplot(data =  data, aes (x = log(price), y = ..density..)) +
  geom_histogram(color = "gray25", fill = "cyan4",alpha = 0.8)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  theme_bw()

# Take the log for price
data$lnprice <- log(data$price)


# Feature variables --------------------------------------------------------------------------

### Distance 
ggplot(data = data, aes(x = distance, y = log(price))) +
  geom_point(color = "grey25") + 
  geom_smooth(method = "loess", color = "cyan4")+
  labs(x = "Distance to city center (miles)",y = "ln(price, US dollars)")+
  theme_bw()

# find the best splits for piecewise
lpm_dist <- lm(log(price) ~ distance, data = data)
fit_seg_1 <- segmented( lpm_dist , seg.Z = ~distance, psi = list( distance=c(1,3,4)) )
summary(fit_seg_1)


### Rating
ggplot(data = data, aes(x = rating, y = log(price))) +
  geom_point(color = "grey25") + 
  geom_smooth(method = "loess", color = "cyan4")+
  labs(x = "Rating",y = "ln(price, US dollars)")+
  theme_bw()

# find the best splits for piecewise
lpm_rating <- lm(log(price) ~ rating, data = data)
fit_seg_2 <- segmented( lpm_rating , seg.Z = ~rating, psi = list( rating=c(3.5, 4.5)) )
summary(fit_seg_2)


### Number of stars
ggplot(data = data, aes(x = stars_f, y = price)) +
  stat_boxplot(aes(group = stars_f), geom = "errorbar", width = 0.3,
               color = c("green4","cyan4", "deeppink4"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = stars_f),
               color = c("green4","cyan4", "deeppink4"), fill = c("green4","cyan4", "deeppink4"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Number of stars",y = "Price (US dollars)")+
  theme_bw()


### TripAdvisor rating
ggplot(data = data, aes(x = ratingta_f, y = price)) +
  stat_boxplot(aes(group = ratingta_f), geom = "errorbar", width = 0.3,
               color = c("green4","cyan4", "purple4", "deeppink4", "darkorange2"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = ratingta_f),
               color = c("green4","cyan4", "purple4", "deeppink4", "darkorange2"), 
               fill = c("green4","cyan4", "purple4", "deeppink4", "darkorange2"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Number of stars",y = "Price (US dollars)")+
  theme_bw()


### Scarce room - does not really have an effect
ggplot(data = data, aes(x = scarce_room, y = price)) +
  stat_boxplot(aes(group = scarce_room), geom = "errorbar", width = 0.3,
               color = c("cyan4", "deeppink4"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = scarce_room),
               color = c("cyan4", "deeppink4"), 
               fill = c("cyan4", "deeppink4"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Number of stars",y = "Price (US dollars)")+
  theme_bw()


### Offer
ggplot(data = data, aes(x = offer, y = price)) +
  stat_boxplot(aes(group = offer), geom = "errorbar", width = 0.3,
               color = c("cyan4", "deeppink4"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = offer),
               color = c("cyan4", "deeppink4"), 
               fill = c("cyan4", "deeppink4"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Number of stars",y = "Price (US dollars)")+
  theme_bw()


### Offer category
ggplot(data = data, aes(x = offer_cat_f, y = price)) +
  stat_boxplot(aes(group = offer_cat_f), geom = "errorbar", width = 0.3,
               color = c("green4","cyan4", "deeppink4", "darkorange2"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = offer_cat_f),
               color = c("green4","cyan4", "deeppink4", "darkorange2"), 
               fill = c("green4","cyan4", "deeppink4", "darkorange2"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Number of stars",y = "Price (US dollars)")+
  theme_bw()

### Neighborhood
ggplot(data = data, aes(x = neighbourhood_f, y = price)) +
  stat_boxplot(aes(group = neighbourhood_f), geom = "errorbar", width = 0.3,
               color = "cyan4", size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = neighbourhood_f),
               color = "cyan4", 
               fill = "cyan4",
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Number of stars",y = "Price (US dollars)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






#####################################################################
# Regressions
#####################################################################
# Basic reg from book
reg0 <- lm(lnprice ~ lspline(distance, c(1,4)) + lspline(rating, 3.5) + star35 + star4, data=data)

summary(reg0)



# Variables
basic <- c("lspline(distance, c(1.1, 2.9, 4.3))",  "lspline(rating, c(3.2, 3.8))", "stars_f")
ratings <- c("rating_count", "ratingta_f", "ratingta_count", "flag_ratingta", "flag_ratingta_count")
extra <- c("scarce_room", "offer", "offer_cat_f", "neighbourhood_f")


pred_1 <- c(basic)
pred_2 <- c(basic, ratings)
pred_3 <- c(basic, ratings, extra)
pred_rf <- c("distance", "rating", "stars_f", ratings, extra)

preds <- list("pred_1" = pred_1, "pred_2" = pred_2, "pred_3" = pred_3)


train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)



set.seed(2021)
system.time({
  simple_reg <- train(
    formula(lnprice ~ lspline(distance, c(1,4)) + lspline(rating, 3.5) + star35 + star4),
    data = data,
    method = "lm",
    trControl = train_control
  )
})


#################################
#           OLS                 #
#################################

ols_model_coeffs <- list()
ols_models <- list()

for (model in names(preds)) {
  
  features <- preds[[model]]
  
  set.seed(2021)
  system.time({
    ols_model <- train(
      formula(paste0("lnprice ~", paste0(features, collapse = " + "))),
      data = data,
      method = "lm",
      trControl = train_control
    )
  })
  
  ols_model_coeffs[[model]] <-  ols_model$finalModel$coefficients
  ols_models[[model]]<- ols_model
}


ols_models

  

#################################
#           LASSO               #
#################################

set.seed(2021)
system.time({
  lasso_model <- train(
    formula(paste0("lnprice ~", paste0(pred_3, collapse = " + "))),
    data = data,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 10, by = 0.01)),
    trControl = train_control
  )
})

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # the column has a name "1", to be renamed

lasso_coeffs_nz <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

print(nrow(lasso_coeffs_nz))


# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])




#################################
#            CART               #
#################################

set.seed(2021)
system.time({
  cart_model <- train(
    formula(paste0("lnprice ~", paste0(pred_rf, collapse = " + "))),
    data = data,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")

#################################
#       Random forest           #
#################################

# set tuning 
tune_grid <- expand.grid(
  .mtry = c(1:15),
  .splitrule = "variance",
  .min.node.size = c(1:15)
)

set.seed(2021)
system.time({
  rf_model <- train(
    formula(paste0("lnprice ~", paste0(pred_rf, collapse = " + "))),
    data = data,
    method = "ranger",
    tuneGrid = tune_grid,
    importance = "impurity",
    trControl = train_control
  )
})

rf_model


# auto tuning first
set.seed(2021)
system.time({
  rf_model_auto <- train(
    formula(paste0("lnprice ~", paste0(pred_rf, collapse = " + "))),
    data = data,
    method = "ranger",
    importance = "impurity",
    trControl = train_control
  )
})
rf_model_auto



#####################################################################
# SUMMARY
#####################################################################

final_models <-
  list("Simple reg" = simple_reg,
       "OLS 1" = ols_models$pred_1,
       "OLS 2" = ols_models$pred_2,
       "OLS 3" = ols_models$pred_3,
       "LASSO" = lasso_model,
       "CART" = cart_model,
       "Random forest"= rf_model,
       "Random forest auto" = rf_model_auto
  )

results <- resamples(final_models) %>% summary()

result_RMSE <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")


result_Rsqr <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~Rsquared")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV R squared" = ".")



comparison <- cbind(result_RMSE, result_Rsqr)
comparison


#####################################################################
# Best deals
#####################################################################

# Simple reg residuals
data$lnprice_hat <- predict(reg0)
data$lnprice_resid <- data$lnprice - data$lnprice_hat
data$bestdeals <- ifelse(data$lnprice_resid %in% tail(sort(data$lnprice_resid, decreasing=TRUE),5),TRUE,FALSE)

data %>%
  select(hotel_id, price, lnprice_resid, distance, stars, rating) %>%
  arrange(lnprice_resid) %>%
  .[1:5,] %>%
  as.data.frame() 



# LASSO residuals
data$lnprice_pred_lasso <- predict(lasso_model, newdata = data)
data$lasso_resid <- data$lnprice - data$lnprice_pred_lasso
data$bestdeals_lasso <- ifelse(data$lasso_resid %in% tail(sort(data$lasso_resid, decreasing=TRUE),5),TRUE,FALSE)

StDev_lasso <- sd(data$lasso_resid)
data$price_pred_lasso <- exp(data$lnprice_pred_lasso) * exp((StDev_lasso^2)/2)

data %>%
  select(hotel_id, price, lasso_resid, distance, stars, rating) %>%
  arrange(lasso_resid) %>%
  .[1:10,] %>%
  as.data.frame() 

data$lasso_resid_2 <- data$price - data$price_pred_lasso



# CART residuals
data$lnprice_pred_cart <- predict(cart_model, newdata = data)
data$cart_resid <- data$lnprice - data$lnprice_pred_cart
data$bestdeals_cart <- ifelse(data$cart_resid %in% tail(sort(data$cart_resid, decreasing=TRUE),5),TRUE,FALSE)

StDev_cart <- sd(data$cart_resid)
data$price_pred_cart <- exp(data$lnprice_pred_cart) * exp((StDev_cart^2)/2)

data %>%
  select(hotel_id, price, cart_resid, distance, stars, rating) %>%
  arrange(cart_resid) %>%
  .[1:10,] %>%
  as.data.frame() 

data$cart_resid_2 <- data$price - data$price_pred_cart



# Random forests residuals
data$lnprice_pred_rf <- predict(rf_model, newdata = data)
data$rf_resid <- data$lnprice - data$lnprice_pred_rf 
data$bestdeals_rf <- ifelse(data$rf_resid %in% tail(sort(data$rf_resid, decreasing=TRUE),5),TRUE,FALSE)

StDev_rf <- sd(data$rf_resid)
data$price_pred_rf <- exp(data$lnprice_pred_rf) * exp((StDev_rf^2)/2)

data %>%
  select(hotel_id, price, rf_resid, distance, stars, rating) %>%
  arrange(rf_resid) %>%
  .[1:10,] %>%
  as.data.frame() 



### GRAPHS --------------------------------------------------------------------------------------------------------------------------------------

# Best deals LASSO lnprice
ggplot(data = data, aes(x = lnprice_pred_lasso, y = lnprice)) +
  geom_point(aes(color=bestdeals_lasso,shape=bestdeals_lasso), size = 1.5, fill="deeppink4", alpha = 0.8,  show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 3.8, y = 3.8, xend = 6, yend =6), size=0.8, color="cyan4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  scale_colour_manual(name='',values=c("grey25",'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
  geom_segment(aes(x = 4.8, y = 3.9, xend = 4.68, yend = 4.05), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 4.93, y = 3.9, label = "Best deal", size=2.5)+
  theme_bw()

# Best deals LASSO price
ggplot(data = data, aes(x = price_pred_lasso, y = price)) +
  geom_point(aes(color=bestdeals_lasso,shape=bestdeals_lasso), size = 1.5, fill="deeppink4", alpha = 0.8,  show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 3.8, y = 3.8, xend = 250, yend =250), size=0.8, color="cyan4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  scale_colour_manual(name='',values=c("grey25",'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
   theme_bw()


# Best deals cart
ggplot(data = data, aes(x = lnprice_pred_cart, y = lnprice)) +
  geom_point(aes(color=bestdeals_cart,shape=bestdeals_cart), size = 1.5, fill="deeppink4", alpha = 0.8,  show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 3.8, y = 3.8, xend = 6, yend =6), size=0.8, color="cyan4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  scale_colour_manual(name='',values=c("grey25",'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
  geom_segment(aes(x = 4.8, y = 3.9, xend = 4.68, yend = 4.05), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 4.93, y = 3.9, label = "Best deal", size=2.5)+
  theme_bw()

# Best deals cart price
ggplot(data = data, aes(x = price_pred_rf, y = price)) +
  geom_point(aes(color=bestdeals_rf,shape=bestdeals_rf), size = 1.5, fill="deeppink4", alpha = 0.8,  show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 3.8, y = 3.8, xend = 250, yend =250), size=0.8, color="cyan4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  scale_colour_manual(name='',values=c("grey25",'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
  theme_bw()


# Best deals random forest
ggplot(data = data, aes(x = lnprice_pred_rf, y = lnprice)) +
  geom_point(aes(color=bestdeals_rf,shape=bestdeals_rf), size = 1.5, fill="deeppink4", alpha = 0.8,  show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 3.8, y = 3.8, xend = 6, yend =6), size=0.8, color="cyan4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  scale_colour_manual(name='',values=c("grey25",'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
  geom_segment(aes(x = 4.8, y = 3.9, xend = 4.68, yend = 4.05), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 4.93, y = 3.9, label = "Best deal", size=2.5)+
  theme_bw()

# Best deals rf price
ggplot(data = data, aes(x = price_pred_rf, y = price)) +
  geom_point(aes(color=bestdeals_rf,shape=bestdeals_rf), size = 1.5, fill="deeppink4", alpha = 0.8,  show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 3.8, y = 3.8, xend = 250, yend =250), size=0.8, color="cyan4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  scale_colour_manual(name='',values=c("grey25",'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
  theme_bw()



### Model powers

data %>% 
  dplyr::summarise(
    rmse = RMSE(price_pred_lasso, price),
    mean_price = mean(price),
    rmse_norm = RMSE(price_pred_lasso, price) / mean(price))


data %>% 
  dplyr::summarise(
    rmse = RMSE(price_pred_rf, price),
    mean_price = mean(price),
    rmse_norm = RMSE(price_pred_rf, price) / mean(price))



   








