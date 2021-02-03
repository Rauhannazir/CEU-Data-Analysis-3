#############################
##     Data analysis 3     ##
##                         ##
##       Assignment I.     ##
##                         ##
##      Data analysis      ##
#############################

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(rattle)
library(ranger)
library(Hmisc)
library(kableExtra)
library(ggcorrplot)


# set data dir, load theme and functions
path <- "C://Users/MViki/Documents/CEU/Winter_semester/DA_3/Classes/Assignment_1/"

source(paste0(path, "da_helper_functions.R"))
source(paste0(path, "theme_bg.R"))

# data used
data_in <- paste0(path, "Data/Clean/")
data_out <- paste0(path, "Data/Clean/")
output <- paste0(path,"Output/")


options(digits = 3)


##############################################################################################
# PART I.
##############################################################################################


#############
# Load data #
#############

data <-
  read_csv(paste0(data_in, "airbnb_paris_workfile_adj.csv")) %>%
  mutate_if(is.character, factor)

# our sample
skimr::skim(data)


count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}

count_missing_values(data)


# copy a variable - purpose later, see at variable importance
data <- data %>% mutate(n_accommodates_copy = n_accommodates)




#####################################
# Descriptive statistics            #
#####################################

# basic descr stat -------------------------------------------
skimr::skim(data)
summary(data$price)
Hmisc::describe(data$price)
describe(data$f_room_type)
describe(data$f_property_type)
table(data$f_number_of_reviews)

#How is the average price changing in my district by`room_type`?
data %>%
  group_by(f_property_type, f_room_type) %>%
  summarise(mean_price = mean(price))


# Histograms
#
#### PRICE
fig1_hist_price <- ggplot(data, aes(price)) +
  geom_histogram(aes(y = ..count../sum(..count..)),
                 binwidth = 25, fill = "cyan4", color = "white", alpha = 0.8) +
  scale_y_continuous(name = "Percent", labels=scales::percent) +
  xlab("Price (US dollars)") +
  theme_classic()
fig1_hist_price

ggsave(paste0(output, "Fig11_hist_price.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)


#### LNPRICE
fig2_hist_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(aes(y = ..count../sum(..count..)),
                 binwidth = 0.15, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  scale_y_continuous(name = "Percent", labels=scales::percent) +
  xlab("ln(price, US dollars)") +
  theme_classic()
fig2_hist_lnprice

ggsave(paste0(output, "Fig21_hist_lnprice.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

# Density chart BY ROOM TYPE 
fig3 <- ggplot(data = data, aes(x=price)) +
  geom_density(aes(color=f_room_type, fill=f_room_type),  na.rm =TRUE, alpha= 0.3) +
  labs(x="Price (US dollars)", y="Density", color = "") +
  scale_color_manual(name="",
                     values=c("green4","cyan4", "deeppink4"),
                     labels=c("Entire home/apt","Private room", "Shared room")) +
  scale_fill_manual(name="",
                    values=c("green4","cyan4", "deeppink4"),
                    labels=c("Entire home/apt","Private room", "Shared room")) +
  xlim(0,750)+
  theme_classic() 
fig3

ggsave(paste0(output, "Fig3_density_by_roomtype.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)


# Barchart  
fig4 <- ggplot(data = data, aes(x = factor(n_accommodates), color = f_room_type, fill = f_room_type)) +
  geom_bar(alpha=0.6, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c("green4","cyan4", "deeppink4")) +
  scale_fill_manual(name="",
                    values=c("green4","cyan4", "deeppink4")) +
  labs(x = "Accomodates (Persons)",y = "Frequency")+
  theme_classic() 
fig4
ggsave(paste0(output, "Fig4_barchart_price_accom_and_roomtype.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)


# Boxplots
#
####Price by room type
fig5_box_room <- ggplot(data = data, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = c("green4","cyan4", "deeppink4"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = c("green4","cyan4", "deeppink4"), fill = c("green4","cyan4", "deeppink4"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_classic()
fig5_box_room

ggsave(paste0(output,"Fig5_box_room"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

####Price by room type
fig6_box_accomodates <- ggplot(data, aes(x = factor(n_accommodates), y = price,
                        fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
    stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
    scale_color_manual(name="",
                     values=c("cyan4", "deeppink4")) +
  scale_fill_manual(name="",
                     values=c("cyan4", "deeppink4")) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  #scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_classic() +
  theme(legend.position = c(0.3,0.8)        )
fig6_box_accomodates 

ggsave(paste0(output,"Fig6_box_accomodates"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)



##############################################################################################
# PART II.
##############################################################################################

# create train and holdout samples -------------------------------------------

set.seed(2021)

train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_work <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_work)
dim(data_holdout)


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("f_room_type", "n_accommodates", "n_beds",  "n_days_since", "flag_days_since")

# Factorized variables
basic_add <- c("f_bathroom", "f_bedroom", "f_neighbourhood_cleansed", "n_minimum_nights", "n_availability_365")

reviews <- c("n_review_scores_rating", "flag_review_scores_rating",
             "n_number_of_reviews",
             "n_reviews_per_month", "flag_reviews_per_month")

host <- c("d_host_is_superhost", "d_host_identity_verified")

# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

# Dummy variables: Extras -> collect all options and create dummies
dummies <-  grep("^d_.*", names(data), value = TRUE)


# Define models: simpler, extended -----------------------------------------------------------

#################################
# Look for interactions         #
#################################

## This huge correlation table shows how strongly numeric variables are correlated
num_data <- data[,unlist(lapply(data, is.numeric))]  
num_data <- num_data %>%  select(matches("^d_.*|^n_.*|^f_.*|^p.*"), 
                                 -c('n_accommodates2', "n_days_since2", "n_days_since3", "n_accommodates_copy"))

corr <- round(cor(num_data), 1)
ggcorrplot(corr)

# Plot interactions between room type/property type and all dummies
sapply(dummies, function(x){
  p <- price_diff_by_variables2(data, "f_room_type", x, "Room type", x)
  print(p)
})


#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_room_type", "d_family_friendly", "Room type", "Family friendly")
p2 <- price_diff_by_variables2(data, "f_room_type", "d_elevator", "Room type", "Elevator")
p3 <- price_diff_by_variables2(data, "f_room_type", "d_have_kitchen", "Room type", "Kitchen")
p4 <- price_diff_by_variables2(data, "f_room_type", "d_have_heating", "Room type", "Heating")
p5 <- price_diff_by_variables2(data, "f_room_type", "d_have_breakfast", "Room type", "Breakfast")
p6 <- price_diff_by_variables2(data, "f_room_type", "d_have_washer", "Room type", "Washer")
p7 <- price_diff_by_variables2(data, "f_room_type", "d_wifi", "Room type", "Wifi")
p8 <- price_diff_by_variables2(data, "f_room_type", "d_have_tv", "Room type", "TV")
p9 <- price_diff_by_variables2(data, "f_room_type", "d_balcony", "Room type", "Balcony")
p10 <- price_diff_by_variables2(data, "f_room_type", "d_have_garden", "Room type", "Garden")
p11 <- price_diff_by_variables2(data, "f_room_type", "d_lockbox", "Room type", "Lockbox")
p12 <- price_diff_by_variables2(data, "f_room_type", "d_cleaning_before_checkout", "Room type", "Cleaning before checkout")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow=4, ncol=3)
g_interactions

#save_fig
save_fig("Fig7_interactions",output,"verylarge")


# Chosen interactions ----------------------------------------------------

## based on correlation plot
X1  <- c("n_accommodates*f_room_type", "n_accommodates*f_bedroom", "n_accommodates*f_bathroom", 
         "n_accommodates*n_beds", "f_bedroom*n_beds")

# with neighborhood and most important interactions
X2  <- c("f_room_type*f_neighbourhood_cleansed", "n_accommodates*f_neighbourhood_cleansed",
         "f_room_type*d_family_friendly",
         "f_room_type*d_elevator",
         "f_room_type*d_have_kitchen", 
         "f_room_type*d_have_breakfast",
         "f_room_type*d_have_washer",
         "f_room_type*d_wifi",
         "f_room_type*d_balcony",
         "f_room_type*d_have_garden",
         "f_room_type*d_lockbox",
         "f_room_type*d_cleaning_before_checkout")

# all dummies with property type, room type and bed type
X3  <- c("f_room_type*f_neighbourhood_cleansed", "n_accommodates*f_neighbourhood_cleansed",
         paste0("(f_room_type) * (",
                paste(dummies, collapse=" + "),")"))



# OLS models --------------------------------------------------------------

# Create models in levels models: 1-0
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev,basic_add),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host,poly_lev),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host,poly_lev,X1),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host,poly_lev,X1,X2),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host,poly_lev,dummies),collapse = " + "))
modellev9 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host,poly_lev,X1,X2,dummies),collapse = " + "))
modellev10 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host,poly_lev,X1,X3,dummies),collapse = " + "))



##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(2021)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:10)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")

  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))

  # Initialize values
  rmse_train <- c()
  rmse_test <- c()

  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared

  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)

    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)

  }

  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                 "Test RMSE")

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

OLS_models <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(OLS_models) <- column_names
print(xtable(OLS_models, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "OLS_table.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)



# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c("cyan4","deeppink4")) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-2), cex=0.6)) +
  labs(x = "Number of coefficients", y = "RMSE")+
  theme_classic()
model_result_plot_levels


save_fig("Fig8-model-result-levels", output, "small")

###########################
#### Model 9 gave the best results based on RMSE test, RMSE train and BIC as well



##############################################################################################
# PART II.
##############################################################################################

# BASED ON WHAT WE LEARNT FROM BUILDING OLS MODELS WE SET UP 4 PREDICTORS --------

predictors_1 <- c(basic_lev, basic_add, reviews, host)
predictors_2 <- c(basic_lev, basic_add, reviews, host, poly_lev, dummies)
predictors_E <- c(basic_lev, basic_add, reviews, host, poly_lev, dummies, X1,X2) # best OLS model basen on what we learnt until now


# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds, verboseIter = FALSE)

#################################
#           OLS                 #
#################################

set.seed(2021)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
    data = data_work,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))



#################################
#           LASSO               #
#################################

set.seed(2021)
system.time({
  lasso_model <- train(
    formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
    data = data_work,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
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

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_nz, by = "variable", all=TRUE)
regression_coeffs %>%
  write.csv(file = paste0(output, "regression_coeffs.csv"))


#################################
#            CART               #
#################################

set.seed(2021)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_work,
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
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(2021)
system.time({
  rf_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_work,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model


#################################
#             GBM               #
#################################

#### Basic GMB model
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(2021)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_work,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model



# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model$finalModel$mtry,
  rf_model$finalModel$min.node.size
),
nrow=1, ncol=2,
dimnames = list("Model A",
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_turning_choices.tex"))


saveRDS(ols_model, paste0(data_out, 'OLS.rds'))
saveRDS(lasso_model, paste0(data_out, 'lasso.rds'))
saveRDS(cart_model, paste0(data_out, 'cart.rds'))
saveRDS(rf_model, paste0(data_out,'random_forest.rds'))
saveRDS(gbm_model, paste0(data_out,'gbm.rds'))



#################################
#          FINAL MODELS         #
#################################
final_models <-
  list("OLS" = ols_model,
       "LASSO" = lasso_model,
       "CART" = cart_model,
       "Random forest"= rf_model,
       "GBM"  = gbm_model
       )

results <- resamples(final_models) %>% summary()


# Save output --------------------------------------------------------
# Model selection is carried out on this CV RMSE

result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

result_4

kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"final_models_cv_rmse.tex"))




# evaluate preferred model on the holdout set -----------------------------

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

result_5

kable(x = result_5, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"final_models_houldout_rmse.tex"))



###################################################
# Diagnsotics #
###################################################



#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


rf_model_var_imp <- importance(rf_model$finalModel)/1000
rf_model_var_imp_df <-
  data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# full varimp plot, top 10 only
##############################

rf_model_var_imp_plot_b <- ggplot(rf_model_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color ="cyan4", size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="cyan4", size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_var_imp_plot_b

save_fig("Fig9-rf-varimp-top10",output, "small")


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

rf_model_var_imp_grouped <- group.importance(rf_model$finalModel, groups)
rf_model_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_var_imp_grouped),
                                            imp = rf_model_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_var_imp_grouped_plot <-
  ggplot(rf_model_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color="cyan4", size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="cyan4", size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_var_imp_grouped_plot

save_fig("Fig10-rf-varimp-group",output, "small")



#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

pdp_n_acc <- pdp::partial(rf_model, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(color="cyan4", size=3) +
  geom_line(color="cyan4", size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_classic()
pdp_n_acc_plot

save_fig("Fig11-gbm-pdp-n-accom", output, "small")



pdp_n_roomtype <- pdp::partial(rf_model, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color="cyan4", size=3) +
  ylab("Predicted price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(60,130), breaks=seq(60,130, by=10)) +
  theme_classic()
pdp_n_roomtype_plot

save_fig("Fig12-gbm-pdp-roomtype", output, "small")




# Subsample performance: RMSE / mean(y) ---------------------------------------

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model, newdata = data_holdout))



######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  group_by(f_room_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Borough", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))


result_3

options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)




###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

Ylev <- data_holdout[["price"]]

# Predicted values
prediction_holdout_pred <- as.data.frame(predict(gbm_model, newdata = data_holdout, interval="predict")) 

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               prediction_holdout_pred)



# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,3] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = "cyan4", size = 1,
             shape = 16, alpha = 0.6, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.8, color="deeppink4", linetype=2) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_classic() 
level_vs_pred

save_fig("Fig13-level-vs-pred", output, "small")



