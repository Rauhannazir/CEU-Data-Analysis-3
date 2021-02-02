#############################
##     Data analysis 3     ##
##                         ##
##       Assignment I.     ##
##                         ##
##     Data preparation    ##
#############################


# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)

# data used
path <- "C://Users/MViki/Documents/CEU/Winter_semester/DA_3/Classes/Assignments/Assignment_1/"

# set data dir, load theme and functions
source(paste0(path, "da_helper_functions.R"))
source(paste0(path, "theme_bg.R"))

data_in <- paste0(path,"Data/Clean/")
data_out <- paste0(path,"Data/Clean/")
output <- paste0(path,"Output/")

options(digits = 3)


#-------------------------------------------------------
# Import data
df <- read_csv(paste(data_in,"airbnb_paris_cleaned.csv", sep = ""))


# FILTER DATA TO ACTUAL CASE ----------------------------------------------

# check for different property types
types <- df %>% group_by(property_type) %>% 
  summarise(number = n()) %>% 
  arrange(.,-number)
rm(types)

# keep if property type is Apartment
df <- df %>%
  filter(grepl("apartment", property_type )) %>% 
  filter(!grepl("serviced apartment", property_type))

# keep if accommodates 2-6 people
df <- df[df$accommodates >= 2 & df$accommodates <= 6,]



# CLEANE VARIABLES AND CREATE WORKFILE ------------------------------------------------
###############################
#### FACTORS
#
# Property type as factor
df %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

df$property_type <- word(df$property_type, -1) 

df <- df %>% 
  mutate( f_property_type = factor(property_type))


#Room type as factor
df %>% 
  group_by(room_type) %>% 
  summarise(cnt = n())


df <- df[df$room_type != "Hotel room",]

df <- df %>%
  mutate(f_room_type = factor(room_type))



# Rename roomt type because it is too long
df$f_room_type2 <- factor(ifelse(df$f_room_type== "Entire home/apt", "Entire/Apt",
                                 ifelse(df$f_room_type== "Private room", "Private",
                                        ifelse(df$f_room_type== "Shared room", "Shared", "."))))

# neighbourhood_cleansed as factors
unique(df$neighbourhood_cleansed)
df <- df %>%
  mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = c("Louvre", "Bourse", "Temple", "Hôtel-de-Ville", 
                                                                              "Panthéon", "Luxembourg", "Palais-Bourbon", "Élysée",
                                                                              "Opéra", "Entrepôt", "Popincourt", "Reuilly", 
                                                                              "Gobelins", "Observatoire", "Vaugirard", "Passy", 
                                                                              "Batignolles-Monceau", "Buttes-Montmartre","Buttes-Chaumont", "Ménilmontant")))

# get host_response_time as factors
df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more")))
                                                                           
                                                                                                                                                          
###############################
#### NUMERIC VARIABLES
#
## Create Numerical variables
df <- df %>%
  mutate( p_host_response_rate = as.numeric(host_response_rate),
          p_host_acceptance_rate = as.numeric(host_acceptance_rate),
          usd_price = price*1.2271) # exchange rate in 2020 December according to Banque De France

# clean number of bathrooms
df <- df %>% rename(bathrooms = bathrooms_text)
# get the number of baths from bathroom_text
df$bathrooms <- as.numeric(gsub("[^0-9.-]", "", gsub("half", 0.5, df$bathrooms, ignore.case = T)))

unique(df$bathrooms)

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "availability_365")
df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)


#create days since first review
df <- df %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

###############################
#### DUMMY VARIABLES
#
# create dummy vars
dummies <- c(names(df)[seq(49,105)],"host_is_superhost", "host_identity_verified" )
df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))



# CREATE WORK FILE --------------------------------------------------------

# keep columns if contain d_, n_, f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type)

# with price info only
df <- df %>%
  drop_na(price)

# rename price in USD to price and price to local price for simplicity 
df <- df %>% 
  rename(price = usd_price,
         local_price = price)

write_csv(df, paste0(data_out, "airbnb_paris_workfile.csv"))

library(skimr)



# CLEANING VALUES -------------------------------------------------------------------------

##################################
# DESCRIBE

# Property type
df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

#####################
### look at price ###
#####################
summary(df$price)
describe(df$price)

# filter out really high extreme values
df <- df %>%
  filter(price <500)

# create ln price
df <- df %>%
  mutate(ln_price = log(price))


################################################
# Look at some numeric key vars                #
################################################

############################
#### n_accommodates
df %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(df, aes(n_accommodates)) +
  geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of bathrooms") +
  theme_classic()

fig3_accommodates <- ggplot(df, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour="cyan4", shape=16, alpha = 0.6)+
  geom_smooth(method="lm", colour="darkmagenta", se=FALSE)+
  labs(x= "Number of people accomodated",y="Price")+
  scale_x_discrete( limits = c("1", "2","3","4","5","6", "7"))+
  theme_classic()
fig3_accommodates


#### I will take the square, log and log squared functional forms
#### no missing values



############################
## n_bathrooms
ggplot(df, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8) +
  xlab("N of bathrooms") +
  theme_classic()

df %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), n = n())

# check number of beds for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_baths = mean(n_bathrooms, na.rm = T), min_baths = min(n_bathrooms, na.rm = T), max_baths = max(n_bathrooms, na.rm = T))

#### I will create pooled categories -> 0, 1 and 2
#### then I will impute 1 for missing values as 88% of observations have 1 bathroom



############################
## n_bedrooms
ggplot(df, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of bedrooms") +
  theme_classic()

df %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of bedrooms for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_bedrooms = mean(n_bedrooms, na.rm = T), min_bedrooms = min(n_bedrooms, na.rm = T), max_bedrooms = max(n_beds, na.rm = T))

#### I will create pooled categories -> 1, 2 and 3
#### then I will impute 1 for observations accommodating 4 or less people, and 2 for those accommodating 5-6



############################
#### n_beds
ggplot(df, aes(n_beds)) +
  geom_histogram( fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of beds") +
  theme_classic()

df %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of beds for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_beds = mean(n_beds, na.rm = T), min_beds = min(n_beds, na.rm = T), max_beds = max(n_beds, na.rm = T))

#### I will take the log for n_bebs
#### then impute accommodates/2 rounded to whole for missing



############################
## n_review_scores_rating
ggplot(data = df, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour="cyan4", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_classic()

#### I will take the log of it



############################
## n_number_of_reviews
df %>%
  filter(n_number_of_reviews <200) %>% 
  ggplot(aes(n_number_of_reviews)) +
    geom_histogram(binwidth = 5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
    ylab("") +
    xlab("N of reviews") +
    theme_classic()


#### I will take the log of it
#### I will also create pools for reviews ->  none, 1-51 and >51 



############################
## n_minimum_nights
df %>% 
  group_by(n_minimum_nights) %>% 
  summarise(cnt = n())

ggplot(df, aes(n_minimum_nights)) +
  geom_histogram( fill = "cyan4", color = "white", alpha = 0.8, size = 0.25, binwidth = 1) +
  xlim(0,50)+
  xlab("N of minimum nights") +
  theme_classic()

#### I will create pooled categories -> 1, 2 and 3, 3+



############################
## n_days_since
skimr::skim(df$n_number_of_reviews)

ggplot(data = df, aes(x=n_number_of_reviews , y=ln_price)) +
  geom_point(size=1.5, colour="cyan4", shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_classic()

#### I will create ^2, ^3, ln, ln^2, ln^3



################################################
# Rename some dummy variables                  #
################################################

df <- df %>% rename(d_family_friendly = d_have_childrenbabycribhighcornerchang,
                    d_coffee_machine = d_have_o_machineee_machinecoffee,
                    d_free_parking_on_premises = d_have_freeon_premises,
                    d_free_parking_on_street = d_have_freestreet,
                    d_paid_parking_off_premises = d_have_paidoff_premisesselfparkingparking,
                    d_paid_parking_on_premises =  d_have_paidon_premisvalet,
                    d_wifi = d_have_wifiinternet, 
                    d_shampoo_conditioner = d_have_shampooconditioner,
                    d_balcony = d_have_balconyterrace) 



################################################
# Deal with missing values                     #
################################################

# where do we have missing variables now? (how much % of variables are missing)
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]


# 1. drop if no target (already did)
df <- df %>%
  drop_na(price)

# 2. drop columns when many missing not important
to_drop <- c( "p_host_response_rate", "f_host_response_time", "p_host_acceptance_rate")
df <- df %>%
  select(-one_of(to_drop))

# 3. imput when few, not that important
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

df <- df %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), 1, n_bathrooms), #assume 1 bath where it is missing
    n_bedrooms=ifelse(is.na(n_bedrooms), ifelse(n_accommodates<=4, 1, 2), n_bedrooms), 
    n_beds = ifelse(is.na(n_beds), round2(n_accommodates/2, 0), n_beds), #assume n_beds=n_accomodates/2 (mostly double beds)
    d_host_is_superhost = ifelse(is.na(d_host_is_superhost), 0, d_host_is_superhost),
    d_host_identity_verified = ifelse(is.na(d_host_identity_verified), 0, d_host_identity_verified)
  )

# 4. Replace missing variables re reviews with zero, when no review + add flags
df <- df %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )


# where do we have missing variables now?
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]
#### there are no missing values left


################################################
# Create pooled categories for variables       #
################################################

# Pool accommodations with 0,1,2,9 bathrooms
df <- df %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,9), labels=c(0,1,2), right = F) )

# Pool accomomdations with 1,2,3,9 bedrooms
df <- df %>%
  mutate(f_bedroom = cut(n_bedrooms, c(1,2,3,9), labels=c(1,2,3), right = F) )

# Pool num of reviews to 3 categories: none, 1-51 and >51
df <- df %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(df$n_number_of_reviews)), labels=c(0,1,2), right = F))

# Pool and categorize the number of minimum nights: 1,2,3, 3+
df <- df %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(df$n_minimum_nights)), labels=c(1,2,3), right = F))


# Change Infinite values with NaNs
for (j in 1:ncol(df) ) data.table::set(df, which(is.infinite(df[[j]])), j, NA)

to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# fill in missing values for pooled variables
df <- df %>%
  mutate(f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
         f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
         f_bedroom=ifelse(is.na(f_bedroom),1, f_bedroom),
         ln_price=ifelse(is.na(ln_price),1, ln_price))


################################################
# Create new functional forms                  #
################################################

# Squares and further values to create
df <- df %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_review_scores_rating = log(n_review_scores_rating),
         ln_number_of_reviews = log(n_number_of_reviews+1)
  )


# Create variables, measuring the time since: squared, cubic, logs

df <- df %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3))



###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type", "f_room_type")

for (i in 1:length(categoricals)) {
  df %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}



# SAVE ADJUSTED WORKFILE --------------------------------------------------

write_csv(df, paste0(data_out, "airbnb_paris_workfile_adj.csv"))








