#############################
##     Data analysis 3     ##
##                         ##
##     Assignment III.     ##
##                         ##
##      Data cleaning      ##
#############################


# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())


# packages
library(dplyr)
library(tidyverse)


dir <- "C:/Users/MViki/Documents/CEU/Winter_semester/DA_3/Classes/Assignments/CEU-Data-Analysis-3/Assignment_3/"

#location folders
data_in <- paste0(dir,"/Data/Raw/")
data_out <- paste0(dir,"/Data/Clean/")


# Load the data
df <- read.csv(paste0(data_in,"hotelbookingdata.csv"), stringsAsFactors = F)


# filter to a chosen date
df <- df[df$year == 2017 & df$month==11 & df$weekend==0, ]

hotels_by_city <- df %>% 
  group_by(s_city) %>% 
  summarise("num_hotels" = n()) %>% 
  arrange(-num_hotels)

df <- df[df$s_city == "Vienna",]

# save hotels Vienna data
write.csv(df, paste0(data_in,"hotelbookingdata-vienna.csv"), row.names= F)

df <- read.csv(paste0(data_in,"hotelbookingdata-vienna.csv"), stringsAsFactors = F)

#####################################################
##                                                 ##
##              I. CLEANING DATA                   ##
##                                                 ##
#####################################################

# Distance to center 
#### delete "miles" and turn to numeric)
df$distance <- as.numeric(gsub("[^0-9\\.]","",df$center1distance))
df$distance_alter <- as.numeric(gsub("[^0-9\\.]","",df$center2distance))


# Accommodationtype 
#### delete _ACCOM_TYPE@ before the actual accommodations 
df$accommodation_type <- unlist(sapply(strsplit(as.character(df$accommodationtype), "@"), '[[', 2))
df$accommodationtype <- NULL

# Number of nights 
#### replace "price for 1 night" string with 1
df$nnights <- 1

# Ratings
#### remove /5 and generate numerical variable 
df$rating <- as.numeric(gsub("/5","",df$guestreviewsrating))


# check: frequency table of all values
table(df$rating)


# RENAME VARIABLES
colnames(df)[colnames(df)=="rating_reviewcount"] <- "rating_count"
colnames(df)[colnames(df)=="rating2_ta"] <- "ratingta"
colnames(df)[colnames(df)=="rating2_ta_reviewcount"] <- "ratingta_count"
colnames(df)[colnames(df)=="addresscountryname"] <- "country"
colnames(df)[colnames(df)=="s_city"] <- "city"
colnames(df)[colnames(df)=="starrating"] <- "stars"

# look at key vars
table(df$stars)

table(df$rating)


# drop vars
df$center2distance <-  NULL
df$center1distance <-  NULL
df$price_night <- NULL
df$guestreviewsrating <- NULL


# DROP PERFECT DUPLICATES
df[duplicated(df)==T,]
#these are perfect duplicates of the observation in the previous row
df <- df[!duplicated(df), ]


# Save the cleaned data set
write.csv(df, paste0(data_out,"hotels-vienna.csv"), row.names = F)








