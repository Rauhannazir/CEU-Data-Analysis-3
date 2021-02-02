#############################
##     Data analysis 3     ##
##                         ##
##       Assignment I.     ##
##                         ##
##       Data cleaning     ##
#############################


# SET UP ------------------------------------------------------------------

# IN: data from web
# OUT: airbnb_paris_cleaned.csv

#setting working directory
rm(list=ls())

library(tidyverse)

## GETTING THE DATA   
path <- "C://Users/MViki/Documents/CEU/Winter_semester/DA_3/Classes/Assignments/CEU-Data-Analysis-3/Assignment_1/" 

#location folders
data_in  <- paste0(path,"/Data/Raw/")
data_out <- paste0(path,"/Data/Clean/")

# load the data
df <- read_rds(paste0(data_in,"airbnb_paris_total.rds"))


# INITIAL STEPS -----------------------------------------------------------

# basic data checks
sort(unique(df$last_scraped)) # they were scraped between 2020 December 15 and 2021 January 1
sum(rowSums(is.na(df)) == ncol(df)) # no only NA rows
nrow(df[duplicated(df),]) #  no duplicates 
sum(colSums(is.na(df)) > 0.5*nrow(df)) # there are 3columns with at least 50% of the values missing 


# drop unnecessary columns
drops <- c("listing_url",
           "last_scraped",
           "name",
           "description",
           "neighborhood_overview",
           "picture_url",
           "host_url",
           "host_location",
           "host_about",
           "host_thumbnail_url",
           "host_picture_url",
           "host_total_listings_count",
           "neighbourhood_group_cleansed",  # only NA values in column
           "bathrooms", # only NA values in column
           "minimum_minimum_nights", "minimum_maximum_nights", "minimum_nights_avg_ntm",
           "maximum_minimum_nights", "maximum_maximum_nights", "maximum_nights_avg_ntm", 
           "calendar_updated", # only NA values in column
           "number_of_reviews_ltm", "number_of_reviews_l30d",
           "calculated_host_listings_count_entire_homes", 
           "calculated_host_listings_count_private_rooms", 
           "calculated_host_listings_count_shared_rooms")

df<-df[ , !(names(df) %in% drops)]

write.csv(df,file=paste0(data_in,"airbnb_paris_raw.csv"))

#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df$junk <- NULL

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)


# FORMATTING COLUMNS ------------------------------------------------------

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-as.numeric(gsub("%","",as.character(df[[perc]])))
}

#remove dollar signs from price variables
for (i in 1:nrow(df)){
  df$price[i] <- as.numeric(gsub("\\$","",as.character(df$price[i])))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified", "has_availability", "instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}



# AMENITIES ---------------------------------------------------------------
#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]

# create data frame of the amenities
ams <- df %>% select(-(1:47))

# delete spaces in the beginning and end of the column names, and transfer all to lower case
names(ams) <- gsub(" ","_", tolower(trimws(names(ams))))

# look at the column names we have
levs <- sort(names(ams))


# merge all the columns with the same column name
ams <- as.data.frame(do.call(cbind, by(t(ams), INDICES= names(ams),FUN=colSums)))

# list of key words to merge together
cat <- c( "kitchen", "stove", "oven", "frige","o_machine|ee_machine|coffee", "gril",
         "free.*on_premises", "free.*street", "paid.*on_premis|valet", "paid.*off_premises|self-parking|parking",
         "wifi|internet", "netflix|tv.*netflix", "cable", "tv", "sound_system",
         "toiletries", "shampoo|conditioner", "body_soap|gel", "hair_dryer", "washer", "dryer", "iron",  
         "heating", "air_cond|fan", "balcony|terrace", "garden",
         "onsite_bar|restaurant", "breakfast",  "work|office", "spa",  "fitness|gym",  
         "children|baby|crib|high|corner|chang", "smoking", "housekeeping", "fireplace", "clothing_storage"
         )

# function to merge columns with the same key word in them
for (i in cat) {
  tdf <- ams %>% select(matches(i))

  ams$new_col <- ifelse(rowSums(tdf)>0, 1, 0)
  
  names(ams)[names(ams) == "new_col"] <- paste0("have_", i)
  
  ams <- ams %>% select(-colnames(tdf)) 
  
} 

# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(ams), function(x){
  ratio <- sum(ams[[x]])/nrow(ams)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})

amenities <- ams[,selected]

df <- df %>% select((1:47))
  
df <- cbind(df, amenities)

#write csv
write.csv(df,file=paste0(data_out,"airbnb_paris_cleaned.csv"))
saveRDS(df, paste0(data_out,"airbnb_paris_cleaned.rds"))


