###################################################################################

## ANNEX A: Codes for Preparing Data

#install.packages("devtools")
require(devtools)
library(devtools)

#install.packages("jsonlite")
library(jsonlite)

### Reading & Saving Raw Data

# get current working dir
wdir <- getwd()
# init the path to raw data files
json_business_filepath <- paste(wdir, "yelp_academic_dataset_business.json", sep="/data/")
json_review_filepath <- paste(wdir, "yelp_academic_dataset_review.json", sep="/data/")

# Due to large filesize of some of the files (e.g. reviews), 
# it is best to use streaming to read in 10,000 lines at a time until 
# completion
df_raw_business <- jsonlite::stream_in(file(json_business_filepath), pagesize = 10000)
df_raw_review <- jsonlite::stream_in(file(json_review_filepath), pagesize = 10000)

# prepare rds file path
rds_raw_business_filepath <- paste(wdir, "yelp_academic_dataset_business.rds", sep="/data/")
rds_raw_review_filepath <- paste(wdir, "yelp_academic_dataset_review.rds", sep="/data/")

# save to RDS files
saveRDS(df_raw_business, file = rds_raw_business_filepath)
saveRDS(df_raw_review, file = rds_raw_review_filepath)

###################################################################################
# get current working dir
wdir <- getwd()
# prepare rds file path
rds_raw_business_filepath <- paste(wdir, "yelp_academic_dataset_business.rds", sep="/data/")
rds_raw_review_filepath <- paste(wdir, "yelp_academic_dataset_review.rds", sep="/data/")

### Cleaning, Extracting & Formatting Restaurant Businesses
# read from RData files for processing
df_raw_business <- readRDS(file = rds_raw_business_filepath)
# if no categories or neighorhoods or attributes, set as NA
df_biz_restaurants <- df_raw_business
rowSelected <- c()
for (i in 1:nrow(df_biz_restaurants)) {
    # if no categories, set as NA
    if (length(df_biz_restaurants$categories[[i]])==0) {
        df_biz_restaurants$categories[[i]] <- NA
    }
    # if no neighborhoods, set as NA
    if (length(df_biz_restaurants$neighborhoods[[i]])==0) {
        df_biz_restaurants$neighborhoods[[i]] <- NA
    }
    # select only restaurants
    bRest <- ("Restaurants" %in%  df_biz_restaurants$categories[[i]]) 
    rowSelected <- c(rowSelected, bRest)
}
# re-order and select columns
colSelected <- c("business_id", "name", "full_address", 
                 "city", "state", "neighborhoods",
                 "longitude",   "latitude",
                 "review_count", "stars", "open")
# There are 21,892 restaurants
df_biz_restaurants <- df_biz_restaurants[rowSelected, colSelected]
# Only 17,558 restaurant still open
df_biz_restaurants <- df_biz_restaurants[df_biz_restaurants$open==TRUE,]

### Cleaning, Extracting & Formatting Reviews
df_raw_review <- readRDS(file = rds_raw_review_filepath)
# prepare the ordered list of names for vote columns
review_vote_names = list()
for(i in 1:length(df_raw_review$votes)) {
    review_vote_names = c(review_vote_names, paste("review.vote", names(df_raw_review$votes[i]), sep="."))
}
review_vote_names = as.character(review_vote_names)
# Re-order the columns, dropping "votes"
df_review_details <- df_raw_review[c("review_id","date","business_id","user_id","stars","text")] 
# Column-join the first 6 columns + 3 vote-type columns
df_review_details <- data.frame(df_review_details, df_raw_review$votes)
names(df_review_details) <- c("review_id","date","business_id","user_id","stars","text",review_vote_names)
# Converts date column to Date type
df_review_details$date <- as.Date(df_review_details$date)
# get only restaurant reviews - 883,750
df_rest_reviews <- df_review_details
df_rest_reviews <- df_rest_reviews[(df_rest_reviews$business_id %in% df_biz_restaurants$business_id ),]

### Save Processed Data in Rdata format
# save list of restaurants
rds_restaurant_details_filepath <- paste(wdir, "biz_restaurants.rds", sep="/mydata/")
saveRDS(df_biz_restaurants, file = rds_restaurant_details_filepath)
# release memory
rm(df_raw_business)
rm(bRest,colSelected,rowSelected)
# save list of restaurant reviews
rds_rest_reviews_filepath <- paste(wdir, "restaurant_reviews.rds", sep="/mydata/")
saveRDS(df_rest_reviews,rds_rest_reviews_filepath)
# release memory
rm(df_raw_review)
rm(df_review_details)


###################################################################################
