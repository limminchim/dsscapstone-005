###################################################################################

## ANNEX C: Preliminary and Exploratory Analysis 

#install.packages("devtools")
require(devtools)
library(devtools)

# get current working dir
wdir <- getwd()

# read list of restaurants from RDS file
rds_restaurant_details_filepath <- paste(wdir, "biz_restaurants.rds", sep="/mydata/")
df_biz_restaurants <- readRDS(file = rds_restaurant_details_filepath)

# read reviews from RDS file
rds_rest_reviews_filepath <- paste(wdir, "restaurant_reviews.rds", sep="/mydata/")
df_rest_reviews <- readRDS(file = rds_rest_reviews_filepath)

numRestaurants <- length(unique(df_biz_restaurants$business_id))

numReviews <- nrow(df_rest_reviews)


### Number of businesses in different states
# count of businesses in different states
df_state <- df_biz_restaurants$state
# gives you the freq table
ft_state <- table(df_state)
# sort
ft_state <- sort(ft_state, decreasing = T)
# histogram
barplot(ft_state, 
        main = "Distribution of restaurants",
        xlab = "across different states",
        ylab = "count")
names(ft_state)

# filtered histogram of states with most businesses
filtered_state <- ft_state[names(ft_state)[ ft_state > 100 ]]
barplot(filtered_state, 
        main = "Distribution of restaurants",
        xlab = "States with > 100 restaurants",
        ylab = "count")
names(filtered_state)[ filtered_state > 100 ]

# count of businesses in different cities
df_city <- df_biz_restaurants$city
# gives you the freq table
ft_city <- table(df_city)
# sort
ft_city <- sort(ft_city, decreasing = T)
# histogram
barplot(ft_city, 
        main = "Distribution of restaurants",
        xlab = "across different cities",
        ylab = "count")


# To view and focus on cities with higher number of restaurants
filtered_city <- ft_city[names(ft_city)[ ft_city > 100 ]]
# filtered histogram of cities with most businesses
barplot(filtered_city, 
        main = "Distribution of restaurants",
        xlab = "Cities with > 100 restaurants",
        ylab = "count")
names(filtered_city)[ filtered_state > 100 ]

### Restaurants and star ratings
# count of stars for different businesses
df_rest_stars <- df_biz_restaurants$stars
# gives you the freq table
ft_rest_star_count <- table(df_rest_stars)
# histogram
barplot(ft_rest_star_count, 
        main = "Distribution of restaurant ratings",
        xlab = "Star-Rating",
        ylab = "count")

### Reviews & review ratings
# count of stars for different businesses
df_reviews_stars <- df_rest_reviews$stars
# gives you the freq table
ft_review_star_count <- table(df_reviews_stars)
# histogram
barplot(ft_review_star_count, 
        main = "Distribution of review ratings",
        xlab = "Star-Rating",
        ylab = "count")


####################################################################################


