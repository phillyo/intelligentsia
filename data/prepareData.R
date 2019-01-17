#################################
# prepare data
##

setwd("/Users/philipp/Google Drive/NYU/capstone/App/shiny/intelligentsia")
#setwd("C:/Temp/Google Drive/NYU/capstone/App/shiny/intelligentsia/")

##########
# zillow #
##########

library(magrittr)
library(padr)
library(lubridate)
library(zoo)

#z <- read.csv2("data/zillow/zestimateFlat.csv", stringsAsFactors = FALSE)

#propertyNeighborhoods <- z %>% distinct(zID, name_n)

pDetails <- read.csv2("data/zillow/propertyDetailsFlatGeo.csv", stringsAsFactors = FALSE)

zestimateProperty <- read.csv2("data/zillow/zestimateProperty.csv", stringsAsFactors = FALSE)

# alternative start #
zestimateProperty <- read.csv2(
    "data/zillow/zestimatePropertyTest.csv", 
    stringsAsFactors = FALSE,
    col.names = c("date","name","property zestimate","regionType","zID")
)
# alternative end  #

zestimateProperty <- zestimateProperty[!duplicated(zestimateProperty),]

# if more than 1 zestimate exists take their mean
zestimateProperty %<>% dplyr::group_by(date, zID) %>% 
    dplyr::summarise(property.zestimate = mean(property.zestimate))

p <- pDetails %>% inner_join(zestimateProperty)

#p %<>% left_join(propertyNeighborhoods)

# fix values
p[p$lastSoldTime == "","lastSoldTime"] <- NA

# change data types
p$date %<>% as.POSIXct()
p$lastSoldTime <- strptime(paste("1",p$lastSoldTime), "%d %b %Y")
p$lastSoldTime %<>% as.POSIXct()
p$lat %<>% as.numeric()
p$lon %<>% as.numeric()


# set all dates to the last day of the month
p$date <- ceiling_date(ymd(p$date), "month") - days(1)

p$zestimate.per.sqft <- p$property.zestimate / p$sqft

p %<>% dplyr::filter(sqft > 10) %>% 
    dplyr::group_by(GEOID, date) %>% 
    dplyr::summarise(avg.z.sqft = mean(zestimate.per.sqft, na.rm = TRUE)) %>%
    dplyr::arrange(date)

write.csv2(p, "data/zillow/zillowData.csv", row.names = FALSE)

print("Zillow data complete")

##########
## yelp ##
##########
# yelpByCT <- read.csv("data/yelp/reviewsByCT.csv", 
#                      stringsAsFactors = FALSE,
#                      sep = "\t", 
#                      col.names = c("GEOID", "year", "month", "count", "rating"))

yelpByCT <- read.csv2("data/yelp/reviewsByCT_knn.csv",
                     stringsAsFactors = FALSE)
names(yelpByCT)[3:4] <- c("rating","count")

yelpByCT$date <- ymd(paste(yelpByCT$year, yelpByCT$month, "1", sep = "-"))
yelpByCT$date <- ceiling_date(yelpByCT$date, "month") - days(2)

yelpByCT %<>% filter(date <= "2016-12-31")

yelpByCT %<>% arrange(GEOID, date)

yelpByCT %<>% 
    group_by(GEOID) %>%
    mutate(rollingReviews = rollapplyr(count, 12, sum, na.rm = TRUE, partial = TRUE),
           avgRating = rollapplyr(rating, 12, mean, na.rm = TRUE, partial = TRUE),
           rank = rank(date)) %>%
    ungroup() %>% 
    dplyr::filter(rank > 12)
    
write.csv2(yelpByCT, "data/yelp/yelpData.csv", row.names = FALSE)

print("Yelp data complete")

#####
# experimental yelp below #

yelpByCT %<>% 
    thicken("month") %>%
    pad("month", by = "date_month") %>%
    tidyr::fill(GEOID) %>%
    group_by(GEOID) %>%
    mutate(rollingReviews = rollapplyr(count, 12, sum, na.rm = TRUE, partial = TRUE),
           avgRating = rollapplyr(rating, 12, mean, na.rm = TRUE, partial = TRUE),
           rank = rank(date_month)) %>%
    ungroup() %>% 
    dplyr::filter(rank > 12) %>%
    dplyr::select(1,2,7,8,9,10)

yelpByCT %>% 
    #filter(GEOID == sample(yelpByCT$GEOID,1)) %>% 
    #dplyr::filter(GEOID %in% c(36005021900,36047041402)) %>%
    #dplyr::filter(GEOID %in% c(36047019300)) %>% 
    dplyr::filter(GEOID %in% c(36047019300, 36061025700)) %>%
    group_by(GEOID) %>%
    thicken("month") %>%
    pad("month", by = "date_month") %>%
    tidyr::fill(GEOID) %>%
    mutate(rollingReviews = rollapplyr(count, 12, sum, na.rm = TRUE, partial = TRUE),
           avgRating = rollapplyr(rating, 12, mean, na.rm = TRUE, partial = TRUE),
           rank = rank(date_month)) %>%
    ungroup() %>% 
    #dplyr::filter(rank > 12) %>%
    dplyr::select(1,2,7,8,9,10) %>%
    #dplyr::filter(GEOID %in% c(36047019300))%>% View()
    dplyr::filter(GEOID %in% c(36047019300, 36061025700)) %>% View()

##########
## taxi ##
##########

taxi <- read.csv("data/taxicab/taxitripsByCT.csv", sep = "\t",
                 col.names = c("GEOID","year","month","trips"))

taxi %<>% filter(year > 2008 & year <= 2016)

taxi$date <- ymd(paste(taxi$year,taxi$month,"1", sep = "-"))
taxi$date <- ceiling_date(taxi$date, "month") - days(2)

taxi %<>% arrange(GEOID, date)

taxi %<>% 
    #filter(GEOID == sample(yelpByCT$GEOID,1)) %>% 
    #dplyr::filter(GEOID %in% c(36005021900,36047041402)) %>%
    #dplyr::filter(GEOID %in% c(36047019300)) %>%
    thicken("month") %>%
    pad("month", by = "date_month") %>%
    tidyr::fill(GEOID) %>%
    group_by(GEOID) %>%
    mutate(rollingTrips = rollapplyr(trips, 12, sum, na.rm = TRUE, partial = TRUE),
           rank = rank(date)) %>%
    dplyr::filter(rank > 12) %>%
    ungroup() %>%
    dplyr::select(GEOID, date_month, trips, rollingTrips, rank, date)

print("Taxicab data complete")

# taxi %>% 
#     #filter(GEOID == sample(yelpByCT$GEOID,1)) %>% 
#     #dplyr::filter(GEOID %in% c(36005021900,36047041402)) %>%
#     dplyr::filter(GEOID %in% c(36047019300,36005021900)) %>%
#     thicken("month") %>%
#     pad("month", by = "date_month") %>%
#     tidyr::fill(GEOID) %>%
#     group_by(GEOID) %>%
#     mutate(rollingTrips = rollapplyr(trips, 12, sum, na.rm = TRUE, partial = TRUE),
#            rank = rank(date)) %>%
#     dplyr::filter(rank > 12) %>%
#     ungroup() %>%
#     select(GEOID, date_month, trips, rollingTrips, rank, date) %>% data.frame()

write.csv2(taxi, "data/taxicab/taxiData.csv", row.names = FALSE)

############
## subway ##
############

library(tidyr)

s <- read.csv("data/subwayStations/NYC_Transit_Subway_Entrance_And_Exit_Data.csv")

s %>% dplyr::select(Line:Route11) %>% 
    write.csv2("data/subwayStations/subwayStations.csv", row.names = FALSE)

s %<>% dplyr::select(3:16) %>% distinct() %>% 
    gather("Route", "Line", 4:14) %>% 
    filter(Line != "") %>% 
    group_by(Station.Latitude, Station.Longitude) %>% 
    mutate(n = dense_rank(Line) - 1,
           total = n()) %>% 
    arrange(Station.Latitude, Station.Longitude, n)

s[s$Station.Name == "6th Av",c(6:7)]

for(r in 1:nrow(s)) {
    n <- s[r,]$n
    s[r,"columns"] <- rep(0:3,3)[1:s[r,]$total][n+1]
}

s[s$Station.Name == "6th Av",c(6:8)]

s$rows <- floor(s$n / 4)
s[s$Station.Name == "6th Av",c(6:9)]
s$offsetY <- s$rows
s$offsetX <- s$columns

s[s$Station.Name == "6th Av",c(6:11)]

s %>% write.csv2("data/subwayStations/subwayStationsFlat.csv", row.names = FALSE)