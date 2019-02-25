library(rgdal)
library(sp)
library(dplyr)
library(magrittr)

#   ____________________________________________________________________________
#   Shapes                                                                  ####

suppressMessages({
    CTshapes <- readOGR("data/cb_2013_36_ct_500k/cb_2013_36_tract_500k.shp", 
                        layer = "cb_2013_36_tract_500k",
                        stringsAsFactors = FALSE)
    
    CTshapes <- readOGR("data/NYCnoWater/NYCnoWater.shp", 
                        layer = "NYCnoWater",
                        stringsAsFactors = FALSE)
    
    hoods <- readOGR("data/NYCneighborhoods2/geo_export_be611e44-cf7b-4c1a-a82f-a5504df8d5d6.shp", 
                     stringsAsFactors = FALSE)
    
    zillowHoods <- readOGR("data/ZillowNeighborhoods-NY", 
                           stringsAsFactors = FALSE)
    
    subwayLines <- readOGR("data/subwayStations/nyctsubwayroutes_20150914/nyctsubwayroutes_20150914.shp",
                           stringsAsFactors = FALSE)
    
    subwayStations <- readOGR("data/subwayStations/nyctsubwaystops_20150914_hudsyds/nyctsubwaystops_20150914_hudsyds.shp",
                              stringsAsFactors = FALSE)
    
    proj4string(hoods) <- proj4string(CTshapes)
    proj4string(subwayLines) <- proj4string(CTshapes)
    proj4string(subwayStations) <- proj4string(CTshapes)
    
    schools <- readOGR("data/Public_School_Locations/Public_Schools_Points_2011-2012A.shp",
                       stringsAsFactors = FALSE)
    
    schools <- spTransform(schools, CRS("+proj=longlat +datum=WGS84"))
})

print("Shapefiles complete")

vars <- read.csv2("data/VariableDescriptions.csv",
                  stringsAsFactors = FALSE)

#pred <- read.csv("data/prediction2.csv")
pred <- read.csv("data/Scored MARS.csv")
pred %<>% mutate(rank = dense_rank(desc(bin_20)))

# divide columns by 100
cols <- c("pct_e9", "pct_e912", "pct_hs", "pct_coll", "pct_ass", "pct_bat",
          "pct_grad", "pct_all_coll", "pct_p1", "pct_p2", "pct_p3", "pct_p4",
          "pct_family", "pct_married", "pct_other_hht", "pct_fam_fem",
          "pct_fam_male", "pct_non_fam", "pct_non_fam_alone",
          "pct_child", "pct_work_yr", "pct_work_not", "pct_taxi")

pred %<>% 
    dplyr::select(one_of(cols)) %>% 
    mutate_each(
        funs(./100)
    ) %>% bind_cols(pred[names(pred) != cols])


# join prediction data with shapefile
GeoDF <- sp::merge(CTshapes, pred, by.x = "GEOID", by.y = "ct", all.x = FALSE)


#   ____________________________________________________________________________
#   Labels and IDs for maps                                                 ####

hoodIDs <- as.vector(hoods$ntacode)
zillowHoodIDs <- as.vector(zillowHoods$REGIONID)

hoodsLabels <- sprintf(
    "<strong>%s</strong>",
    hoods$ntaname) %>% lapply(htmltools::HTML)

zillowHoodsLabels <- sprintf(
    "<strong>%s</strong>",
    zillowHoods$NAME) %>% lapply(htmltools::HTML)

schoolNames <- sprintf(
    "<strong>%s</strong>",
    schools$SCHOOLNAME) %>% lapply(htmltools::HTML)


#   ____________________________________________________________________________
#   Zillow                                                                  ####

library(magrittr)
library(padr)
library(lubridate)

p <- read.csv2("data/zillow/zestimatePerCT_2.csv", stringsAsFactors = FALSE,
               colClasses = c("Date", NA, NA))

p %<>% filter(year(date) <= 2016)

print("Zillow data complete")


#   ____________________________________________________________________________
#   Yelp                                                                    ####

library(stringr)

yelpByCT <- read.csv2("data/yelp/yelpData.csv", stringsAsFactors = FALSE,
                      colClasses = c(NA, NA, NA, NA, NA, "Date", NA, NA))

yelpByCT %<>% filter(year <= 2016)

yelp <- read.csv2("data/yelp/businessDetailsFlat.csv",
                  stringsAsFactors = FALSE)

yelp %<>% filter(grepl("(pizza)|(bars)|(coffee)|(galleries)|(burgers)", categories))

yelpNames <- sprintf(
    "<strong>%s</strong><br>%s",
    yelp$name, yelp$categories) %>% lapply(htmltools::HTML)

yelp$lat %<>% as.numeric()
yelp$lon %<>% as.numeric()

print("Yelp data complete")


#   ____________________________________________________________________________
#   NYC Taxicab                                                             ####

taxi <- read.csv2("data/taxicab/taxiData.csv", stringsAsFactors = FALSE,
                  colClasses = c(NA, "Date", NA, NA, NA, "Date"))

taxi %<>% filter(year(date_month) <= 2016)

print("Taxicab data complete")


#   ____________________________________________________________________________
#   NYC Subway                                                              ####

subway <- read.csv2("data/subwayStations/subwayStations.csv",
                    stringsAsFactors = FALSE)

names(subway)[3:4] <- c("Latitude", "Longitude")

subwayFlat <- read.csv2("data/subwayStations/subwayStationsFlat.csv",
                    stringsAsFactors = FALSE)

names(subwayFlat)[2:3] <- c("lat", "lon")

print("Subway data complete")


#   ____________________________________________________________________________
#   Google Trends                                                           ####

g <- read.csv2("data/google/googleTrendsByCT.csv",
               stringsAsFactors = FALSE)

g %<>% filter(year > 2004 & year <= 2016)

print("Google data complete")

#   ____________________________________________________________________________
#   Wikipedia edits                                                         ####

w <- read.csv2("data/wikipedia/wikiEditsByCT.csv",
               stringsAsFactors = FALSE)

w %<>% filter(year > 2004 & year <= 2016)

print("Wikipedia data complete")
