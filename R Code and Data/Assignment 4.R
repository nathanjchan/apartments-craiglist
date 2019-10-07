# Assignment 4
# Nathan Chan
# github.com/nathanjchan
# nathanjchan2@gmail.com

cl = readRDS("cl_apartments.rds")

library(ggplot2)
library(ggmap)
library(stringr)
library(sf)
library(ggpubr)
library(ggpmisc)

# remove two weird points and all NA, 
# and remove weird sqft and super low prices
cl2 = subset(cl, cl$price != 9951095 & 
               cl$price != 34083742 & 
               cl$price > 100 &
               cl$sqft != 200000 &
               cl$sqft > 10
             ) 

# 1. Analyzing apartments in Davis on a map

davis = subset(cl2, cl2$city == "Davis" | cl2$place == "Davis")

davis_coords = c(-121.795024, 38.531215, -121.675951, 38.578497)
davis_map = get_stamenmap(davis_coords, zoom = 12, maptype = "toner")

# A. In which areas of Davis are there apartment sizes of different types?

# change bedrooms to categorical variable
# https://stackoverflow.com/questions/16639484/how-to-convert-integer-into-categorical-data-in-r
davis$bedrooms = as.factor(davis$bedrooms)

# On map by number of bedrooms
ggmap(davis_map) + 
  geom_point(aes(longitude, latitude, color = bedrooms), davis) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain number of bedrooms") +
  guides(size = guide_legend(title = "Number of bedrooms"), color = guide_legend(title = "Number of bedrooms")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# On map by square-footage
ggmap(davis_map) + 
  geom_point(aes(longitude, latitude, color = sqft, size = sqft), davis) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain square-footage") +
  guides(size = guide_legend(title = "Square-footage"), color = guide_legend(title = "Square-footage")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# Note: there are some duplicates in the data set.
# I will ignore them, as they do not appear on the map 
# (they are stacked on top of each other and appear as one point)

# B. Are apartments closer to UC Davis more expensive than apartments farther away?

# On map by price
ggmap(davis_map) + 
  geom_point(aes(longitude, latitude, color = price, size = price), davis) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain prices") +
  guides(size = guide_legend(title = "Price"), color = guide_legend(title = "Price")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# On map by price per bedroom
ggmap(davis_map) + 
  geom_point(aes(longitude, latitude, color = price/bedrooms, size = price/bedrooms), davis) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain price per bedroom") +
  guides(size = guide_legend(title = "Price per bedroom"), color = guide_legend(title = "Price per bedroom")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# On map by price per square-foot
ggmap(davis_map) + 
  geom_point(aes(longitude, latitude, color = price/sqft, size = price/sqft), davis) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain price per square-foot") +
  guides(size = guide_legend(title = "Price per square-foot"), color = guide_legend(title = "Price per square-foot")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# C. Is there a connection between where an apartment is located and the type of parking available?

# On map by type of parking
ggmap(davis_map) + 
  geom_point(aes(longitude, latitude, color = parking), davis) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain types of parking") +
  guides(color = guide_legend(title = "Type of parking")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# 2. Analyzing apartments in the Bay Area on a map

bay_area = subset(cl2, cl2$county == "San Francisco" |
                   cl2$county == "San Mateo" |
                   cl2$county == "Santa Clara" |
                   cl2$county == "Alameda" |
                   cl2$county == "Contra Costa"
                   )

# bay_area = subset(bay_area, is.na(bay_area$bedrooms) == FALSE)

bay_area_coords = c(-122.604945, 36.957870, -121.551308, 38.105303)
bay_area_map = get_stamenmap(bay_area_coords, zoom = 10, maptype = "toner")

# A. In which areas of the Bay Area are there apartment sizes of different types?

bay_area2 = bay_area
bay_area2$bedrooms = as.factor(bay_area2$bedrooms)

# On map by number of bedrooms
ggmap(bay_area_map) + 
  geom_point(aes(longitude, latitude, color = bedrooms), bay_area2[is.na(bay_area2$bedrooms) == FALSE, ]) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in the Bay Area with certain number of bedrooms") +
  guides(size = guide_legend(title = "Number of bedrooms"), color = guide_legend(title = "Number of bedrooms")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# On map by square-footage
ggmap(bay_area_map) + 
  geom_point(aes(longitude, latitude, color = sqft), bay_area2[is.na(bay_area2$sqft) == FALSE, ]) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Bay Area with certain square-footage") +
  guides(size = guide_legend(title = "Square-footage"), color = guide_legend(title = "Square-footage")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))
# (will not include in report because it doesn't show anything useful)

# B. Are apartments close to popular areas more expensive than apartments farther away?

# On map by price
ggmap(bay_area_map) + 
  geom_point(aes(longitude, latitude, color = price), bay_area2) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in the Bay Area with certain prices") +
  guides(size = guide_legend(title = "Price"), color = guide_legend(title = "Price")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))
# (will not include; not very helpful)

# On map by price per bedroom
ggmap(bay_area_map) + 
  geom_point(aes(longitude, latitude, color = price/bedrooms), bay_area[bay_area$bedrooms > 0, ]) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain price per bedroom") +
  guides(size = guide_legend(title = "Price per bedroom"), color = guide_legend(title = "Price per bedroom")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))
# (not including studio apartments)

# On map by price per square-foot
ggmap(bay_area_map) + 
  geom_point(aes(longitude, latitude, color = price/sqft), bay_area) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain price per square-foot") +
  guides(size = guide_legend(title = "Price per square-foot"), color = guide_legend(title = "Price per square-foot")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))
# (will not include; not very helpful)
  
# C. Is there a connection between where an apartment is located and the type of parking available?

# On map by type of parking
ggmap(bay_area_map) + 
  geom_point(aes(longitude, latitude, color = parking), bay_area) +
  labs(x = "Longitude", y = "Latitude", title = "Apartments in Davis with certain types of parking") +
  guides(color = guide_legend(title = "Type of parking")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# 3. Oldest populations in the southern Bay Area and rental market effects

# read census data
# change column names
# https://stackoverflow.com/questions/20956119/assign-headers-based-on-existing-row-in-dataframe-in-r
census = read.csv("2010_census_data/DEC_10_SF1_SF1DP1_with_ann.csv")
colnames(census) = as.character(unlist(census[1, ]))
census = census[-1, ]

# get only southern Bay Area
south_bay_area = subset(bay_area, bay_area$place == "Fremont" |
                          bay_area$place == "Redwood City" |
                          bay_area$place == "Mountain View" |
                          bay_area$place == "San Mateo" |
                          bay_area$place == "Menlo Park" |
                          bay_area$place == "Palo Alto" |
                          bay_area$place == "San Carlos" |
                          bay_area$place == "Atherton" |
                          bay_area$place == "Protola Valley" |
                          bay_area$place == "Foster City" |
                          bay_area$place == "Belmont" |
                          bay_area$place == "Los Altos Hills" |
                          bay_area$place == "Half Moon Bay" |
                          bay_area$place == "Los Altos" |
                          bay_area$place == "Woodside" |
                          bay_area$place == "San Jose" |
                          bay_area$place == "Campbell" |
                          bay_area$place == "Gilroy" |
                          bay_area$place == "Los Gatos" |
                          bay_area$place == "Santa Clara" |
                          bay_area$place == "Sunnyvale" |
                          bay_area$place == "Milpitas" |
                          bay_area$place == "Cuptertino" |
                          bay_area$place == "Morgan Hill" |
                          bay_area$place == "Saratoga"
                          )

# get data only related to southern Bay Area
south_census = subset(census, str_detect(census$Geography, "Fremont") |
                        str_detect(census$Geography, "Redwood City") |
                        str_detect(census$Geography, "Mountain View") |
                        str_detect(census$Geography, "San Mateo") |
                        str_detect(census$Geography, "Menlo Park") |
                        str_detect(census$Geography, "Palo Alto") |
                        str_detect(census$Geography, "San Carlos") |
                        str_detect(census$Geography, "Atherton") |
                        str_detect(census$Geography, "Protola Valley") |
                        str_detect(census$Geography, "Foster City") |
                        str_detect(census$Geography, "Belmont") |
                        str_detect(census$Geography, "Los Altos Hills") |
                        str_detect(census$Geography, "Half Moon Bay") |
                        str_detect(census$Geography, "Los Altos") |
                        str_detect(census$Geography, "Woodside") |
                        str_detect(census$Geography, "San Jose") |
                        str_detect(census$Geography, "Campbell") |
                        str_detect(census$Geography, "Gilroy") |
                        str_detect(census$Geography, "Los Gatos") |
                        str_detect(census$Geography, "Santa Clara") |
                        str_detect(census$Geography, "Sunnyvale") |
                        str_detect(census$Geography, "Milpitas") |
                        str_detect(census$Geography, "Cuptertino") |
                        str_detect(census$Geography, "Morgan Hill") |
                        str_detect(census$Geography, "Saratoga")
                      )

# ignore this function; doesn't work
# for some reason cannot compare the strings properly
merge_data = function(south_bay_area, south_census) {
  # Create a vector with the median ages in the same order as the south_bay_area
  # Will add median age vector to south_bay_area
  median_age = c()
  for (i in 1:nrow(south_bay_area)) {
    for (j in 1:nrow(south_census)) {
      if (str_detect(as.character(south_census[j, ]$Geography), as.character(south_bay_area[i, ]$place)) == TRUE) {
        median_age[i] = as.character(south_census[j, ]$Percent..SEX.AND.AGE...Total.population...Median.age..years.)
        break
      }
    }
  }
  return(median_age)
}

write.csv(south_census, "south_census.csv")
south_census = read.csv("south_census.csv")

remove_excess = function(data) {
  # remove parts of the string that shouldn't be there
  # allow us to merge
  for (i in 1:nrow(data)) {
    if (str_detect(data[i, 3], "city")) {
      data[i, 3] = str_remove(data[i, 3], " city, California")
    }
    else if (str_detect(data[i, 3], "town")) {
      data[i, 3] = str_remove(data[i, 3], " town, California")
    }
  }
  return(data)
}

south_census = remove_excess(south_census)

combined_bay_area = merge(south_bay_area, south_census, by.x = "city", by.y = "Geography")

# Plot median age against price of apartment
ggplot(combined_bay_area, aes(x = Number..SEX.AND.AGE...Total.population...Median.age..years., y = price)) +
  geom_point() +
  labs(x = "Median age of population in city where apartment is located", y = "Price of apartment",
       title = "Median age of the city compared to prices of apartments in the city") +
  geom_smooth(method = 'loess') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  stat_cor(method = "pearson")
