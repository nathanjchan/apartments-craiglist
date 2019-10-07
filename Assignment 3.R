# STA 141A Assignment 3
# Nathan Chan

cl = readRDS("cl_apartments.rds")

# 1

find_max = function(data) {
  # Reads date, prints the latest date
  max = 0
  for (i in 1:nrow(data)) {
    temp = as.Date(data[i, 6])
    if (is.na(temp) | is.na(max)) {
      next
    }
    if (temp > max) {
      max = temp
      print(max)
    } else {
      next
    }
  }
}

find_min = function(data) {
  # Reads date, prints the earliest date
  min = as.Date("2018-10-15")
  for (i in 1:nrow(data)) {
    temp = as.Date(data[i, 6])
    if (is.na(temp) | is.na(min)) {
      next
    }
    if (temp < min) {
      min = temp
      print(min)
    } else {
      next
    }
  }
}

find_max(cl)
find_min(cl)

dates = sort(cl$date_posted)

# 2

unique(cl$laundry)
table(cl$laundry)
table(cl$parking)

ff = subset(cl, cl$bedrooms >= 2 & cl$bathrooms >= 1.5 & cl$pets == "both" & cl$laundry != "none" &
              cl$parking != "none" & cl$city != "NA")

library("plyr")
cities_count = count(cl, "city")
cities_count
sort(cities_count$freq)

ff_cities = subset(ff, ff$city == "Los Angeles" | ff$city == "Sacramento" | ff$city == "San Diego" | 
                     ff$city == "San Francisco" | ff$city == "San Jose")

cities = subset(cl, (cl$city == "Los Angeles" | cl$city == "Sacramento" | cl$city == "San Diego" | 
                     cl$city == "San Francisco" | cl$city == "San Jose") & cl$city != "NA")

ff_not_cities = subset(ff, ff$city != "Los Angeles" & ff$city != "Sacramento" & ff$city != "San Diego" & 
                         ff$city != "San Francisco" & ff$city != "San Jose")

not_cities = subset(cl, (cl$city != "Los Angeles" & cl$city != "Sacramento" & cl$city != "San Diego" & 
                      cl$city != "San Francisco" & cl$city != "San Jose") & cl$city != "NA")

prop_city = nrow(ff_cities) / nrow(cities)
prop_not_cities = nrow(ff_not_cities) / nrow(not_cities)

nrow(ff_cities)
nrow(cities)
nrow(ff_not_cities)
nrow(not_cities)

library(ggplot2)
library(ggpubr)
library(ggpmisc)

# http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/

cl2 = subset(cl, cl$price != 9951095 & cl$price != 34083742 & cl$price > 100) # remove two weird points and all NA
cl2 = subset(cl2, cl2$sqft != 200000 & cl2$sqft > 10) # also remove weird sqft and super low prices

g1 = ggplot(cl2, aes(x = bedrooms, y = price)) + geom_point() + geom_smooth(method = 'loess') +
  labs(x = "Number of bedrooms", y = "Price of apartment", 
       title = "How number of bedrooms affects price of apartment") + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  stat_cor(method = "pearson", label.y = 18000) 

g2 = ggplot(cl2, aes(x = bathrooms, y = price)) + geom_point() + geom_smooth(method = 'loess') +
  labs(x = "Number of bathrooms", y = "Price of apartment", 
       title = "How number of bathrooms affects price of apartment") + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  stat_cor(method = "pearson", label.y = 18000)

library(gridExtra)
grid.arrange(g1, g2)

test = unique(cl$title)

# 3

SF = subset(cl, cl$city == "San Francisco")
LA = subset(cl, cl$city == "Los Angeles")
SM = subset(cl, cl$city == "Sacramento")

three_cities = subset(cl, (cl$city == "Los Angeles" | cl$city == "Sacramento" | cl$city == "San Francisco") 
                      & cl$city != "NA")
three_cities = subset(three_cities, three_cities$sqft > 10)

library("ggridges")
library("scales")

# https://stackoverflow.com/questions/36604127/creating-a-bar-plot-with-proportions-on-ggplot

ggplot(three_cities, aes(bedrooms, y = ..prop.., fill = city)) + geom_bar() + facet_wrap(~ city) +
  labs(x = "Proportion of apartments with certain bedroom count", y = "Proportion of apartments", 
       title = "Proportion of apartments with certain number of bedrooms in three cities") +
  guides(fill = guide_legend(title = "City")) + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  scale_y_continuous(labels = percent_format())

ggplot(three_cities, aes(bathrooms, y = ..prop.., fill = city)) + geom_bar() + facet_wrap(~ city) +
  labs(x = "Proportion of apartments with certain bathroom count", y = "Proportion of apartments", 
       title = "Proportion of apartments with certain number of bathrooms in three cities") +
  guides(fill = guide_legend(title = "City")) + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  scale_y_continuous(labels = percent_format())

ggplot(three_cities, aes(price, fill = city)) + geom_density() + facet_grid(city ~ .) +
  labs(x = "Price of apartment", y = "Proportion of apartments with certain price",
       title = "Proportion of apartments with certain prices in three cities") +
  guides(fill = guide_legend(title = "City")) + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  scale_y_continuous(labels = percent_format())
  
ggplot(three_cities, aes(sqft, fill = city)) + geom_density() + facet_grid(city ~ .) +
  labs(x = "Square-footage of apartment", y = "Proportion of apartments with certain square-footage",
       title = "Proportion of apartments with certain square-footage in three cities") +
  guides(fill = guide_legend(title = "City")) + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# Changing formating:
# https://stackoverflow.com/questions/28243514/ggplot2-change-title-size
# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
# http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles

# 4

# 1)

with_amenities = subset(cl2, cl2$laundry != "none" & cl2$parking != "none")
without_amenities = subset(cl2, cl2$laundry == "none" & cl2$parking == "none")

mean(with_amenities$price, na.rm = TRUE)
mean(without_amenities$price, na.rm = TRUE)

sd(with_amenities$price, na.rm = TRUE)
sd(without_amenities$price, na.rm = TRUE)

nrow(with_amenities)
nrow(without_amenities)

# 2)

ggplot(cl2, aes(x = sqft, y = price)) + geom_point() +
  labs(x = "Apartment square-footage", y = "Apartment price", title = "Apartment square-footage compared to price") +
  theme_minimal() + theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

ggplot(three_cities, aes(x = sqft, y = price)) + geom_point() + facet_grid(city ~ .) +
  labs(x = "Apartment square-footage", y = "Apartment price",
       title = "Apartment square-footage compared to price") +
  theme_minimal() + geom_smooth(method = 'lm') +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica"))

# 3)

cl3 = subset(cl2, is.na(cl2$laundry) != TRUE & is.na(cl2$parking) != TRUE)
  
yes_laundry = subset(cl3, cl3$laundry != "none")

no_laundry = subset(cl3, cl3$laundry == "none")

yes_parking = subset(cl3, cl3$parking != "none")

no_parking = subset(cl3, cl3$parking == "none")

nrow(subset(yes_laundry, yes_laundry$parking != "none"))
nrow(subset(yes_laundry, yes_laundry$parking == "none"))

nrow(yes_laundry)

nrow(subset(no_laundry, no_laundry$parking != "none"))
nrow(subset(no_laundry, no_laundry$parking == "none"))

nrow(no_laundry)

nrow(subset(yes_parking, yes_parking$laundry != "none"))
nrow(subset(yes_parking, yes_parking$laundry == "none"))

nrow(yes_parking)

nrow(subset(no_parking, no_parking$laundry != "none"))
nrow(subset(no_parking, no_parking$laundry == "none"))

nrow(no_parking)

# 4)

lasf = subset(three_cities, three_cities$city != "Sacramento")

ggplot(lasf, aes(price, fill = city)) + geom_density() + facet_grid(city ~ .) +
  labs(x = "Price of apartment", y = "Proportion of apartments with certain price",
       title = "Proportion of apartments with certain prices in LA and SF") +
  guides(fill = guide_legend(title = "City")) + theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  scale_y_continuous(labels = percent_format())

mean(lasf$price[lasf$city == "Los Angeles"], na.rm = TRUE)
mean(lasf$price[lasf$city == "San Francisco"], na.rm = TRUE)

sd(lasf$price[lasf$city == "Los Angeles"], na.rm = TRUE)
sd(lasf$price[lasf$city == "San Francisco"], na.rm = TRUE)

lasf = subset(lasf, is.na(lasf$price) == FALSE)
nrow(subset(lasf, lasf$city == "Los Angeles"))
nrow(subset(lasf, lasf$city == "San Francisco"))

# 5)

bed_bath = subset(cl2, is.na(cl2$bedrooms) == FALSE & is.na(cl2$bathrooms) == FALSE)

# https://stackoverflow.com/questions/46845342/plot-data-with-duplicate-points

ggplot(bed_bath, aes(x = bedrooms, y = bathrooms)) + geom_point() + geom_smooth(method = "loess", size = 2) + 
  theme_minimal() + geom_jitter(alpha = 0.5) +
  theme(plot.title = element_text(size = 12, face = "bold"), text = element_text(family = "Helvetica")) +
  labs(x = "Number of bedrooms", y = "Number of bathrooms", title = "Number of bedrooms compared with bathrooms")
