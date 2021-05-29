rm(list = ls()) 
options(scipen=999)

options(digits=6)

pacman::p_load(pacman, data.table, lubridate, ggplot2, readr, visdat, 
               reshape2, dplyr, stargazer, quantmod, GGally, bit64,rio)
getwd()
setwd(C:\Users/maor1/Google Drive/BIG DATA)

calender <-fread("C:\\big data in economics\\exercise1\\calendar_clean.csv")
listings <- fread("C:\\big data in 
                economics\\exercise1\\listings_clean.csv")
calender<- calendar_clean
listings<-listings_clean
class(listings$host_since)
listings$host_since <- as_date(listings$host_since)

length(unique(calender$listing_id)
total_listings <- listings[, .N, by = host_since][order(host_since)]
total_listings$cumsum <- cumsum(total_listings$N)

ggplot(data=total_listings, aes(x=host_since , y=cumsum)) +
  geom_line() + theme_bw()

##########by_month###############

listings$floor_month <- floor_date(listings$host_since,'month')
by_months <- listings[,.N, by = floor_month][order(floor_month)]
by_months$cumsum <- cumsum(by_months$N)
ggplot(by_months) +
  aes(floor_month, N, fill = floor_month) +
  geom_bar(stat = "identity")

ggplot(data = by_months, aes(x=floor_month, y=N)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(data = by_months, aes(x=floor_month, y=cumsum)) +
  geom_line() +
  geom_point() +
  theme_bw()

####by_days#####
listings[, `:=` (wday = wday(host_since),
                 day = day(host_since))]

by_days <- listings[,.N, by = day][order(day)]
by_days$cumsum <- cumsum(by_days$N)

ggplot(data = by_days, aes(x=day, y=N)) +
  geom_line(color = 'blue') +
  geom_point(color = 'red') +
  theme_bw()

ggplot(data = by_days, aes(x=day, y=cumsum)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_bw()

####property_type####

property_type <- listings[, .N, by = property_type]
property_type$property_type <- na_if(property_type$property_type, "")
property_type$prop <-(property_type$N/sum(property_type$N)
export(property_type,"property_type.csv")

####number of beds####
number_of_beds2 <- listings[, .N, by = beds][order(beds)]
number_of_beds <- listings[, .(sum_of_beds = sum(beds, na.rm = T)), 
                    by = floor_month][order(floor_month)]
number_of_beds$cumsum <- cumsum(number_of_beds$sum_of_beds)

ggplot(data=number_of_beds) +
  geom_line(aes(x=floor_month , y=cumsum)) +
  geom_point(aes(x=floor_month , y=sum_of_beds), color = 'red') +
  geom_line(aes(x=floor_month , y=sum_of_beds), color = 'blue', linetype = "dotdash") +
  theme_bw()
export(number_of_beds2,"number_of_beds.csv")

####host_type####

listings[, .N, by = host_is_superhost]
ggplot(listings[, .N, by = host_is_superhost]) +
  aes(host_is_superhost, N, fill = host_is_superhost) +
  geom_bar(stat = "identity")

####rating####

listings[, .N, by = review_scores_rating][order( review_scores_rating)]
ggplot(listings[, .N, by = review_scores_rating]) +
  aes(review_scores_rating, N, fill = review_scores_rating) +
  geom_bar(stat = "identity")

####question2####

#class(calender$available_category)
#calender$date <- as_date(calender$date)
#calender$price <- parse_number(calender$price_dollars)
#calender$available_category <- as.factor(calender$available_category)
#calender$floor_date <- floor_date(calender$date,'month')
#setnames(calender,'listing_id', 'id')
#calender <- merge(x = calender,
#                  y = listings [,.(id,price_dollars)],
#                  by = "id")
#
#price_evolution <- calender[available_category == 1,.(id,
#                               available_category,
#                               price_dollars.x,
#                               date, floor_date)][order(floor_date)]
#class(price_evolution$price_dollars.x)
#price_evolution$price_dollars.x <- parse_number(price_evolution$price_dollars.x)
#price_evolution$rollmean <- frollmean(price_evolution$price_dollars.x, n = 5)



calender[, `:=` (wday = wday(date, label = TRUE),
                 month = month(date),
                 day = day(date))]
ggplot(calender) +
  geom_bar(aes(as.factor(month)), color = "black")

ggplot(calender) +
  geom_bar(aes(as.factor(month), fill = available_category), color = "black") +
  facet_grid(. ~ available_category)

#4#

