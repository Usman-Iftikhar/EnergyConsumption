# Title: C3T1 Energy Consumption

# Last update: 2019.02.19

# File: EnergyConsumption.R
# Project name: C3T1 Energy Consumption


###############
# Project Notes
###############

# Summarize project: 
# Deal with regional home developer to develop analytics for a new set of electrical sub-metering 
# devices used for power management in smart homes.


###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()
?getwd  # get help
# set working directory
setwd("/Users/muift/Documents/R_Projects/C3T1")
dir()


################
# Load packages
################

install.packages("dplyr")
install.packages("tidyr")
install.packages("RMySQL")
install.packages("lubridate")

library(ggplot2)
library(caret)
library(corrplot)
library(C50)
library(doParallel)
library(mlbench)
library(readr)
library(parallel)
library(plyr)
# library(knitr)
library(arules)
library(caTools)
library(prabclus)
library(DiceOptim)
library(DiceDesign)
library(trimcluster)
library(arulesViz)
library(dbplyr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(RMySQL)
library(lubridate)
library(plotly)



## Create a database connection 
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)
#[1] "iris"    "yr_2006" "yr_2007" "yr_2008" "yr_2009" "yr_2010"

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

## Lists attributes contained in a table
dbListFields(con,'yr_2006')
#[1] "id"                    "Date"                 
#[3] "Time"                  "Global_active_power"  
#[5] "Global_reactive_power" "Global_intensity"     
#[7] "Voltage"               "Sub_metering_1"       
#[9] "Sub_metering_2"        "Sub_metering_3" 

## Get Date, Time, and 3 sub-meter attributes for all the years

# ----- 2006
yr_2006 <- dbGetQuery(con, 
                      "SELECT Date, 
                        Time, 
                        Sub_metering_1, 
                        Sub_metering_2, 
                        Sub_metering_3 
                      FROM yr_2006")
str(yr_2006)
#'data.frame':	21992 obs. of  5 variables:
#$ Date          : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
#$ Time          : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  1 1 2 1 1 2 1 1 1 2 ...
#$ Sub_metering_3: num  17 16 17 17 17 17 17 17 17 16 ...

summary(yr_2006)
#Date               Time                Sub_metering_1   Sub_metering_2   Sub_metering_3 
#Length:21992       Length:21992        Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
#Class :character   Class :character    1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
#Mode  :character   Mode  :character    Median : 0.000   Median : 0.000   Median : 0.00  
#                                       Mean   : 1.249   Mean   : 2.215   Mean   : 7.41  
#                                       3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.00  
#                                       Max.   :77.000   Max.   :74.000   Max.   :20.00

head(yr_2006)
#        Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2006-12-16 17:24:00              0              1             17
#2 2006-12-16 17:25:00              0              1             16
#3 2006-12-16 17:26:00              0              2             17
#4 2006-12-16 17:27:00              0              1             17
#5 2006-12-16 17:28:00              0              1             17
#6 2006-12-16 17:29:00              0              2             17

tail(yr_2006)
#            Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#21987 2006-12-31 23:54:00              0              0              0
#21988 2006-12-31 23:55:00              0              0              0
#21989 2006-12-31 23:56:00              0              0              0
#21990 2006-12-31 23:57:00              0              0              0
#21991 2006-12-31 23:58:00              0              0              0
#21992 2006-12-31 23:59:00              0              0              0

# ----- 2007
yr_2007 <- dbGetQuery(con, 
                      "SELECT Date, 
                      Time, 
                      Sub_metering_1, 
                      Sub_metering_2, 
                      Sub_metering_3 
                      FROM yr_2007")
str(yr_2007)
#'data.frame':	521669 obs. of  5 variables:
#$ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
#$ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

summary(yr_2007)
#Date               Time                Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Length:521669      Length:521669       Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#Class :character   Class :character    1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
#Mode  :character   Mode  :character    Median : 0.000   Median : 0.000   Median : 0.000  
#                                       Mean   : 1.232   Mean   : 1.638   Mean   : 5.795  
#                                       3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
#                                       Max.   :78.000   Max.   :78.000   Max.   :20.000  

head(yr_2007)
#        Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2007-01-01 00:00:00              0              0              0
#2 2007-01-01 00:01:00              0              0              0
#3 2007-01-01 00:02:00              0              0              0
#4 2007-01-01 00:03:00              0              0              0
#5 2007-01-01 00:04:00              0              0              0
#6 2007-01-01 00:05:00              0              0              0

tail(yr_2007)
#             Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#521664 2007-12-31 23:54:00              0              0             18
#521665 2007-12-31 23:55:00              0              0             18
#521666 2007-12-31 23:56:00              0              0             18
#521667 2007-12-31 23:57:00              0              0             18
#521668 2007-12-31 23:58:00              0              0             18
#521669 2007-12-31 23:59:00              0              0             18

## ----- 2008
yr_2008 <- dbGetQuery(con, 
                      "SELECT Date, 
                      Time, 
                      Sub_metering_1, 
                      Sub_metering_2, 
                      Sub_metering_3 
                      FROM yr_2008")
str(yr_2008)
#'data.frame':	526905 obs. of  5 variables:
#$ Date          : chr  "2008-01-01" "2008-01-01" "2008-01-01" "2008-01-01" ...
#$ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_3: num  18 18 18 18 18 17 18 18 18 18 ...

summary(yr_2008)
#Date               Time               Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Length:526905      Length:526905       Min.   : 0.00   Min.   : 0.000   Min.   : 0.000  
#Class :character   Class :character    1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000  
#Mode  :character   Mode  :character    Median : 0.00   Median : 0.000   Median : 1.000  
#                                       Mean   : 1.11   Mean   : 1.256   Mean   : 6.034  
#                                       3rd Qu.: 0.00   3rd Qu.: 1.000   3rd Qu.:17.000  
#                                       Max.   :80.00   Max.   :76.000   Max.   :31.000 

head(yr_2008)
#        Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2008-01-01 00:00:00              0              0             18
#2 2008-01-01 00:01:00              0              0             18
#3 2008-01-01 00:02:00              0              0             18
#4 2008-01-01 00:03:00              0              0             18
#5 2008-01-01 00:04:00              0              0             18
#6 2008-01-01 00:05:00              0              0             17

tail(yr_2008)
#             Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#526900 2008-12-31 23:54:00              0              0              0
#526901 2008-12-31 23:55:00              0              0              0
#526902 2008-12-31 23:56:00              0              0              0
#526903 2008-12-31 23:57:00              0              0              0
#526904 2008-12-31 23:58:00              0              0              0
#526905 2008-12-31 23:59:00              0              0              0

## ----- 2009
yr_2009 <- dbGetQuery(con, 
                      "SELECT Date, 
                      Time, 
                      Sub_metering_1, 
                      Sub_metering_2, 
                      Sub_metering_3 
                      FROM yr_2009")
str(yr_2009)
#'data.frame':	521320 obs. of  5 variables:
#$ Date          : chr  "2009-01-01" "2009-01-01" "2009-01-01" "2009-01-01" ...
#$ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

summary(yr_2009)
#Date               Time                Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Length:521320      Length:521320       Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#Class :character   Class :character    1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
#Mode  :character   Mode  :character     Median : 0.000   Median : 0.000   Median : 1.000  
#                                       Mean   : 1.137   Mean   : 1.136   Mean   : 6.823  
#                                       3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:18.000  
#                                       Max.   :82.000   Max.   :77.000   Max.   :31.000 

head(yr_2009)
#        Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2009-01-01 00:00:00              0              0              0
#2 2009-01-01 00:01:00              0              0              0
#3 2009-01-01 00:02:00              0              0              0
#4 2009-01-01 00:03:00              0              0              0
#5 2009-01-01 00:04:00              0              0              0
#6 2009-01-01 00:05:00              0              0              0

tail(yr_2009)
#             Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#521315 2009-12-31 23:54:00              0              0             18
#521316 2009-12-31 23:55:00              0              0             18
#521317 2009-12-31 23:56:00              0              0             19
#521318 2009-12-31 23:57:00              0              0             18
#521319 2009-12-31 23:58:00              0              0             18
#521320 2009-12-31 23:59:00              0              0             19

## ----- 2010
yr_2010 <- dbGetQuery(con, 
                      "SELECT Date, 
                      Time, 
                      Sub_metering_1, 
                      Sub_metering_2, 
                      Sub_metering_3 
                      FROM yr_2010")
str(yr_2010)
#'data.frame':	457394 obs. of  5 variables:
#$ Date          : chr  "2010-01-01" "2010-01-01" "2010-01-01" "2010-01-01" ...
#$ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_3: num  18 18 19 18 18 19 18 18 19 18 ...

summary(yr_2010)
#Date               Time                 Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Length:457394      Length:457394       Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000  
#Class :character   Class :character    1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 1.000  
#Mode  :character   Mode  :character    Median : 0.0000   Median : 0.000   Median : 1.000  
#                                       Mean   : 0.9875   Mean   : 1.102   Mean   : 7.244  
#                                       3rd Qu.: 0.0000   3rd Qu.: 1.000   3rd Qu.:18.000  
#                                       Max.   :88.0000   Max.   :80.000   Max.   :31.000 

head(yr_2010)
#        Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2010-01-01 00:00:00              0              0             18
#2 2010-01-01 00:01:00              0              0             18
#3 2010-01-01 00:02:00              0              0             19
#4 2010-01-01 00:03:00              0              0             18
#5 2010-01-01 00:04:00              0              0             18
#6 2010-01-01 00:05:00              0              0             19

tail(yr_2010)
#             Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#457389 2010-11-26 20:57:00              0              0              0
#457390 2010-11-26 20:58:00              0              0              0
#457391 2010-11-26 20:59:00              0              0              0
#457392 2010-11-26 21:00:00              0              0              0
#457393 2010-11-26 21:01:00              0              0              0
#457394 2010-11-26 21:02:00              0              0              0

## ----- COMBINED DATA SET ----- ##

meter_data <- bind_rows(yr_2007, yr_2008, yr_2009, yr_2010)

str(meter_data)
#'data.frame':	2027288 obs. of  5 variables:
#$ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
#$ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

summary(meter_data)
#Date               Time                Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Length:2027288     Length:2027288      Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#Class :character   Class :character    1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
#Mode  :character   Mode  :character    Median : 0.000   Median : 0.000   Median : 1.000  
#                                       Mean   : 1.121   Mean   : 1.289   Mean   : 6.448  
#                                       3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
#                                       Max.   :88.000   Max.   :80.000   Max.   :31.000  

head(meter_data)
#        Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2007-01-01 00:00:00              0              0              0
#2 2007-01-01 00:01:00              0              0              0
#3 2007-01-01 00:02:00              0              0              0
#4 2007-01-01 00:03:00              0              0              0
#5 2007-01-01 00:04:00              0              0              0
#6 2007-01-01 00:05:00              0              0              0

tail(meter_data)
#              Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#2027283 2010-11-26 20:57:00              0              0              0
#2027284 2010-11-26 20:58:00              0              0              0
#2027285 2010-11-26 20:59:00              0              0              0
#2027286 2010-11-26 21:00:00              0              0              0
#2027287 2010-11-26 21:01:00              0              0              0
#2027288 2010-11-26 21:02:00              0              0              0

######################
#     PREPROCESSING 
######################

## Combine Date and Time attribute values in a new attribute column
meter_data <-cbind(meter_data,
                   paste(meter_data$Date,
                         meter_data$Time), 
                   stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more then 5 attributes you will need to change the column number)
colnames(meter_data)[6] <-"DateTime"

## Move the DateTime attribute within the dataset
meter_data <- meter_data[,c(ncol(meter_data), 1:(ncol(meter_data)-1))]

summary(meter_data)
#Sub_metering_1   Sub_metering_2   Sub_metering_3 
#Kitchen                 Laundry  Water-heater/AC
#Min.   : 0.000   Min.   : 0.000   Min.   : 0.000 
#1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000 
#Median : 0.000   Median : 0.000   Median : 1.000  
#Mean   : 1.121   Mean   : 1.289   Mean   : 6.448  
#3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000
#Max.   :88.000   Max.   :80.000   Max.   :31.000  

sapply(meter_data, sd, na.rm=TRUE)
#Sub_metering_1 
#6.147370e+00 
#Sub_metering_2 
#5.786206e+00
#Sub_metering_3
#8.434118e+00

head(meter_data)
#             DateTime       Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2007-01-01 00:00:00 2007-01-01 00:00:00              0              0              0
#2 2007-01-01 00:01:00 2007-01-01 00:01:00              0              0              0
#3 2007-01-01 00:02:00 2007-01-01 00:02:00              0              0              0
#4 2007-01-01 00:03:00 2007-01-01 00:03:00              0              0              0
#5 2007-01-01 00:04:00 2007-01-01 00:04:00              0              0              0
#6 2007-01-01 00:05:00 2007-01-01 00:05:00              0              0              0

## Convert DateTime from character to POSIXct 
meter_data$DateTime <- as.POSIXct(meter_data$DateTime, 
                                  "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(meter_data$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(meter_data)
#'data.frame':	2027288 obs. of  6 variables:
#$ DateTime      : POSIXct, format: "2007-01-01 01:00:00" ...
#$ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
#$ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

## -- CREATING OF NEW ATTRIBUTES

## Create "year" attribute with lubridate
meter_data$year <- year(meter_data$DateTime)
meter_data$month <- month(meter_data$DateTime)
meter_data$quarter <- quarter(meter_data$DateTime)
meter_data$week <- week(meter_data$DateTime)
meter_data$day <- day(meter_data$DateTime)
meter_data$hour <- hour(meter_data$DateTime)


## ----- PLOTS ----- ##

# Plot all of sub-meter 1 data
plot(meter_data$Sub_metering_1)

# Subset the second week of 2008 - All observations (dplyr package)
houseWeek <- filter(meter_data, year == 2008 & week == 2)
plot(houseWeek$Sub_metering_1)

# Subset the 9th day of January 2008 - All observations (plotly package)
houseDay <- filter(meter_data, year == 2008 & month == 1 & day == 9)

# plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$DateTime, y = houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = "Laundry Room", mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (Watt-hours)"))

