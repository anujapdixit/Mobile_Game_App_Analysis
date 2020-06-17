
## -----------------------
# IMPORT LIBS
library(tidyverse)
library(data.table)
library(lubridate)
#install.packages('tidytext')
library(tidytext)

library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages('formattable')
library(formattable)
#install.packages('janitor')
library(janitor)
## -----------------------



#
## -----------------------
# IMPORT FILE
setwd("/Users/MasterDK-Laptop/Desktop/Jupyter_Notebooks_1A/JupyterNB-winter/Bana277-Jup/Project")


# Set df
data <- read.csv(file.choose(), stringsAsFactors=FALSE, header = T)

#  CHANGE THE COL NAMES
data <- data %>%
  clean_names("snake")
## -----------------------



## -----------------------
# CHANGING VARIABLES
data2 = data %>% drop_na("user_rating_count")
# here we are creating a our treatment variable from zero subscriber friends = 0,
# and 1 or more =1
data2$rating = ifelse(data2$average_user_rating > 4, 1, 0)
data2$ratedadult = ifelse(data2$age_rating > "17+", 1, 0)
data2$subtitleyes   =  ifelse(data2$subtitle =='', 0, 1)

data2 = data2 %>% select(-age_rating, age_rating)

# data2$month_updated <- format(data2$current_version_release_date, '%b')

data2$original_release_date <- dmy(data2$original_release_date)
data2$current_version_release_date <- dmy(data2$current_version_release_date)

data2$tenure = (today(tzone = "") - data2$original_release_date)
data2$tenure = as.numeric(data2$tenure)
data2$latestversion = (today(tzone = "") - data2$current_version_release_date)
data2$latestversion = as.numeric(data2$latestversion)
# data2$langcount =

data2$in_app_purchases = as.double(data2$in_app_purchases)
data2$in_app_purchases = data2$in_app_purchases %>% replace_na(0)
data2$freetostart = ifelse(data2$price == 0, 1, 0)
data2$freetoplay = ifelse(data2$price == 0 & data2$in_app_purchases == 0, 1, 0)
# data2$playnowpaylater = ifelse(data2$price == 0.00 & data2$in_app_purchases ==
# 0.00)
data2$in_app_purchasesyes = ifelse(data2$in_app_purchases == 0.00, 0,1) 

# TURNING INTO FACTORS
data2$rating = as.factor(data2$rating)
data2$ratedadult = as.factor(data2$ratedadult)
data2$age_rating = as.factor(data2$age_rating)
data2$primary_genre = as.factor(data2$primary_genre)
data2$genres = as.factor(data2$genres)

glimpse(data2)
## -----------------------


## -----------------------
logitdata = data2
glimpse(logitdata)


modpart = glm(rating ~ 
                user_rating_count +
                #               price +
                size +
                ratedadult +
                tenure +
                latestversion +
                #               freetostart +
                #               freetoplay +
                #               in_app_purchasesyes +
                subtitleyes,
              data = logitdata, family=binomial)
summary(modpart)
## -----------------------




