getwd()
setwd("/Users/Dana/Desktop")
dat <- read.csv("sample_data.csv",header=T)
str(dat)

library(dplyr)
library(tidyverse)
library(anytime)
library(lubridate)
library(magrittr)
library(data.table)

### First Question ###
BDV <- dat[which(dat$country_id=='BDV'), ]
str(BDV) # 844 obs with 4 variables
BDV %>% group_by(site_id) %>% summarize(count=n(), unique_ID=n_distinct(user_id))
# unique_ID shows number of unique users per each site (5NPAU has largest)


### Second Question ###
data2 <- dat
data2$ts <- gsub("/","-",data2$ts)
data2$ts %<>% parse_date_time(orders="dmy hm") 

s <- '2019-02-03 00:00:00' 
e <- '2019-02-04 23:59:59'

data3 <- data2 %>% filter(ts >as.POSIXct(s, tz='UTC') ) %>% filter(ts <as.POSIXct(e, tz='UTC'))
data4 <- data3 %>% group_by(user_id, site_id) %>% summarize(count=n())
data4[data4$count > 10,]
# 4 users visited a specific site for more than 10 times


### Third Qudstion ###
data2 %>% group_by(user_id) %>% filter(ts==max(ts)) %>% distinct(user_id,.keep_all = T) %>% 
  group_by(site_id) %>% summarise(unique_num_users=n()) %>% 
  arrange(desc(unique_num_users))


### Fourth Question ###
newdat <- data2 %>% group_by(user_id) %>% summarise(min = min(ts), max = max(ts)) #1916 unique users
same <- newdat[which(newdat$min==newdat$max), ]
different <- newdat[which(newdat$min!=newdat$max), ]
str(same) #1274 users whose first/last visits are to the same website.
str(different) #642 users whose first/last visits are to the different website


### Fifth Question ###
data2 %>% data.table
dataA <- data2 %>% group_by(user_id, country_id) %>% summarise(count=n())
dataB <- dataA$user_id %>% table
dataC <- dataB[dataB>1]
id_visit2 <- names(dataC)

data1 <- data2 %>% filter(user_id %in% id_visit2) %>% group_by(site_id) %>% 
  summarise(uniq_count_A=n_distinct(user_id))
data3 <- data2 %>% group_by(site_id) %>% summarise(uniq_count_B=n_distinct(user_id)) 

Answer1 <- merge(data1, data3) %>% mutate(ratio=uniq_count_B/uniq_count_A) %>% 
  arrange(desc(ratio))
Answer2 <- Answer1 %>% head(3)
Answer2 #QGO3G, 5NPAU, N0OTG
