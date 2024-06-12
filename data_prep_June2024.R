### Preparation ####

## Library
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(purrr) # for map() function
library(lubridate) # working with time & date format
library(data.table) # for %like%

## Read raw data (n=22,873)

# import re-sighting records (including banding information)
raw = read.csv("RBAll_95-23_20231207.csv", colClasses="character")
head(raw)
names(raw)[1]="UID" # TL: when I import the file, the name of the first column is ï..UID, therefore, this line of code is needed. Moreover, I changed the long connection line with a shorter one for the age class definition in the csv-file, as the longer connection lines were not recognized by my R version.

# number of records: 22,873
nrow(raw)


## Unify time format
time_vector = raw$start.time

unified_time = as.Date(time_vector, formats = c("%Y-%m-%d"))
unified_time[is.na(unified_time)] = as.Date(time_vector[is.na(unified_time)], formats = c("%Y/%m/%d"))

head(unified_time)
tail(unified_time)

# save in a new column
raw$observation.date = unified_time
head(raw)
# sort the records by time
raw = raw[order(raw$observation.date),]

## Extract banding information (1132 birds)

# banding info
band = raw[raw$retrieved.from=="BFSCN_B",c("UID","band.of.ind.","region_4","observation.date", "banded.age", "banded.ageclass", "note")]
colnames(band)[colnames(band)=="observation.date"] = "banded.date"


## Remove invalid records
# resighting DataFrame: extract necessary columns (including banding events)
res = raw[,c("UID","band.of.ind.", "region_4","observation.date", "latitude", "longitude", "retrieved.from")]

# remove records without observation date (n=22,873→22,871)
res = res[!is.na(res$observation.date), ]

# remove resighting records without bird id (n=22,871→22,869)
res = res[res$band.of.ind. != "", ]
res = res[!is.na(res$band.of.ind.), ]
# TL: when I run this code, n remains 22,873 # MML: I revised the line so that it can select and remove the 2 rows with empty strings

# join banding info to all resightings
BFS_all = left_join(res, band[,c(2,4,5,6)], by="band.of.ind.")
colnames(BFS_all)

# convert categorical variables into factors
BFS_all$UID = as.factor(BFS_all$UID)
BFS_all$band.of.ind. = as.factor(BFS_all$band.of.ind.)
BFS_all$region_4 = as.factor(BFS_all$region_4)
BFS_all$latitude = as.numeric(BFS_all$latitude)
BFS_all$longitude = as.numeric(BFS_all$longitude)
BFS_all$retrieved.from = as.factor(BFS_all$retrieved.from)
BFS_all$banded.age = as.numeric(BFS_all$banded.age) # NAs came from the unknown group
BFS_all$banded.ageclass = as.factor(BFS_all$banded.ageclass)


## calculate current age of each record (n=22,871→21,270)

# the bird's age at the time of each resighting = banded age + time difference between the observation and the banding date
BFS_all$current.age = BFS_all$banded.age + time_length(difftime(BFS_all$observation.date, BFS_all$banded.date), "years")

table(is.na(BFS_all$current.age))
table(floor(BFS_all$current.age))

# keep the records without definite age here for now to match the logic of the manuscript
# BFS_all = BFS_all[!is.na(BFS_all$current.age),]

# remove incorrect records that were earlier than banding 
BFS_all = BFS_all[-which(BFS_all$current.age < 0), ]

# number of records: 22,866
nrow(BFS_all)


## Duplicate and incorrect records (n=22,866→19,155)

# compare if the time is unique within the records from the same individual
BFS_all.UT = BFS_all %>%
  group_by(band.of.ind., observation.date) %>%
  mutate(
    time_status = case_when(
      n() > 1 & n_distinct(region_4) == 1 ~ "Repeated time, same region",
      n() > 1 & n_distinct(region_4) > 1 ~ "Repeated time, different regions",
      TRUE ~ "Unique time"
    )
  ) %>%
  ungroup()

# check the "Repeated time, different regions" group -> found 8 records of 4 individuals
# checked all resightings of these individuals and identified the incorrect records

# remove the incorrect records
BFS_all.UT.1 = BFS_all.UT[!BFS_all.UT$UID %in% c("BFSCN_008310","HKBWS_0750","HKBWS_0941","HKBWS_1022"),]


# number of records: 22,862
nrow(BFS_all.UT.1)

# keep only the first record on each date for each individual
BFS_all.UT.2 = BFS_all.UT.1 %>%
  group_by(band.of.ind., observation.date) %>%
  slice(1) %>%
  ungroup()

# number of records: 20,597
nrow(BFS_all.UT.2)

## Temporal range (n=20,597→19,785)
# Nov 2006–Aug 2023
BFS_all.T = BFS_all.UT.2[BFS_all.UT.2$observation.date >= "2006-11-1" & BFS_all.UT.2$observation.date <= "2023-8-31", ]

# number of records: 19,785
nrow(BFS_all.T)

# number of marked individuals: 1,068
length(unique(BFS_all.T$band.of.ind.))

## Temporal & Spatial range (n=19,785→13,598)
# exclude records during migration seasons (September, October, March, April) (n=6,187)
BFS_all.T.1 = BFS_all.T %>%
  filter(
    # Include data during the breeding (May to August) and wintering season (November to February)
    (month(observation.date) %in% c(1, 2, 5, 6, 7, 8, 11, 12)) 
  )
# exclude records from North China Coast(n=185)
BFS_all.T.R = BFS_all.T.1[!BFS_all.T.1$region_4=="N",]
# exclude records from other regions (n=57)
BFS_all.T.R.1 = BFS_all.T.R[!BFS_all.T.R$region_4=="O",]

# exclude atypical records (n=71+633)  

# calculate nrow of winter resightings from South Korea (n=73)
nrow(BFS_all.T.R.1 %>%
  filter(
    # Include South Korea data during the wintering season (November to February)
    (month(observation.date) %in% c(11, 12, 1, 2) & region_4 == "K")
  ))
# calculate nrow of summer resightings from Japan, South China Coast, and Taiwan (n=636)
nrow(BFS_all.T.R.1 %>%
       filter(
         # Include South Korea data during the wintering season (November to February)
         (month(observation.date) %in% c(5, 6, 7, 8) & region_4 %in% c("T", "S", "J"))
       ))
# keep records: breeding season - South Korea only & wintering season - Japan, South China, Taiwan only

BFS_all.T.R.2 = BFS_all.T.R.1 %>%
  filter(
    # Include South Korea data during the breeding season (May to August)
    (month(observation.date) %in% c(5, 6, 7, 8) & region_4 == "K") |
      # Include data from other specified regions (T, S, J) during the wintering season (November to February)
      (month(observation.date) %in% c(11, 12, 1, 2) & region_4 %in% c("T", "S", "J"))
  )


### Wintering region ####

## Check the regions where each individual was sighted in each season

# use a longer time scale (1994-2023), and finer seasonal intervals (1 Dec-28 Feb, 1 June-31 Aug) to determine the region in each sampling occasion
# check if individuals are sighted the same number of times in different regions during each season

check = BFS_all.UT.2 %>%  # BFS_all.UT.2 still has the out-of-date records 
  arrange(observation.date) %>%
  mutate(time = observation.date) %>% # Use mutate to create a new column named time, which takes the values from observation.date.
  mutate(SO = case_when( # Use mutate to create a new column named SO(Sampling Occasion), which identifies the corresponding season and year interval based on the values in time.
    time>=as.Date("1994/12/1")&time<=as.Date("1995/2/28")~"1994W",	time>=as.Date("1995/6/1")&time<=as.Date("1995/8/31")~"1995S",
    time>=as.Date("1995/12/1")&time<=as.Date("1996/2/29")~"1995W",	time>=as.Date("1996/6/1")&time<=as.Date("1996/8/31")~"1996S",
    time>=as.Date("1996/12/1")&time<=as.Date("1997/2/28")~"1996W",	time>=as.Date("1997/6/1")&time<=as.Date("1997/8/31")~"1997S",
    time>=as.Date("1997/12/1")&time<=as.Date("1998/2/28")~"1997W",	time>=as.Date("1998/6/1")&time<=as.Date("1998/8/31")~"1998S",
    time>=as.Date("1998/12/1")&time<=as.Date("1999/2/28")~"1998W",	time>=as.Date("1999/6/1")&time<=as.Date("1999/8/31")~"1999S",
    time>=as.Date("1999/12/1")&time<=as.Date("2000/2/29")~"1999W",	time>=as.Date("2000/6/1")&time<=as.Date("2000/8/31")~"2000S",
    time>=as.Date("2000/12/1")&time<=as.Date("2001/2/28")~"2000W",	time>=as.Date("2001/6/1")&time<=as.Date("2001/8/31")~"2001S",
    time>=as.Date("2001/12/1")&time<=as.Date("2002/2/28")~"2001W",	time>=as.Date("2002/6/1")&time<=as.Date("2002/8/31")~"2002S",
    time>=as.Date("2002/12/1")&time<=as.Date("2003/2/28")~"2002W",	time>=as.Date("2003/6/1")&time<=as.Date("2003/8/31")~"2003S",
    time>=as.Date("2003/12/1")&time<=as.Date("2004/2/29")~"2003W",	time>=as.Date("2004/6/1")&time<=as.Date("2004/8/31")~"2004S",
    time>=as.Date("2004/12/1")&time<=as.Date("2005/2/28")~"2004W",	time>=as.Date("2005/6/1")&time<=as.Date("2005/8/31")~"2005S",
    time>=as.Date("2005/12/1")&time<=as.Date("2006/2/28")~"2005W",	time>=as.Date("2006/6/1")&time<=as.Date("2006/8/31")~"2006S",
    time>=as.Date("2006/12/1")&time<=as.Date("2007/2/28")~"2006W",	time>=as.Date("2007/6/1")&time<=as.Date("2007/8/31")~"2007S",
    time>=as.Date("2007/12/1")&time<=as.Date("2008/2/29")~"2007W",	time>=as.Date("2008/6/1")&time<=as.Date("2008/8/31")~"2008S",
    time>=as.Date("2008/12/1")&time<=as.Date("2009/2/28")~"2008W",	time>=as.Date("2009/6/1")&time<=as.Date("2009/8/31")~"2009S",
    time>=as.Date("2009/12/1")&time<=as.Date("2010/2/28")~"2009W",	time>=as.Date("2010/6/1")&time<=as.Date("2010/8/31")~"2010S",
    time>=as.Date("2010/12/1")&time<=as.Date("2011/2/28")~"2010W",	time>=as.Date("2011/6/1")&time<=as.Date("2011/8/31")~"2011S",
    time>=as.Date("2011/12/1")&time<=as.Date("2012/2/29")~"2011W",	time>=as.Date("2012/6/1")&time<=as.Date("2012/8/31")~"2012S",
    time>=as.Date("2012/12/1")&time<=as.Date("2013/2/28")~"2012W",	time>=as.Date("2013/6/1")&time<=as.Date("2013/8/31")~"2013S",
    time>=as.Date("2013/12/1")&time<=as.Date("2014/2/28")~"2013W",	time>=as.Date("2014/6/1")&time<=as.Date("2014/8/31")~"2014S",
    time>=as.Date("2014/12/1")&time<=as.Date("2015/2/28")~"2014W",	time>=as.Date("2015/6/1")&time<=as.Date("2015/8/31")~"2015S",
    time>=as.Date("2015/12/1")&time<=as.Date("2016/2/29")~"2015W",	time>=as.Date("2016/6/1")&time<=as.Date("2016/8/31")~"2016S",
    time>=as.Date("2016/12/1")&time<=as.Date("2017/2/28")~"2016W",	time>=as.Date("2017/6/1")&time<=as.Date("2017/8/31")~"2017S",
    time>=as.Date("2017/12/1")&time<=as.Date("2018/2/28")~"2017W",	time>=as.Date("2018/6/1")&time<=as.Date("2018/8/31")~"2018S",
    time>=as.Date("2018/12/1")&time<=as.Date("2019/2/28")~"2018W",	time>=as.Date("2019/6/1")&time<=as.Date("2019/8/31")~"2019S",
    time>=as.Date("2019/12/1")&time<=as.Date("2020/2/29")~"2019W",	time>=as.Date("2020/6/1")&time<=as.Date("2020/8/31")~"2020S",
    time>=as.Date("2020/12/1")&time<=as.Date("2021/2/28")~"2020W",	time>=as.Date("2021/6/1")&time<=as.Date("2021/8/31")~"2021S",
    time>=as.Date("2021/12/1")&time<=as.Date("2022/2/28")~"2021W",	time>=as.Date("2022/6/1")&time<=as.Date("2022/8/31")~"2022S",
    time>=as.Date("2022/12/1")&time<=as.Date("2023/2/28")~"2022W",	time>=as.Date("2023/6/1")&time<=as.Date("2023/8/31")~"2023S",
    time>=as.Date("2023/12/1")&time<=as.Date("2024/2/28")~"2023W"
  )) %>%
  group_by(band.of.ind., SO ) %>% # Use group_by to group the data by band.of.ind. and SO.
  summarize(region_4 = list(region_4)) %>% # Use summarize to create a list named region_4, containing the regions for each individual within each time interval.
  mutate( # Use mutate and map_dbl to calculate the frequency of each region for each individual within each time interval.
    freq_count = map_dbl(region_4, ~{  # Use map_dbl to iterate over each element of region_4.
      freq = table(.x) # Creates a frequency table for each element in region_4 
      max_freq = max(freq) # Checks the maximum count in the frequency table
      sum(freq == max_freq) # Calculate the number of regions that have the maximum frequency. E.g. freq_count = 2 means there are 2 regions have the highest frequency.
    })) %>% 
  drop_na()


## Identify the most visited region in each season (keep the LATEST if tie)

# use a longer time scale (1994-2023), and finer seasonal intervals (1 Dec-28 Feb, 1 June-31 Aug) to determine the region in each sampling occasion

BFS_06_23_region_latest = BFS_all.UT.2 %>%
  # Convert observation.date to Date format
  mutate(time = as.Date(observation.date),
         region_4 = as.character(region_4)) %>%  # Convert region_4 to character
  mutate(SO = case_when(
    time>=as.Date("1994/12/1")&time<=as.Date("1995/2/28")~"1994W",	time>=as.Date("1995/6/1")&time<=as.Date("1995/8/31")~"1995S",
    time>=as.Date("1995/12/1")&time<=as.Date("1996/2/29")~"1995W",	time>=as.Date("1996/6/1")&time<=as.Date("1996/8/31")~"1996S",
    time>=as.Date("1996/12/1")&time<=as.Date("1997/2/28")~"1996W",	time>=as.Date("1997/6/1")&time<=as.Date("1997/8/31")~"1997S",
    time>=as.Date("1997/12/1")&time<=as.Date("1998/2/28")~"1997W",	time>=as.Date("1998/6/1")&time<=as.Date("1998/8/31")~"1998S",
    time>=as.Date("1998/12/1")&time<=as.Date("1999/2/28")~"1998W",	time>=as.Date("1999/6/1")&time<=as.Date("1999/8/31")~"1999S",
    time>=as.Date("1999/12/1")&time<=as.Date("2000/2/29")~"1999W",	time>=as.Date("2000/6/1")&time<=as.Date("2000/8/31")~"2000S",
    time>=as.Date("2000/12/1")&time<=as.Date("2001/2/28")~"2000W",	time>=as.Date("2001/6/1")&time<=as.Date("2001/8/31")~"2001S",
    time>=as.Date("2001/12/1")&time<=as.Date("2002/2/28")~"2001W",	time>=as.Date("2002/6/1")&time<=as.Date("2002/8/31")~"2002S",
    time>=as.Date("2002/12/1")&time<=as.Date("2003/2/28")~"2002W",	time>=as.Date("2003/6/1")&time<=as.Date("2003/8/31")~"2003S",
    time>=as.Date("2003/12/1")&time<=as.Date("2004/2/29")~"2003W",	time>=as.Date("2004/6/1")&time<=as.Date("2004/8/31")~"2004S",
    time>=as.Date("2004/12/1")&time<=as.Date("2005/2/28")~"2004W",	time>=as.Date("2005/6/1")&time<=as.Date("2005/8/31")~"2005S",
    time>=as.Date("2005/12/1")&time<=as.Date("2006/2/28")~"2005W",	time>=as.Date("2006/6/1")&time<=as.Date("2006/8/31")~"2006S",
    time>=as.Date("2006/12/1")&time<=as.Date("2007/2/28")~"2006W",	time>=as.Date("2007/6/1")&time<=as.Date("2007/8/31")~"2007S",
    time>=as.Date("2007/12/1")&time<=as.Date("2008/2/29")~"2007W",	time>=as.Date("2008/6/1")&time<=as.Date("2008/8/31")~"2008S",
    time>=as.Date("2008/12/1")&time<=as.Date("2009/2/28")~"2008W",	time>=as.Date("2009/6/1")&time<=as.Date("2009/8/31")~"2009S",
    time>=as.Date("2009/12/1")&time<=as.Date("2010/2/28")~"2009W",	time>=as.Date("2010/6/1")&time<=as.Date("2010/8/31")~"2010S",
    time>=as.Date("2010/12/1")&time<=as.Date("2011/2/28")~"2010W",	time>=as.Date("2011/6/1")&time<=as.Date("2011/8/31")~"2011S",
    time>=as.Date("2011/12/1")&time<=as.Date("2012/2/29")~"2011W",	time>=as.Date("2012/6/1")&time<=as.Date("2012/8/31")~"2012S",
    time>=as.Date("2012/12/1")&time<=as.Date("2013/2/28")~"2012W",	time>=as.Date("2013/6/1")&time<=as.Date("2013/8/31")~"2013S",
    time>=as.Date("2013/12/1")&time<=as.Date("2014/2/28")~"2013W",	time>=as.Date("2014/6/1")&time<=as.Date("2014/8/31")~"2014S",
    time>=as.Date("2014/12/1")&time<=as.Date("2015/2/28")~"2014W",	time>=as.Date("2015/6/1")&time<=as.Date("2015/8/31")~"2015S",
    time>=as.Date("2015/12/1")&time<=as.Date("2016/2/29")~"2015W",	time>=as.Date("2016/6/1")&time<=as.Date("2016/8/31")~"2016S",
    time>=as.Date("2016/12/1")&time<=as.Date("2017/2/28")~"2016W",	time>=as.Date("2017/6/1")&time<=as.Date("2017/8/31")~"2017S",
    time>=as.Date("2017/12/1")&time<=as.Date("2018/2/28")~"2017W",	time>=as.Date("2018/6/1")&time<=as.Date("2018/8/31")~"2018S",
    time>=as.Date("2018/12/1")&time<=as.Date("2019/2/28")~"2018W",	time>=as.Date("2019/6/1")&time<=as.Date("2019/8/31")~"2019S",
    time>=as.Date("2019/12/1")&time<=as.Date("2020/2/29")~"2019W",	time>=as.Date("2020/6/1")&time<=as.Date("2020/8/31")~"2020S",
    time>=as.Date("2020/12/1")&time<=as.Date("2021/2/28")~"2020W",	time>=as.Date("2021/6/1")&time<=as.Date("2021/8/31")~"2021S",
    time>=as.Date("2021/12/1")&time<=as.Date("2022/2/28")~"2021W",	time>=as.Date("2022/6/1")&time<=as.Date("2022/8/31")~"2022S",
    time>=as.Date("2022/12/1")&time<=as.Date("2023/2/28")~"2022W",	time>=as.Date("2023/6/1")&time<=as.Date("2023/8/31")~"2023S",
    time>=as.Date("2023/12/1")&time<=as.Date("2024/2/28")~"2023W",
    TRUE ~ NA_character_
  )) %>%
  group_by(band.of.ind., SO) %>%
  summarize(
    regions = list(region_4),
    dates = list(time),
    .groups = 'drop'
  ) %>%
  mutate(region_4 = map2(regions, dates, ~{
    region_list <- .x
    date_list <- .y
    freq <- table(region_list)
    max_freq <- max(freq)
    tied_regions <- names(freq[freq == max_freq])
    
    if (length(tied_regions) > 1) {
      tied_dates <- date_list[region_list %in% tied_regions]
      latest_date <- max(tied_dates)
      latest_region_indices <- which(region_list %in% tied_regions & date_list == latest_date)
      latest_region <- region_list[latest_region_indices[1]]
      
      if (length(unique(date_list[latest_region_indices])) > 1) {
        return("Check")
      } else {
        return(latest_region)
      }
    } else {
      return(tied_regions)
    }
  })) %>%
  ungroup() %>%
  arrange(SO) %>%
  pivot_wider(
    names_from = SO, 
    values_from = region_4,
    values_fill = list("-"),
    id_cols = band.of.ind.
  )

## Identify the most visited region in each winter (keep the SOUTHERNMOST if tie) 
BFS_06_23_winter = BFS_all.UT.2 %>%
  # Convert observation.date to Date format
  mutate(time = as.Date(observation.date),
         region_4 = as.character(region_4)) %>%  # Convert region_4 to character
  mutate(SO = case_when(
    time>=as.Date("1994/12/1")&time<=as.Date("1995/2/28")~"1994W",	
    time>=as.Date("1995/12/1")&time<=as.Date("1996/2/29")~"1995W",	
    time>=as.Date("1996/12/1")&time<=as.Date("1997/2/28")~"1996W",	
    time>=as.Date("1997/12/1")&time<=as.Date("1998/2/28")~"1997W",	
    time>=as.Date("1998/12/1")&time<=as.Date("1999/2/28")~"1998W",	
    time>=as.Date("1999/12/1")&time<=as.Date("2000/2/29")~"1999W",
    time>=as.Date("2000/12/1")&time<=as.Date("2001/2/28")~"2000W",	
    time>=as.Date("2001/12/1")&time<=as.Date("2002/2/28")~"2001W",	
    time>=as.Date("2002/12/1")&time<=as.Date("2003/2/28")~"2002W",	
    time>=as.Date("2003/12/1")&time<=as.Date("2004/2/29")~"2003W",	
    time>=as.Date("2004/12/1")&time<=as.Date("2005/2/28")~"2004W",	
    time>=as.Date("2005/12/1")&time<=as.Date("2006/2/28")~"2005W",	
    time>=as.Date("2006/12/1")&time<=as.Date("2007/2/28")~"2006W",	
    time>=as.Date("2007/12/1")&time<=as.Date("2008/2/29")~"2007W",	
    time>=as.Date("2008/12/1")&time<=as.Date("2009/2/28")~"2008W",	
    time>=as.Date("2009/12/1")&time<=as.Date("2010/2/28")~"2009W",	
    time>=as.Date("2010/12/1")&time<=as.Date("2011/2/28")~"2010W",	
    time>=as.Date("2011/12/1")&time<=as.Date("2012/2/29")~"2011W",	
    time>=as.Date("2012/12/1")&time<=as.Date("2013/2/28")~"2012W",	
    time>=as.Date("2013/12/1")&time<=as.Date("2014/2/28")~"2013W",	
    time>=as.Date("2014/12/1")&time<=as.Date("2015/2/28")~"2014W",	
    time>=as.Date("2015/12/1")&time<=as.Date("2016/2/29")~"2015W",	
    time>=as.Date("2016/12/1")&time<=as.Date("2017/2/28")~"2016W",	
    time>=as.Date("2017/12/1")&time<=as.Date("2018/2/28")~"2017W",	
    time>=as.Date("2018/12/1")&time<=as.Date("2019/2/28")~"2018W",	
    time>=as.Date("2019/12/1")&time<=as.Date("2020/2/29")~"2019W",	
    time>=as.Date("2020/12/1")&time<=as.Date("2021/2/28")~"2020W",	
    time>=as.Date("2021/12/1")&time<=as.Date("2022/2/28")~"2021W",	
    time>=as.Date("2022/12/1")&time<=as.Date("2023/2/28")~"2022W",	
    time>=as.Date("2023/12/1")&time<=as.Date("2024/2/28")~"2023W",
    TRUE ~ NA_character_
  )) %>%
  group_by(band.of.ind., SO) %>%
  summarize(
    regions = list(region_4),
    lat = list(latitude),
    .groups = 'drop'
  ) %>%
  mutate(region_4 = map2(regions, lat, ~{
    region_list <- .x
    lat_list <- .y
    freq <- table(region_list)
    max_freq <- max(freq)
    tied_regions <- names(freq[freq == max_freq])
    
    if (length(tied_regions) > 1) {
      tied_lat <- lat_list[region_list %in% tied_regions]
      southernmost_lat <- min(tied_lat, na.rm = TRUE)
      southernmost_region_indices <- which(region_list %in% tied_regions & lat_list == southernmost_lat)
      southernmost_region <- region_list[southernmost_region_indices[1]]
      
      if (length(unique(lat_list[southernmost_region_indices])) > 1) {
        return("Check")
      } else {
        return(southernmost_region)
      }
    } else {
      return(tied_regions)
    }
  })) %>%
  ungroup() %>%
  arrange(SO) %>%
  pivot_wider(
    names_from = SO, 
    values_from = region_4,
    values_fill = list("-"),
    id_cols = band.of.ind.
  )


## Identify the most representative wintering region for each individual

# choose the wintering region that a bird has visited the most times
BFS_06_23_winter$most_visited =
  apply(BFS_06_23_winter[, -c(1,28)], 1, function(x) {
    # ignore "-"
    freq = table(factor(x[x != "-"], unique(x[x != "-"])))
    if (length(freq) == 0) {
      return(NA)
    }
    # return "unclear" if the counts are the same
    max_freq = max(freq)
    if (sum(freq == max_freq) > 1) {
      return("unclear")
    }
    names(which.max(freq))
  })

# check the number of individuals in each wintering region
table(BFS_06_23_winter[,"most_visited"])


## Create the wintering region table

# extract "band.of.ind." & "most_visited"
winter = BFS_06_23_winter[,c("band.of.ind.","most_visited")] # NA = never been seen during non-breeding seasons
table(winter$most_visited)

### Encounter history ####
## select records within sampling occasions: Winter (Nov–Feb), Summer (May–Aug)
BFS_06_23_count = BFS_all.T.R.2 %>%
  mutate(time = as.Date(observation.date)) %>%
  mutate(SO = case_when(  # set criteria for sampling occasions 
    time>=as.Date("2006/11/1")&time<=as.Date("2007/2/28")~"2006W",	time>=as.Date("2007/5/1")&time<=as.Date("2007/8/31")~"2007S",
    time>=as.Date("2007/11/1")&time<=as.Date("2008/2/29")~"2007W",	time>=as.Date("2008/5/1")&time<=as.Date("2008/8/31")~"2008S",
    time>=as.Date("2008/11/1")&time<=as.Date("2009/2/28")~"2008W",	time>=as.Date("2009/5/1")&time<=as.Date("2009/8/31")~"2009S",
    time>=as.Date("2009/11/1")&time<=as.Date("2010/2/28")~"2009W",	time>=as.Date("2010/5/1")&time<=as.Date("2010/8/31")~"2010S",
    time>=as.Date("2010/11/1")&time<=as.Date("2011/2/28")~"2010W",	time>=as.Date("2011/5/1")&time<=as.Date("2011/8/31")~"2011S",
    time>=as.Date("2011/11/1")&time<=as.Date("2012/2/29")~"2011W",	time>=as.Date("2012/5/1")&time<=as.Date("2012/8/31")~"2012S",
    time>=as.Date("2012/11/1")&time<=as.Date("2013/2/28")~"2012W",	time>=as.Date("2013/5/1")&time<=as.Date("2013/8/31")~"2013S",
    time>=as.Date("2013/11/1")&time<=as.Date("2014/2/28")~"2013W",	time>=as.Date("2014/5/1")&time<=as.Date("2014/8/31")~"2014S",
    time>=as.Date("2014/11/1")&time<=as.Date("2015/2/28")~"2014W",	time>=as.Date("2015/5/1")&time<=as.Date("2015/8/31")~"2015S",
    time>=as.Date("2015/11/1")&time<=as.Date("2016/2/29")~"2015W",	time>=as.Date("2016/5/1")&time<=as.Date("2016/8/31")~"2016S",
    time>=as.Date("2016/11/1")&time<=as.Date("2017/2/28")~"2016W",	time>=as.Date("2017/5/1")&time<=as.Date("2017/8/31")~"2017S",
    time>=as.Date("2017/11/1")&time<=as.Date("2018/2/28")~"2017W",	time>=as.Date("2018/5/1")&time<=as.Date("2018/8/31")~"2018S",
    time>=as.Date("2018/11/1")&time<=as.Date("2019/2/28")~"2018W",	time>=as.Date("2019/5/1")&time<=as.Date("2019/8/31")~"2019S",
    time>=as.Date("2019/11/1")&time<=as.Date("2020/2/29")~"2019W",	time>=as.Date("2020/5/1")&time<=as.Date("2020/8/31")~"2020S",
    time>=as.Date("2020/11/1")&time<=as.Date("2021/2/28")~"2020W",	time>=as.Date("2021/5/1")&time<=as.Date("2021/8/31")~"2021S",
    time>=as.Date("2021/11/1")&time<=as.Date("2022/2/28")~"2021W",	time>=as.Date("2022/5/1")&time<=as.Date("2022/8/31")~"2022S",
    time>=as.Date("2022/11/1")&time<=as.Date("2023/2/28")~"2022W",	time>=as.Date("2023/5/1")&time<=as.Date("2023/8/31")~"2023S"
  )) %>%
  group_by(band.of.ind., SO, ) %>%
  summarize(records = n()) %>%
  arrange(SO) %>%  # sort columns by sampling occasions
  pivot_wider(names_from = SO, values_from = records, values_fill = 0)%>%
  mutate_all(~ ifelse(. > 0, 1, 0)) # fill with 1 if any record exists in each sampling occasion

# number of individuals: 1,028
nrow(BFS_06_23_count)


## Make all encounter histories started in winter (n=1,028→569 individuals)

# get the digit of the first "1"
start_index = numeric(nrow(BFS_06_23_count))       # creates a new empty vector


for (i in 1:nrow(BFS_06_23_count)) {
  encounter_indices = which(BFS_06_23_count[i,] == "1") # find indices of "1" in each row
  if (length(encounter_indices) > 0) {
    start_index[i] = min(encounter_indices) - 1 # subtract 1 to account for bird id in the first column
  } else {
    start_index[i] = NA # assign NA if no "1" is found in the row
  }
}



# add "start_index" as a new column
BFS_06_23_count.1 = cbind(BFS_06_23_count, start_index) 

# rename the new column
colnames(BFS_06_23_count.1)[36] = "start_index"


# check each bird's "start_index" with the following loop
# the value is ODD -> started in winter -> skip to the next row
# the value is EVEN -> started in summer -> replace the first "1" with "0" 
BFS_06_23_count.2 = as.data.frame(BFS_06_23_count.1)

# Iterating over each bird's encounter history
for (i in 1:nrow(BFS_06_23_count.2)) {
  if (BFS_06_23_count.2$start_index[i] %% 2 == 1) {
    # Skip processing if the first '1' indicates a winter start (odd start_index)
    next
  } else { # A summer start 
    # Get the season indexes of all the winter encounters of each bird
    winter_spotted_indices = which(BFS_06_23_count.2[i, 2:35] == 1 & (2:35) %% 2 == 0)
    # If there are at least one winter encounter
    if (length(winter_spotted_indices) > 0) { 
      # Replace '1's before the first winter '1' with '0's
      BFS_06_23_count.2[i, 2:(winter_spotted_indices[1])+1-1] = 0
    } else { 
      # If no '1's are found in winter columns, set the encounter history to "0"
      BFS_06_23_count.2[i, 2:35] = 0
    }
  }
}


# remove individuals with no resightings
BFS_06_23_count.3 = BFS_06_23_count.2[rowSums(BFS_06_23_count.2[, 2:35]) != 0, ]

# number of individuals: 618
nrow(BFS_06_23_count.3)

## Encounter history
BFS_06_23_count.4 = BFS_06_23_count.3 %>%
  rowwise() %>%
  mutate(ch = paste0(c_across(2:35), collapse = "")) %>%
  ungroup()

## First sighted age 
# the age of each individual when it was first sighted 

BFS_first_age <- BFS_all.T.R.2 %>%
  arrange(band.of.ind., observation.date) %>%
  group_by(band.of.ind.) %>%
  mutate(month = as.numeric(format(observation.date, "%m"))) %>%
  filter(month %in% c(11, 12, 1, 2)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-month) # don't need the month column in the final dataset


### RMark format ####

## Combine information (n=618→492 individuals)

# combining bird id, encounter history, wintering region and first sighted age together
merged = merge(BFS_06_23_count.4[,c("band.of.ind.","ch"),], winter[, c("band.of.ind.","most_visited")], by = "band.of.ind.", all.x = TRUE)
merged2 = merge(merged, BFS_first_age[, c("band.of.ind.","current.age")], by = "band.of.ind.", all.x = TRUE)

# remove individuals with unknown wintering regions (seen in Nov, can't assign wintering region) (n=55)
merged3 = merged2[!is.na(merged2$most_visited),]

# remove individuals with unclear & atypical regions
merged4 = merged3[!(merged3$most_visited %in% c("K", "N", "O", "unclear")), ]

# remove individuals with unclear age
merged5 = merged4[!is.na(merged4$current.age),]

# number of individuals: 497 (TL: 497) #MML: fixed
nrow(merged5)


# set id as rownames
merged6 = merged5
rownames(merged6) =  merged6$band.of.ind.
merged6 = merged6[,-1]

# save as RMark input data frame
write.csv(merged6, "merged6_June24.csv", row.names=TRUE)


### Mapping the data points ####

# select resighting records with BFS_all.UT.2$region_4 = merged6$most_visited
# use BFS_all.UT.2 because some birds' wintering regions were determined using records before 2006
map = merge(BFS_all.UT.2, merged5, by = "band.of.ind.", all.y = FALSE)

map.1 = map[map$region_4==map$most_visited,]

# remove records without latitude
map.2 = map.1[!is.na(map.1$latitude),]

# select the southernmost
map.3 = map.2 %>% 
  group_by(band.of.ind.) %>% 
  slice(which.min(latitude))

# compare marked individuals with merged.5 -> 5 birds missing because their only winter sighting was banding records with no coordinates 

length(unique(map.3$band.of.ind.))
missing_id = setdiff(merged5$band.of.ind., unique(map.3$band.of.ind.))

# add coordinates based on description manually
missing = map.1[map.1$band.of.ind. %in% missing_id,]

missing[missing$band.of.ind.=="A45", 5:6] = c(22.51005042, 114.0667395)  
missing[missing$band.of.ind.=="J14", 5:6] = c(26.1721, 127.6561)
missing[missing$band.of.ind.=="J16", 5:6] = c(26.1951287,127.6783528)
missing[missing$band.of.ind.=="N43", 5:6] = c(23.17488, 120.1128)
missing[missing$band.of.ind.=="TX2", 5:6] = c(23.073004, 120.120903)

# rbind missing data.frame with map.3
map.4 = rbind(missing, map.3)

# save
write.csv(map.4, "map.4.csv")
