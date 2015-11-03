# Description: Loads weather data and converts to a tidy format for R processing
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
require(ggplot2)
require(zoo)
require(stringr)



#### Init =====================================================================
awsDir <- "./data/raw/AWS"
sunDir <- "./data/raw/Daily_Sunshine"
synopDir <- "./data/raw/Synoptic_data"

dir.create("./env", F, T)


#### Load data ================================================================

# Load AWS data
awsFiles <- list.files(awsDir) %>%
  data.frame(file = ., stringsAsFactors = F) %>%
  filter(grepl("Data", file)) %>%
  .$file

awsData <- NULL
for (iF in awsFiles) {
  cat("Loading", iF, "...\n")
  
  awsData <- read.csv(file.path(awsDir, iF), stringsAsFactors=F) %>%
    select(-hm) %>%
    rename(StationId = Station.Number,
           Year = Year.Month.Day.Hour.Minutes.in.YYYY,
           Month = MM,
           Day = DD,
           Hour = HH24,
           Minute = MI.format.in.Local.time,
           Year1 = Year.Month.Day.Hour.Minutes.in.YYYY.1,
           Month1 = MM.1,
           Day1 = DD.1,
           Hour1 = HH24.1,
           Minute1 = MI.format.in.Local.standard.time,
           Precip = Precipitation.since.9am.local.time.in.mm,
           PrecipQ = Quality.of.precipitation.since.9am.local.time,
           AirTemp = Air.Temperature.in.degrees.C,
           AirTempQ = Quality.of.air.temperature,
           DewTemp = Dew.point.temperature.in.degrees.C,
           DewTempQ = Quality.of.dew.point.temperature,
           RelHumidity = Relative.humidity.in.percentage..,
           RelHumidityQ = Quality.of.relative.humidity,
           WindSpeed = Wind.speed.in.km.h,
           WindSpeedQ = Wind.speed.quality,
           WindDir = Wind.direction.in.degrees.true,
           WindDirQ = Wind.direction.quality,
           MaxGust = Speed.of.maximum.windgust.in.last.10.minutes.in..km.h,
           MaxGustQ = Quality.of.speed.of.maximum.windgust.in.last.10.minutes,
           AWS = AWS.Flag
           ) %>%
    mutate(Precip = as.numeric(Precip), # "#####" characters in some files
           MaxGust = as.numeric(MaxGust), # "###" characters in some files
           ts.ds = ymd_hm(paste(Year, Month, Day, Hour, Minute)), # Daylight savings time
           ts = ymd_hm(paste(Year1, Month1, Day1, Hour1, Minute1))) %>%
    select(-c(Year, Month, Day, Hour, Minute, Year1, Month1, Day1, Hour1, Minute1, X., ts.ds)) %>%
    #select(-c(Year, Month, Day, Hour, Minute, Year1, Month1, Day1, Hour1, Minute1, X.)) %>%
    filter(!is.na(AirTemp)) %>% #Filter all NAs. NAs between actual values are added again later.
    bind_rows(awsData)
}

# filter for quick analysis/checks 
# awsData <- awsData %>%
#   filter(StationId==61055)

# add NAs for missing time stamps
awsTs <- awsData %>%
  select(StationId, ts) %>%
  group_by(StationId) %>%
  summarise(tsStart = min(ts),
            tsEnd = max(ts)) %>%
  group_by(StationId) %>%
  do(data.frame(StationId = .$StationId,
                ts = seq(.$tsStart, .$tsEnd, 30*60))) #half-hourly intervals
awsData <- full_join(awsData, awsTs) %>%
  arrange(StationId, ts)





# Load AWS station details
awsDet <-  read.csv(file.path(awsDir, "HM01X_StnDet_999999998672034.txt"), stringsAsFactors=F, header=F) %>% 
  select(2:13) %>% 
  rename(StationId = V2,
         RainfallDistrict = V3,
         StationName = V4,
         OpenDate = V5,
         CloseDate = V6,
         Latitude = V7,
         Longitude = V8,
         LatLongMethod = V9,
         Region = V10,
         StationHeightAboveSea = V11,
         BaromHeightAboveSea = V12,
         WMOIndex = V13) %>% 
  mutate(StationName = str_trim(StationName))














# THESE DATA SOURCES ARE USED TO AUGMENT HALF-HOURLY AND TO HELP FILL IN BLANKS
# CURRENTLY NOT USED - BASING IMPUTATION ON HALF-HOURLY DATA ONLY

# Load daily sunshine data
sunFiles <- list.files(sunDir) %>%
  data.frame(file = ., stringsAsFactors = F) %>%
  filter(grepl("Data", file)) %>%
  .$file

sunData <- NULL
for (iF in sunFiles) {
  cat("Loading", iF, "...\n")
  
  sunData <- read.csv(file.path(sunDir, iF), stringsAsFactors=F) %>%
    rename(StationId = Station.Number,
           Sunshine = Number.of.hours.of.bright.sunshine.in.the.24.hours.midnight.to.midnight.Local.Time,
           SunshineQ = Quality.of.sunshine.information
    ) %>%
    mutate(Date = ymd(paste(Year, Month, Day))) %>%
    select(-c(dc, Year, Month, Day, X.)) %>% 
    bind_rows(sunData)
}


sunDet <-  read.csv(file.path(sunDir, "DC02D_StnDet_999999998672036.txt"), stringsAsFactors=F) %>% 
  select(2:13) %>% 
  rename(StationId = Bureau.of.Meteorology.Station.Number,
         RainfallDistrict = Rainfall.district.code,
         StationName = Station.Name,
         OpenDate = Month.Year.site.opened...MM.YYYY.,
         CloseDate = Month.Year.site.closed...MM.YYYY.,
         Latitude = Latitude.to.4.decimal.places.in.decimal.degrees,
         Longitude = Longitude.to.4.decimal.places.in.decimal.degrees,
         LatLongMethod = Method.by.which.latitude.longitude.was.derived,
         Region = State,
         StationHeightAboveSea = Height.of.station.above.mean.sea.level.in.metres,
         BaromHeightAboveSea = Height.of.barometer.above.mean.sea.level.in.metres,
         WMOIndex = WMO..World.Meteorological.Organisation..Index.Number) %>% 
  mutate(StationName = str_trim(StationName))






# Load synoptic data (3 hour weather data)
synopFiles <- list.files(synopDir) %>%
  data.frame(file = ., stringsAsFactors = F) %>%
  filter(grepl("Data", file)) %>%
  .$file

synopData <- NULL
for (iF in synopFiles) {
  cat("Loading", iF, "...\n")
  
  synopData <- read.csv(file.path(synopDir, iF), stringsAsFactors=F) %>%
    rename(StationId = Station.Number,
           Minute = Minute.in.Local.Time,
           Year1 = Year.1,
           Month1 = Month.1,
           Day1 = Day.1,
           Hour1 = Hour.1,
           Minute1 = Minute.in.Local.Standard.Time,
           Precip = Precipitation.in.mm,
           PrecipQ = Quality.of.precipitation,
           AirTemp = Air.temperature.in.Degrees.C,
           AirTempQ = Quality.of.air.temperature,
           DewTemp = Dew.point.temperature.in.Degrees.C,
           DewTempQ = Quality.of.dew.point.temperature,
           RelHumidity = Relative.humidity.in.percentage..,
           RelHumidityQ = Quality.of.relative.humidity,
           WindSpeed = Wind.speed.measured.in.km.h,
           WindSpeedQ = Quality.of.wind.speed,
           WindDir = Wind.direction.measured.in.degrees,
           WindDirQ = Quality.of.wind.direction,
           CloudAmount = Total.cloud.amount.in.eighths,
           CloudAmountQ = Quality.of.total.cloud.amount
    ) %>%
    mutate(ts.ds = ymd_hm(paste(Year, Month, Day, Hour, Minute)), # Daylight savings time
           ts = ymd_hm(paste(Year1, Month1, Day1, Hour1, Minute1))) %>%
    select(-c(hc, Year, Month, Day, Hour, Minute, Year1, Month1, Day1, Hour1, Minute1, X.)) %>%
    bind_rows(synopData)
}


save.image("./env/weather.RData")
