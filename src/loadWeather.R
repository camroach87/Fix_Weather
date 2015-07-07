# Description: Loads weather data and converts to a tidy format for R processing
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
require(ggplot2)
require(zoo)



#### Init =====================================================================
awsDir <- "./data/raw/AWS"
sunDir <- "./data/raw/Daily_Sunshine"
synopDir <- "./data/raw/Synoptic_data"
plotDir <- "./plots/aws"
outputDir <- "./data/tidy"

dir.create(plotDir, F, T)
dir.create(outputDir, F, T)


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

# add NAs for missing time stamps (if there are any - doesn't look like it)
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







#### Calculate missing values =================================================

# TODO (Cameron): This might be better written as a function which takes ts, 
# variable with missing values and predictor variables as inputs. Have only set
# up to calculate temperature at the moment.

# Check for length of stretch of NAs
# as.data.frame.rle <- function(x, ...) do.call(data.frame, x)
# awsNaRuns <- awsData %>%
#   group_by(StationId) %>%
#   do(runs = rle(.$AirTemp)) %>%
#   do(data.frame(StationId = .$StationId,
#                 as.data.frame.rle(.$runs))) %>%
#   filter(is.na(values))

# calcNaRuns <- function(x) {
#   n <- length(x)
#   
#   run.df <- data.frame(value = rep(NA,n),
#                        length = rep(0,n))
#   df.idx <- 1
#   
#   for (i in 1:n) {
#     cat(i, "...\n")
#     value1 <- x[i]
#     value2 <- x[i+1]
#     
#     if (identical(value1, value2)) {
#       # Increase run length by 1
#       run.df$value[df.idx] <- value1
#       run.df$length[df.idx] <- run.df$length[df.idx] + 1
#     } else {
#       # End of run. Store and move to next value
#       run.df$value[df.idx] <- value1
#       run.df$length[df.idx] <- run.df$length[df.idx] + 1
#       df.idx <- df.idx + 1
#     }
#   }
#   
#   run.df <- run.df %>% filter(length!=0)
#   
#   return(run.df)
# }
# 
# awsNaRuns <- awsData %>% 
#   group_by(StationId) %>% 
#   do(data.frame(StationId = .$StationId,
#                 runs = calcNaRuns(.$AirTemp))) %>% 
#   filter(is.na(value))
# 
# 
# awsNaRuns %>%
#   #filter(lengths!=1) %>%
#   ggplot(aes(x=lengths)) + 
#   geom_histogram(binwidth=1) +
#   facet_wrap(~StationId) +
#   ggtitle("Histogram of consecutive runs of NAs")






# Imputation of missing data
awsData.imp <- awsData %>% 
  group_by(StationId) %>%
  do(data.frame(.,
                AirTemp.int = na.approx(.$AirTemp)))






# awsData.imp %>%
#   #filter(floor_date(ts, "month") == dmy("1/4/2004")) %>%
#   filter(year(ts) == 2004) %>%
#   ggplot() + 
#   geom_line(aes(x=ts, y=AirTemp.int), colour="red") +
#   geom_point(aes(x=ts, y=AirTemp), colour="blue") +
#   facet_wrap(~StationId)

















#### Analyse ==================================================================
stationPlots <- unique(awsData$StationId)
for (iS in stationPlots) {
  cat("Outputting plot for station number", iS, "...\n")
  
  naTimes <- awsData %>%
    filter(StationId==iS,
           is.na(AirTemp))
  awsData %>%
    filter(StationId==iS) %>%
    ggplot(aes(x=ts, y=AirTemp)) + 
    geom_point() +
    geom_vline(data = naTimes, aes(xintercept = as.numeric(ts)), colour="red", alpha=0.15) +
    ggtitle(paste(iS, "half-hourly air temperature data.")) +
    ggsave(file.path(plotDir, paste0("awsAirTemp", iS, ".png")), width=22, height=12)
}




#### Output tidy data =========================================================
awsData %>%
  write.csv()
