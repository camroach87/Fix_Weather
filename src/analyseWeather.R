# Description: Plots raw data characteristics. Calculates missing values.
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
require(ggplot2)
require(zoo)
require(xts)
require(dygraphs)

load("./env/weather.RData")

awsData <- awsData %>% 
  filter(ts >= dmy("1/1/2000"),
         StationId %in% c(86071, 86077, 86282, 86338, 87031))
  

#### Init =====================================================================
plotDir <- "./plots/aws"
outputDir <- "./data/tidy"

dir.create(plotDir, F, T)
dir.create(outputDir, F, T)




#### Plot raw data ============================================================
stationPlots <- unique(awsData$StationId)

# Highlights missing half-hourly values
for (iS in stationPlots) {
  cat("Outputting plot for station number", iS, "...\n")
  
  naTimes <- awsData %>%
    filter(StationId==iS,
           is.na(AirTemp))
#   p1 <- awsData %>%
#     filter(StationId==iS) %>%
#     ggplot(aes(x=ts, y=AirTemp)) + 
#     geom_point() +
#     geom_vline(data = naTimes, aes(xintercept = as.numeric(ts)), 
#                colour="red", alpha=0.15) +
#     ggtitle(paste(iS, "half-hourly air temperature data."))
#   ggsave(file.path(plotDir, paste0("awsAirTemp", iS, ".png")), p1,
#          width=22, height=12)
  
  png(file.path(plotDir, paste0("awsAirTemp", iS, ".png")),
      width=20, height=12, units = "in", res=75)
  awsData %>%
    filter(StationId==iS) %>%
    with(plot(ts, AirTemp,
              main=paste(iS, "half-hourly air temperature data.")))
  abline(v = naTimes$ts, col = rgb(1,0,0,0.15))
  dev.off()
}

# Shows plot of number of NAs per month
awsData %>% 
  mutate(ts.YM = floor_date(ts, "month")) %>% 
  group_by(StationId, ts.YM) %>% 
  summarise(na.count = sum(is.na(AirTemp))) %>% 
  ggplot(aes(x=ts.YM, y=na.count)) + 
  geom_line() +
  facet_wrap(~StationId) +
  scale_y_log10() +
  ggtitle("Number of NAs in each month.") +
  ylab("log(NA count)") +
  ggsave(file.path(plotDir, paste("naCount.png")), 
         width=sqrt(2)*10, height=10)


# Check for length of stretch of NAs
as.data.frame.rle <- function(x, ...) do.call(data.frame, x)
awsNaRuns <- awsData %>%
  group_by(StationId) %>%
  mutate(na.test = is.na(AirTemp)) %>% 
  do(runs = rle(.$na.test)) %>%
  do(data.frame(StationId = .$StationId,
                as.data.frame.rle(.$runs))) %>%
  filter(values==TRUE, lengths>1)
ggplot(awsNaRuns, aes(x=lengths)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~StationId, scales="free") +
  scale_y_sqrt() +
  ggtitle("NA run lengths.") +
  ggsave(file.path(plotDir, paste("naRunLength.png")), 
         width=sqrt(2)*12, height=12)









#### Checks =================================================================== 

#Station 68228 has the longest stretch of NAs (about 10 days). First bit of code
#finds week with most NAs missing. Next bit plots half-hourly and 3-hourly
#synoptic data. Can see that we are missing values in both.
awsData %>% 
  filter(StationId==68228, is.na(AirTemp)) %>% 
  select(ts, AirTemp) %>% 
  mutate(ts.YMW = floor_date(ts, "week"))  %>%  
  group_by(ts.YMW) %>% 
  summarise(NA.count = n()) %>% 
  arrange(desc(NA.count))
awsData %>% 
  filter(StationId==68228, floor_date(ts, "month") == ymd("2003-05-01")) %>% 
  select(ts, AirTemp) %>% 
  ggplot(aes(x=ts, y=AirTemp)) +
  geom_point()
synopData %>% 
  filter(StationId==68228, floor_date(ts, "month") == ymd("2003-05-01")) %>% 
  select(ts, AirTemp) %>% 
  ggplot(aes(x=ts, y=AirTemp)) +
  geom_point()



# TODO (Cameron): Check if there are any cases where a whole day is missing and 
# synoptic data is available. Actually, just merge synoptic data and find NAs in
# half-hourly - check if any synoptics aren't NA.








#### Fill NA values ===========================================================

# Filter for data post 2000 only.
# TODO (Cameron): Go through each station and work out the best starting point.
# Some have good data earlier than this and some have bad data after this.

# Create a function in order to interpolate and flag missing data
interpAndFlag <- function(df) {
  # Using linear interpolation. Spline can give weird values (e.g. negatives for precip).
  
  df.imp <- df %>% 
    do(data.frame(.,
                  Value.int = na.approx(.$Value))) %>% 
    mutate(Q = ifelse(is.na(Value), "INT", "")) %>% 
    select(-Value) %>%
    rename(Value = Value.int)
  
  return(df.imp)
}

# Remove quality flags. These are all "N", i.e., not quality controlled.
awsData.imp <- awsData %>% 
  select(c(StationId, ts, Precip, AirTemp, DewTemp, RelHumidity, WindSpeed, WindDir, MaxGust, AWS)) %>% 
  gather(WeatherVar, Value, -c(StationId, ts)) %>% 
  filter(!is.na(Value)) %>% 
  group_by(StationId, WeatherVar)

# add all timeseries times  
awsTs <- awsData.imp %>%
  summarise(tsStart = min(ts),
            tsEnd = max(ts)) %>%
  group_by(StationId, WeatherVar) %>%
  do(data.frame(StationId = .$StationId,
                WeatherVar = .$WeatherVar,
                ts = seq(.$tsStart, .$tsEnd, 30*60))) #half-hourly intervals
awsData.imp <- full_join(awsData.imp, awsTs) %>%
  arrange(StationId, WeatherVar, ts)
rm(list = c("awsData", "awsTs")) # To free up memory

# Take out stuff that shouldn't be interpolated
awsData.no.imp <- awsData.imp %>% 
  filter(WeatherVar=="AWS") %>% 
  spread(WeatherVar, Value)

awsData.imp <- awsData.imp %>% 
  filter(WeatherVar!="AWS") %>% #Remove columns that we do not want to interpolate
  do(interpAndFlag(.)) #%>% 
  #bind_rows(awsData.imp %>% filter(WeatherVar=="AWS"))

awsData.imp.values <- awsData.imp %>% 
  select(StationId, ts, WeatherVar, Value) %>% 
  spread(WeatherVar, Value)

awsData.imp.flags <- awsData.imp %>% 
  ungroup() %>% 
  select(StationId, ts, WeatherVar, Q) %>% 
  mutate(WeatherVar = paste0(WeatherVar, "Q")) %>% 
  spread(WeatherVar, Q)

awsData.imp <- inner_join(awsData.imp.values, awsData.imp.flags) %>%
  select(StationId, ts, AirTemp, AirTempQ, DewTemp, DewTempQ, 
         Precip, PrecipQ, RelHumidity, RelHumidityQ, 
         WindSpeed, WindSpeedQ, WindDir, WindDirQ, MaxGust, 
         MaxGustQ)


# Add data that isn't interpolated back in
awsData.imp <- full_join(awsData.imp, awsData.no.imp)







# Plot to zoom in on data and see interpolated values
rawData <- awsData.imp %>% 
  filter(StationId==86282) %>% 
  select(StationId, ts, AirTemp, AirTempQ) %>% 
  mutate(Value = ifelse(AirTempQ=="INT", NA, AirTemp)) %>% 
  select(ts, Value) %>% 
  xts(x = .$Value, order.by = .$ts)
impData <- awsData.imp %>% 
  filter(StationId==86282) %>% 
  select(StationId, ts, AirTemp, AirTempQ) %>% 
  mutate(Value = AirTemp) %>%
  select(ts, Value) %>% 
  xts(x = .$Value, order.by = .$ts)
data.xts <- cbind(impData, rawData)
names(data.xts) <- c("Imputation", "Raw")

dygraph(data.xts) %>% 
  dyRangeSelector() %>% 
  dyOptions(drawPoints=TRUE,
            colors = c("red", "blue")) %>% 
  dyOptions(useDataTimezone = TRUE)




#### Output ===================================================================
awsData.imp %>% 
  rename(TimeSeries = ts) %>% 
  write.csv(file.path(outputDir, "awsData_tidy.csv"), row.names=F)

sunData %>% 
  write.csv(file.path(outputDir, "sunData_tidy.csv"), row.names=F)

awsDet %>% 
  write.csv(file.path(outputDir, "aws_details.csv"), row.names=F)

