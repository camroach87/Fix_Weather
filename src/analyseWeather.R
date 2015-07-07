# Description: Plots raw data characteristics. Calculates missing values.
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
require(ggplot2)
require(zoo)

load("./env/weather.RData")

#### Init =====================================================================
plotDir <- "./plots/aws"

dir.create(plotDir, F, T)




#### Plot raw data ============================================================
stationPlots <- unique(awsData$StationId)

# Highlights missing half-hourly values
for (iS in stationPlots) {
  cat("Outputting plot for station number", iS, "...\n")
  
  naTimes <- awsData %>%
    filter(StationId==iS,
           is.na(AirTemp))
  awsData %>%
    filter(StationId==iS) %>%
    ggplot(aes(x=ts, y=AirTemp)) + 
    geom_point() +
    geom_vline(data = naTimes, aes(xintercept = as.numeric(ts)), 
               colour="red", alpha=0.15) +
    ggtitle(paste(iS, "half-hourly air temperature data.")) +
    ggsave(file.path(plotDir, paste0("awsAirTemp", iS, ".png")), 
           width=22, height=12)
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
# synoptic data is available.








#### Fill NA values ===========================================================

# Filter for data post 2000 only.
# TODO (Cameron): Go through each station and work out the best starting point.
# Some have good data earlier than this and some have bad data after this.
awsData <- awsData %>% 
  filter(ts >= dmy("1/1/2000"))


