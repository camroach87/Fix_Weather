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
  ggsave(file.path(plotDir, paste("naRunLength.png")), 
         width=sqrt(2)*12, height=12)



