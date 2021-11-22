### clean fremont weir Sacramento river hieght

library(dplyr)
library(imputeTS)
source("scripts/functions/f_get_fre.R")
source("scripts/functions/f_get_dayflow.R")

f_clean_indundation <- function(){

  fre <- f_get_fre()
  fre$date <- as.Date(fre$datetime)

  # remove unrealistic values (Peak Stage of Record 41.02')
  fre.qc<- fre %>%
    filter(value > 2 & value < 41.03)

  # this is hourly data, so need to calc max per day
  discharge_sac <-
    fre.qc %>%
    group_by(date) %>%
    summarise(height_sac = max(value, na.rm = TRUE))

  # look for missing dates
  time.check= seq(as.Date('1995-02-23'),as.Date('2021-01-01'),by='day')
  length(time.check) # missing 57
  d <- discharge_sac$date

  continous.dates <- data.frame (x = 1:9445, date = seq(as.Date('1995-02-23'),as.Date('2021-01-01'),by='day'))
  discharge_sac_na <- merge(discharge_sac, continous.dates, by = "date", all = TRUE)

  discharge_sac_na$height_sac_na <- na_ma(discharge_sac_na$height_sac, k = 7, weighting = "exponential", maxgap = Inf)

  ### clean dayflow
  # load dayflow

  dayflow <- f_get_dayflow()
  dayflow$date <- as.Date(dayflow$Date, "%m/%d/%Y")

  # merge two water datasets
  All.flows <- merge(dayflow[,c(5,30)], discharge_sac_na[,c(1,4)], by = "date")
  check.again <- seq(as.Date('1996-10-01'), as.Date('2020-09-30'), by = 'day') # no missing dates!!!

  ### calculate inundation days
  # definition for inundation days

  # definition for inundation days
  for(i in 1:nrow(All.flows)){
    if(All.flows[i,"height_sac_na"] < 33.5){
      All.flows[i,"Inund.days"] <- 0}
    else if(All.flows[i, "height_sac_na"] >= 33.5){
      All.flows[i, "Inund.days"] <- All.flows[i-1, "Inund.days"]+1}
    else {
      All.flows[i, "Inund.days"] <- 0 }
  }

  # jessica's addition to fix the tails
  for(i in 2:nrow(All.flows)){
    if(All.flows[i, "YOLO"] >= 4000 & All.flows[i-1, "Inund.days"] > 0){
      All.flows[i, "Inund.days"] <- All.flows[i-1, "Inund.days"]+1}
  }



  ### add column for inundation yes (1) or no (0)
  # flooding? yes (1), no (0)
  All.flows <- All.flows %>%
    mutate(inundation = ifelse(Inund.days > 0, 1, 0))

  write.csv(All.flows, "data_clean/inundation_days.csv", row.names = FALSE)


}

