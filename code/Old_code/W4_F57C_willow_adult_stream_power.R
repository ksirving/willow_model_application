## willow germination and depth

library(sf)
library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

## upload hydraulic data
F57C <- read.csv("input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")
# F37B_Low <- read.csv("input_data/HecRas/hydraulic_ts_F37B_Low.csv")
# LA2 <- read.csv("input_data/HecRas/hydraulic_ts_LA2.csv")
# LA3 <- read.csv("input_data/HecRas/hydraulic_ts_LA3.csv")
# GLEN <- read.csv("input_data/HecRas/hydraulic_ts_GLEN.csv")

hydraul <- F57C[,c(-1)]
names(hydraul)
hyd_sp <- hydraul[,c(1:3,7, 12, 15)]
colnames(hyd_sp) <-c("DateTime", "node", "Q", "sp_lb_ft_LOB", "sp_lb_ft_MC", "sp_lb_ft_ROB")


## convert unit from lb ft s to w/m2

hyd_sp <- hyd_sp %>%
  mutate(sp_watts_LOB = (sp_lb_ft_LOB*4.44822)/0.3048,
         sp_watts_MC = (sp_lb_ft_MC*4.44822)/0.3048,
         sp_watts_ROB = (sp_lb_ft_ROB*4.44822)/0.3048) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

hyd_sp<-reshape2::melt(hyd_sp, id=c("DateTime","Q", "node", "date_num"))

## format date time
hyd_sp$DateTime<-as.POSIXct(hyd_sp$DateTime,
                             format = "%Y-%m-%d %H:%M",
                             tz = "GMT")

new_data <- hyd_sp %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

save(new_data, file="output_data/W4_F57C_willow_adult_stream_power_discharge_probs_2010_2017_TS.RData")

## thresholds 
# >= 20 & <= 4000 = suitable
## depth v discharge

## plot
range(new_data$Q) 

## smooth spline the curve to get exact value of discharge at a given probability

## filter data by cross section position
new_dataM <- filter(new_data, variable == "sp_watts_MC")
new_dataL <- filter(new_data, variable == "sp_watts_LOB")
new_dataR <- filter(new_data, variable == "sp_watts_ROB")

## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 20)$y
newx1a

newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 4000)$y
newx2a

## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 20)$y
newx1aL

newx2aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 4000)$y
newx2aL

## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values
newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 20)$y
newx1aR

newx2aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 4000)$y
newx2aR

### df for limits
limits <- as.data.frame(matrix(ncol=3, nrow=2)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("thresh_1", "thresh_2")

limits$LOB <-c(newx1aL[1], 
               newx2aL[1])

limits$MC <- c(newx1a[1], 
               newx2a[1])

limits$ROB <- c(newx1aR[1], 
                newx2aR[1])
limits

write.csv(limits, "output_data/W4_willow_adult_stream_power_Q_limits.csv")
### percentage of time above threshold

## create expression for below stats - change each time!!!!!

# thresh_ex <- expression(Q <= newx2a) ## only higher threshold
thresh_ex <- expression(Q >= newx1a) ## only lower threshold
# thresh_ex <- expression(Q >= newx1a & Q <= newx2a) ## both thresholds

# thresh_exL <- expression(Q <= newx2aL) ## only higher threshold
thresh_exL <- expression(Q >= newx1aL) ## only lower threshold
# thresh_exL <- expression(Q >= newx1aL & Q <= newx2aL) ## both thresholds

# thresh_exR <- expression(Q <= newx2aR) ## only higher threshold
thresh_exR <- expression(Q >= newx1aR) ## only lower threshold
# thresh_exR <- expression(Q >= newx1aR & Q <= newx2aR) ## both thresholds

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)

# create year_month column       
new_dataLx <- new_dataL %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataLx)

# create year_month column       
new_dataRx <- new_dataR %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataRx)


# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

## define critical period or season for adult as all year is critical

non_critical <- c(1:3,10:12) 
critical <- c(4:9) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataLx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataRx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

# time stats ------------------------------------------------
### time stats

time_statsm <- new_dataMx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(eval(thresh_ex))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(eval(thresh_ex))/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_statsl <- new_dataLx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(eval(thresh_exL))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(eval(thresh_exL))/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataRx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(eval(thresh_exR))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(eval(thresh_exR))/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="ROB")

time_stats <- rbind(time_statsm, time_statsl, time_statsr)

time_stats
write.csv(time_stats, "output_data/W4_F57C_time_stats_willow_adult_stream_power.csv")


# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
## packages

## change year to water year and count hours within Q range
new_dataM  <- new_dataMx %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh_ex))) %>%
  mutate(threshold = if_else(eval(thresh_ex),  row_number(), 0L))%>%
  mutate(position="MC")

new_dataL  <- new_dataLx %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh_exL))) %>%
  mutate(threshold = if_else(eval(thresh_exL),  row_number(), 0L))%>%
  mutate(position="LOB")

new_dataR  <- new_dataRx %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh_exR))) %>%
  mutate(threshold = if_else(eval(thresh_exR),  row_number(), 0L))%>%
  mutate(position="ROB")
new_dataR
# select columns needed and combine dfs

new_dataMx <- select(new_dataM, c(Q, month, water_year, year, day, ID, threshold, position, season)) # all probs
new_dataLx <- select(new_dataL, c(Q, month, water_year, year, day, ID, threshold, position, season))
new_dataRx <- select(new_dataR, c(Q, month, water_year, year, day, ID, threshold, position, season))

new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)
new_datax
## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "day", "month", "year", "Q", "water_year", "position", "season"))
melt_data <- melt_data %>% rename(consec_hours = value) %>%
  select(-variable)

## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  group_by(ID, day, month, water_year, position, season) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_year01 <- total_days01 %>%
  group_by(water_year, position, season) %>%
  summarise(days_per_water_year = sum(n_days)) #%>%

total_days_per_year01

write.csv(total_days_per_year01, "output_data/W4_F57C_number_of_days_willow_adult_stream_power.csv")

