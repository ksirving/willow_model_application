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

F57C <- read.csv("input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20_2 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")
# F37B_Low <- read.csv("input_data/HecRas/hydraulic_ts_F37B_Low.csv")
# LA2 <- read.csv("input_data/HecRas/hydraulic_ts_LA2.csv")
# LA3 <- read.csv("input_data/HecRas/hydraulic_ts_LA3.csv")
# LA14 <- read.csv("input_data/HecRas/hydraulic_ts_LA14.csv")
# F300 <- read.csv("input_data/HecRas/hydraulic_ts_F300.csv")
# GLEN <- read.csv("input_data/HecRas/hydraulic_ts_GLEN.csv")
# LA20_1 <- read.csv("input_data/HecRas/hydraulic_ts_LA20.csv")

# N11101250 <- read.csv("input_data/HecRas/hydraulic_ts_11101250.csv")
# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## not soft - just for dates

## go through script one at a time

N11101250 <- N11101250[-1,]
N11101250 <- N11101250 %>%
  mutate(Q_ts.datetime = F34D$Q_ts.datetime)

## LA20_2
LA20_2 <- LA20_2[-1,]
LA20_2 <- LA20_2 %>%
  mutate(Q_ts.datetime = F34D$Q_ts.datetime)


hydraul <- F57C[,-1]
names(hydraul)

## change some names
hydraul <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  select(c(DateTime, Q, node, Stream.Power..lb.ft.s..LOB, Hydr..Depth..ft..LOB,Stream.Power..lb.ft.s..MC, Hydr..Depth..ft..MC, 
           Stream.Power..lb.ft.s..ROB, Hydr..Depth..ft..ROB)) %>%
  rename(sp_ft_LOB = Stream.Power..lb.ft.s..LOB, depth_ft_LOB = Hydr..Depth..ft..LOB, sp_ft_MC = Stream.Power..lb.ft.s..MC,
         depth_ft_MC = Hydr..Depth..ft..MC, sp_ft_ROB = Stream.Power..lb.ft.s..ROB, depth_ft_ROB = Hydr..Depth..ft..ROB) %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  mutate(sp_w_LOB = (sp_ft_LOB*4.44822)/0.3048,
         sp_w_MC = (sp_ft_MC*4.44822)/0.3048,
         sp_w_ROB = (sp_ft_ROB*4.44822)/0.3048) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)


## format date time
hyd_dep$DateTime<-as.POSIXct(hyd_dep$DateTime,
                             format = "%Y-%m-%d %H:%M",
                             tz = "GMT")

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

hyd_sp <- hyd_dep %>%
  select(-contains("depth"))
head(hyd_sp)

hyd_dep <- hyd_dep %>%
  select(-contains("sp"))
head(hyd_dep)
# ## melt channel position data

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour"))
hyd_sp<-reshape2::melt(hyd_sp, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour"))



# stream power -------------------------------------------------------------------


## smooth spline the curve to get exact value of discharge at a given probability

## filter data by cross section position
new_dataM <- filter(hyd_sp, variable == "sp_w_MC")
new_dataL <- filter(hyd_sp, variable == "sp_w_LOB")
new_dataR <- filter(hyd_sp, variable == "sp_w_ROB")

## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
if(min(MC_curve$y)>20) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 20)$y
}


if(max(MC_curve$y)<4000) {
  newx2a <- max(MC_curve$x)
} else {
  newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 4000)$y
}

## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
if(min(LOB_curve$y)>20) {
  newx1aL <- min(LOB_curve$x)
} else {
  newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 20)$y
}


if(max(LOB_curve$y)<4000) {
  newx2aL <- max(LOB_curve$x)
} else {
  newx2aL <- approx(x = LOB_curve$y, y =LOB_curve$x, xout = 4000)$y
}

## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values
if(min(ROB_curve$y)>20) {
  newx1aR <- min(ROB_curve$x)
} else {
  newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 20)$y
}


if(max(ROB_curve$y)<4000) {
  newx2aR <- max(ROB_curve$x)
} else {
  newx2aR <- approx(x = ROB_curve$y, y =ROB_curve$x, xout = 4000)$y
}

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

write.csv(limits, "output_data/W4_F57C_willow_adult_stream_power_Q_limits.csv")
### percentage of time above threshold


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
  dplyr::mutate(Annual = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_statsl <- new_dataLx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aL & Q <= newx2aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aL & Q <= newx2aL)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataRx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aR & Q <= newx2aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aR & Q <= newx2aR)/length(DateTime)*100) %>%
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
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
  mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L))%>%
  mutate(position="MC")

new_dataL  <- new_dataLx %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1aL & Q <= newx2aL)) %>%
  mutate(threshold = if_else(Q >= newx1aL & Q <= newx2aL,  row_number(), 0L))%>%
  mutate(position="LOB")

new_dataR  <- new_dataRx %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1aR & Q <= newx2aR)) %>%
  mutate(threshold = if_else(Q >= newx1aR & Q <= newx2aR,  row_number(), 0L))%>%
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


# Depth -------------------------------------------------------------------

## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_dataM <- filter(hyd_dep, variable == "depth_cm_MC")
new_dataL <- filter(hyd_dep, variable == "depth_cm_LOB")
new_dataR <- filter(hyd_dep, variable == "depth_cm_ROB")

## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

if(min(MC_curve$y)>5) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 5)$y
}

## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
if(min(LOB_curve$y)>5) {
  newx1aL <- min(LOB_curve$x)
} else {
  newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 5)$y
}
newx1aL
## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values
if(min(ROB_curve$y)>5) {
  newx1aR <- min(ROB_curve$x)
} else {
  newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 5)$y
}

### df for limits
limits <- as.data.frame(matrix(ncol=3, nrow=1)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("thresh_1")

limits$LOB <-c(newx1aL[1])

limits$MC <- c(newx1a[1])

limits$ROB <- c(newx1aR[1])
limits

write.csv(limits, "output_data/W4_F57C_willow_germ_depth_Q_limits.csv")
### percent

## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/F57C_willow_germ_Depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=5, x=newx1a), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=5, x=newx1aL), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=5, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

### plot discharge over time

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)

# create year_month column       
new_dataLx <- new_dataL %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataLx)

# create year_month column       
new_dataRx <- new_dataR %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataRx)
## define critical period or season for adult as all year is critical

non_critical <- c(1:3,10:12) 
critical <- c(4:9) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataLx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataRx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

### time stats

time_statsm <- new_dataMx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_statsl <- new_dataLx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aL)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataRx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aR)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="ROB")
time_statsr

time_stats <- rbind(time_statsm, time_statsl, time_statsr)
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)
head(melt_time)
write.csv(melt_time, "output_data/W4_F57C_willow_germ_depth_time_stats.csv")

# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q >= newx1a)) %>%
  mutate(Min.Depth = if_else(Q >= newx1a, row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

new_dataL <- new_dataL %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q >= newx1aL)) %>%
  mutate(Min.Depth = if_else(Q >= newx1aL, row_number(), 0L))

new_dataL <- mutate(new_dataL, position="LOB")

new_dataR <- new_dataR %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q >= newx1aR)) %>%
  mutate(Min.Depth = if_else(Q >= newx1aR, row_number(), 0L))

new_dataR <- mutate(new_dataR, position="ROB")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, water_year, day all IDs and probs
# names(new_data)

new_dataMx <- select(new_dataM, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime))# all probs
names(new_dataMx)
new_dataLx <- select(new_dataL, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime) )# all probs
names(new_dataLx)
new_dataRx <- select(new_dataR, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime) )# all probs
names(new_dataRx)
## has some values but just becuase of the fake thresholds
# range(new_dataRx$Medium)
new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "water_year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)
head(melt_data)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days_low))

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days_low))

## count days in each year for time thing
total_days_per_year01 <- total_days01 %>%
  group_by(water_year) %>%
  summarise(days_per_water_year = sum(n_days_low)) %>%
  mutate(suitablility = ifelse(days_per_water_year >= 85 & days_per_water_year <= 280, "Yes", "No"))

total_days <- total_days_per_month01
head(total_days)
total_days_year <- total_days_per_year01

write.csv(total_days, "output_data/W4_F57C_willow_germ_depth_total_days.csv")
write.csv(total_days_year, "output_data/F5_F57C_willow_germ_depth_total_days_year.csv")

# # create water_year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month water_year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)


## define seasons/critical period
non_critical <- c(7:11) 
critical <- c(12, 1:6) 

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) #%>%
# pivot_wider(names_from = season, values_from = days_per_month)



# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/W4_F57C_willow_germ_depth_total_days_long.csv")



