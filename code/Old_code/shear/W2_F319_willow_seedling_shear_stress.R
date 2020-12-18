## willow model application
## Jenny Rogers & Katie Irving

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

### data upload
shear <- read.csv("input_data/shear.csv", skip = 2)
head(shear)
######## seedling inudation curve from data predicting percent mortality from duration and depth Halsell et al and Vandersande et al


names(shear)[c(1,2, 4)] <- c("shear", "Year", "mortality")
shear$Year <- as.factor(shear$Year)

png("Figures/Seedling_Shear_colours.png", width = 600, height = 600)

ggplot(data = shear, mapping = aes(x = shear, y = mortality))+
  geom_point(aes( group = Year, color = Year), size = 4) +
  # geom_point(size = 4)+
  geom_smooth(method = "lm")+
  labs(y = "Mortality (%)", x = "Bed Shear Stress (Pa)")+
  theme_classic()+
  theme(axis.text = element_text(size = 30), axis.title = element_text(size = 30), 
        legend.text=element_text(size=20), legend.title=element_text(size=20))

dev.off()

summary(shear_seedling <- lm(mortality~shear, data = shear))

save(shear_seedling, file= "shear_seedling.rda")


## upload hydraulic data

# N11101250 <- read.csv("input_data/HecRas/hydraulic_ts_11101250.csv")
# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv")
# F37B_High <- read.csv("input_data/HecRas/hydraulic_ts_F37B_High.csv")
# F45B <- read.csv("input_data/HecRas/hydraulic_ts_F45B.csv")
# F300 <- read.csv("input_data/HecRas/hydraulic_ts_F300.csv")
F319 <- read.csv("input_data/HecRas/hydraulic_ts_F319.csv")
# LA13 <- read.csv("input_data/HecRas/hydraulic_ts_LA13.csv")
# LA14 <- read.csv("input_data/HecRas/hydraulic_ts_LA14.csv")
# LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20.csv")
## select columns

hydraul <- F319[,c(-1)]
hyd_shear <- hydraul[,c(1:3,6,10,14)]
colnames(hyd_shear) <-c("DateTime", "node", "Q", "shear_lb_ft_LOB", "shear_lb_ft_MC", "shear_lb_ft_ROB")

# nas <- which(complete.cases(hyd_dep) == FALSE)
# hyd_dep[nas,]

## convert unit to pascals

hyd_shear <- hyd_shear %>%
  mutate(shear_pa_LOB = (shear_lb_ft_LOB/0.020885),
         shear_pa_MC = (shear_lb_ft_MC/0.020885),
         shear_pa_ROB = (shear_lb_ft_ROB/0.020885)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))
hyd_shear


hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q", "node", "date_num"))
hyd_shear <- hyd_shear %>% rename(shear = value)
hyd_shear <- filter(hyd_shear, variable == "shear_pa_MC")
## predict on node data using model
names(hyd_shear)


## workflow
## get probabilities for shearth at each hourly time step
## get thresholds i.e. 25, 50, 75%

head(hyd_shear)
summary(shear_seedling)

new_data <- hyd_shear %>%
  mutate(prob_fit = predict(shear_seedling, newdata = hyd_shear, type="response")) #%>%
  mutate(prob_fit = ifelse(prob_fit >100, 100, prob_fit)) ## percentage goes up to 200 so cut off at 100

head(new_data)
range(new_data$prob_fit)

save(new_data, file="output_data/W2_F319_seedling_shear_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))


head(new_data)

save(new_data, file="output_data/W2_F319_seedling_shear_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------


load( file="output_data/W2_F319_seedling_shear_discharge_probs_2010_2017_TS.RData")
head(new_data)

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="shear_pa_MC")
peakQM  <- max(peakQM$Q)
peakQM 


## filter data by cross section position

new_dataM <- filter(new_data, variable == "shear_pa_MC")

## Main channel curves

load(file="root_interpolation_function.Rdata")

newx1a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 25)
newx1a

newx2a  <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 50)
newx2a 


newx3a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 75)
newx3a


## MAKE DF OF Q LIMITS

## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")


limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])

limits

write.csv(limits, "output_data/W2_F319_seedling_shear_Q_limits.csv")

#### plot
png("figures/Application_curves/Shear/F319_seedling_shear_prob_Q_thresholds.png", width = 500, height = 600)
labels <- c(shear_pa_LOB = "Left Over Bank", shear_pa_MC = "Main Channel", shear_pa_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("shear_pa_LOB", "shear_pa_MC", "shear_pa_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=25, x=newx1a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=25, x=newx1a[2]), color="green") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=25, x=newx1a[3]), color="green") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=25, x=newx1a[4]), color="green") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=50, x=newx2a[1]), color="red") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=50, x=newx2a[2]), color="red") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=50, x=newx2a[3]), color="red") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=50, x=newx2a[4]), color="red") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=75, x=newx3a[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=75, x=newx3a[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=75, x=newx3a[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="shear_pa_MC"), aes(y=75, x=newx3a[4]), color="blue") +
  
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F319: Willow Seedling/Shear: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)


# dataframe for stats -----------------------------------------------------


## define seasons/critical period
non_critical <- c(1:3,10:12) 
critical <- c(4:9) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )



# time stats - mid channel ------------------------------------------------

if(is.na(newx1a[1])) {
  
  low_threshM <- expression(Q < 0)
  ## 1a) if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx1a)==1 && newx1a < peakQM){
  # sum the amount of time above threshold
  low_threshM <- expression(Q >= newx1a)
  
  ## 1b) if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx1a)==1 && newx1a > peakQM){
  # sum the amount of time below the threshold
  low_threshM <- expression(Q <= newx1a)
  
  ## 2a) if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx1a)==2 && newx1a[1] < peakQM) { 
  # sum the amount of time above the first and below the 2nd threshold
  low_threshM <- expression(Q >= newx1a[1] & Q <= newx1a[2])
  
  ## 2b) if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx1a)==2 && (newx1a[1] > peakQM || newx1a[2] < peakQM )) {
  # sum the amount of time below the first and above the 2nd threshold
  low_threshM <- expression(Q <= newx1a[1] & Q >= newx1a[2])
  
  ## 3a) if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx1a) == 3 && newx1a[3] > peakQM) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  low_threshM <- expression(Q <= newx1a[1] | Q >= newx1a[2] & Q <= newx1a[3])
  
  ## 3b) if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx1a) == 3 && newx1a[1] < peakQM) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  low_threshM <- expression(Q >= newx1a[1] & Q <= newx1a[2] | Q >= newx1a[3])
  
  ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
} else if (length(newx1a) == 4 && newx1a[1] < peakQM) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  low_threshM <- expression(Q >= newx1a[1] & Q <= newx1a[2] |  Q >= newx1a[3] & Q <= newx1a[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx1a) == 4 && (newx1a[1] < peakQM && newx1a[2] < peakQM && newx1a[3] < peakQM && newx1a[4] < peakQM || newx1a[1] > peakQM 
                                   && newx1a[2] > peakQM && newx1a[3] > peakQM && newx1a[4] > peakQM  || newx1a[2] < peakQM && newx1a[3] > peakQM)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  low_threshM <- expression(Q <= newx1a[1] & Q >= newx1a[2] |  Q <= newx1a[3] & Q >= newx1a[4])
}

low_threshM

### medium threshold

if(is.na(newx2a[1])) {
  med_threshM <- expression(Q < 0)
  ## if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx2a)==1 && newx2a < peakQM){
  # sum the amount of time above threshold
  med_threshM <- expression(Q >= newx2a)
  
  ## if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx2a)==1 && newx2a > peakQM){
  # sum the amount of time below the threshold
  med_threshM <- expression(Q <= newx2a)
  
  ## if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx2a)==2 && newx2a[1] < peakQM) { 
  # sum the amount of time above the first and below the 2nd threshold
  med_threshM <- expression(Q >= newx2a[1] & Q <= newx2a[2])
  
  ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx2a)==2 && (newx2a[1] > peakQM || newx2a[2] < peakQM) ) {
  # sum the amount of time below the first and above the 2nd threshold
  med_threshM <- expression(Q <= newx2a[1] & Q >= newx2a[2])
  
  ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx2a) == 3 && newx2a[3] > peakQM) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  med_threshM <- expression(Q <= newx2a[1] | Q >= newx2a[2] & QM <= newx2a[3])
  
  ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx2a) == 3 && newx2a[1] < peakQM) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshM <- expression(Q >= newx2a[1] & Q <= newx2a[2] | Q >= newx2a[3])
  
  ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
} else if (length(newx2a) == 4 && newx2a[1] < peakQM) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  med_threshM <- expression(Q >= newx2a[1] & Q <= newx2a[2] |  Q >= newx2a[3] & Q <= newx2a[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx2a) == 4 && (newx2a[1] < peakQM && newx2a[2] < peakQM && newx2a[3] < peakQM && newx2a[4] < peakQM || newx2a[1] > peakQM 
                                   && newx2a[2] > peakQM && newx2a[3] > peakQM && newx2a[4] > peakQM  || newx2a[2] < peakQM && newx2a[3] > peakQM)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshM <- expression(Q <= newx2a[1] & Q >= newx2a[2] |  Q <= newx2a[3] & Q >= newx2a[4])
}

med_threshM

###  high threshold

if(is.na(newx3a[1])) {
  high_threshM <- expression(Q < 0)
  ## if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx3a)==1 && newx3a < peakQM){
  # sum the amount of time above threshold
  high_threshM <- expression(Q >= newx3a)
  
  ## if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx3a)==1 && newx3a > peakQM){
  # sum the amount of time below the threshold
  high_threshM <- expression(Q <= newx3a)
  
  ## if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx3a)==2 && newx3a[1] < peakQM) { 
  # sum the amount of time above the first and below the 2nd threshold
  high_threshM <- expression(Q >= newx3a[1] & Q <= newx3a[2])
  
  ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx3a)==2 && (newx3a[1] > peakQM || newx3a[2] < peakQM) ) {
  # sum the amount of time below the first and above the 2nd threshold
  high_threshM <- expression(Q <= newx3a[1] & Q >= newx3a[2])
  
  ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx3a) == 3 && newx3a[3] > peakQM) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  high_threshM <- expression(Q <= newx3a[1] | Q >= newx3a[2] & Q <= newx3a[3])
  
  ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx3a) == 3 && newx3a[1] < peakQM) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  high_threshM <- expression(Q >= newx3a[1] & Q <= newx3a[2] | QM >= newx3a[3])
  
  ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
} else if (length(newx3a) == 4 && newx3a[1] < peakQM) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  med_threshM <- expression(Q >= newx3a[1] & Q <= newx3a[2] |  Q >= newx3a[3] & Q <= newx3a[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx3a) == 4 && (newx3a[1] < peakQM && newx3a[2] < peakQM && newx3a[3] < peakQM && newx3a[4] < peakQM || newx3a[1] > peakQM 
                                   && newx3a[2] > peakQM && newx3a[3] > peakQM && newx3a[4] > peakQM  || newx3a[2] < peakQM && newx3a[3] > peakQM)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshM <- expression(Q <= newx3a[1] & Q >= newx3a[2] |  Q <= newx3a[3] & Q >= newx3a[4])
}

high_threshM

###### calculate amount of time

time_statsm <- new_dataMx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Low = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshM))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshM))/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsm

time_stats <- time_statsm

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position", "water_year"))
melt_time <- rename(melt_time, Probability_Threshold = variable)

write.csv(melt_time, "output_data/W2_F319_willow_seedling_shear_time_stats.csv")

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season, -year) %>% distinct()

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )


# Number of days above discharge ------------------------------------------
new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(eval(low_threshM))) %>%
  mutate(Low = if_else(eval(low_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID02 = data.table::rleid(eval(med_threshM))) %>%
  mutate(Medium = if_else(eval(med_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID03 = data.table::rleid(eval(high_threshM))) %>%
  mutate(High = if_else(eval(high_threshM), row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

# melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_datax)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/W2_F319_willow_seedling_shear_total_days.csv")
# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)

## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period
non_critical <- c(1:3,10:12) 
critical <- c(4:9) 

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

unique(total_days$season)
str(total_days)
# ## melt data

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/W2_F319_willow_seedling_shear_total_days_long.csv")
