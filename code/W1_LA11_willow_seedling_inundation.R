## willow model application
## Jenny Rogers & Katie Irving

## packages

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
inund <- read.csv("input_data/inundation.csv")

######## seedling inudation curve from data predicting percent mortality from duration and depth Halsell et al and Vandersande et al


inund <- inund %>% 
  filter(species == "Salix gooddingii")

head(inund)

ggplot(data = inund, mapping = aes(x = depth_cm, y = mortality_prec))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  labs(x = "Depth (cm)", y = "Mortality (%)")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

summary(depth_seedling_mod <- lm(mortality_prec ~ depth_cm + I(depth_cm^2), data = inund))

save(depth_seedling_mod, file = "models/depth_seedling_mod.rda")

## upload hydraulic data


# F57C <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_LA8.csv")
LA11 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_LA20_2.csv")

## go through script one at a time

hydraul <- LA11[,-1]
names(hydraul)
head(hydraul)
## select columns

hyd_dep <- hydraul[,c(1:3,5,9,13)]
colnames(hyd_dep) <-c("DateTime", "node", "Q", "depth_ft_LOB", "depth_ft_MC", "depth_ft_ROB")

## convert unit from feet to meters

hyd_dep <- hyd_dep %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

# ## melt channel position data
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))
hyd_dep <- hyd_dep %>% rename(depth_cm = value)
head(hyd_dep)


## workflow
## get probabilities for depth at each hourly time step
## get thresholds i.e. 25, 50, 75%

head(hyd_dep)
summary(depth_seedling_mod)

new_data <- hyd_dep %>%
  mutate(prob_fit = predict(depth_seedling_mod,newdata = hyd_dep, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit >100, 100, prob_fit)) ## percentage goes up to 200 so cut off at 100
  
head(new_data)
range(new_data$prob_fit)

save(new_data, file="output_data/W1_LA11_seedling_inundation_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime))

  

head(new_data)

save(new_data, file="output_data/W1_LA11_seedling_inundation_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------


load( file="output_data/W1_LA11_seedling_inundation_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 24.50545 40888.74219
range(new_data$prob_fit) ## -0.00446516  0.39894192


peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
peakQM 

peakQL <- filter(peak, variable=="depth_cm_LOB")
peakQL  <- max(peakQL$Q) ## 
peakQL 

peakQR <- filter(peak, variable=="depth_cm_ROB")
peakQR  <- max(peakQR$Q) ## 
peakQR 

## filter data by cross section position

new_dataM <- filter(new_data, variable == "depth_cm_MC")
new_dataL <- filter(new_data, variable == "depth_cm_LOB")
new_dataR <- filter(new_data, variable == "depth_cm_ROB")

## Main channel curves

load(file="root_interpolation_function.Rdata")

newx1a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 25)
newx1a

newx2a  <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 50)
newx2a 


newx3a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 75)
newx3a

## LOB curves
newx1aL <- RootLinearInterpolant(new_dataL$Q, new_dataL$prob_fit, 25)
newx1aL 


newx2aL <- RootLinearInterpolant(new_dataL$Q, new_dataL$prob_fit, 50)
newx2aL 


newx3aL <- RootLinearInterpolant(new_dataL$Q, new_dataL$prob_fit, 75)
newx3aL 


## ROB curves

newx1aR <- RootLinearInterpolant(new_dataR$Q, new_dataR$prob_fit, 25)
newx1aR 


newx2aR <- RootLinearInterpolant(new_dataR$Q, new_dataR$prob_fit, 50)
newx2aR 


newx3aR <- RootLinearInterpolant(new_dataR$Q, new_dataR$prob_fit, 75)
newx3aR 

## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")

limits$LOB <- c(newx1aL[1], newx1aL[2],newx1aL[3],newx1aL[4], 
                newx2aL[1], newx2aL[2],newx2aL[3], newx2aL[4],  
                newx3aL[1], newx3aL[2],newx3aL[3], newx3aL[4])

limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1aL[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])

limits$ROB <- c(newx1aR[1], newx1aR[2],newx1aR[3], newx1aR[4], 
                newx2aR[1], newx2aR[2],newx2aR[3], newx2aR[4],
                newx3aR[1], newx3aR[2],newx3aR[3], newx3aR[4])
limits

write.csv(limits, "output_data/W1_seedling_depth_LA11_Q_limits.csv")

# plot discharge points ---------------------------------------------------
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("Figures/Application_curves/Depth/LA11_willow_seedling_depth_prob_Q_thresholds.png", width = 500, height = 600)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=25, x=newx1a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=25, x=newx1a[2]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=25, x=newx1a[3]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=25, x=newx1a[4]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=50, x=newx2a[1]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=50, x=newx2a[2]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=50, x=newx2a[3]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=50, x=newx2a[4]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=75, x=newx3a[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=75, x=newx3a[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=75, x=newx3a[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=75, x=newx3a[4]), color="blue") +
  
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=25, x=newx1aL[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=25, x=newx1aL[2]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=25, x=newx1aL[3]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=25, x=newx1aL[4]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=50, x=newx2aL[1]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=50, x=newx2aL[2]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=50, x=newx2aL[3]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=50, x=newx2aL[4]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=75, x=newx3aL[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=75, x=newx3aL[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=75, x=newx3aL[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=75, x=newx3aL[4]), color="blue") +
  
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=25, x=newx1aR[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=25, x=newx1aR[2]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=25, x=newx1aR[3]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=25, x=newx1aR[4]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2aR[1]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2aR[2]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2aR[3]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2aR[4]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=75, x=newx3aR[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=75, x=newx3aR[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=75, x=newx3aR[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=75, x=newx3aR[4]), color="blue") +
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA11: seedling/Depth: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataMx)

# create year_month column       
new_dataLx <- new_dataL %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataLx)

# create year_month column       
new_dataRx <- new_dataR %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataRx)


# discharge time series plots with probability lines ----------------------

##  plot time series of discharge - 0.2 prob line

ggplot(new_datax) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newxmid, linetype="dashed", color="red")+
  facet_wrap(~year, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

##  plot time series of discharge - subset to one year
new_datax_2016 <- filter(new_datax, year==2016)

ggplot(new_datax_2016) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newxmid, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

# dataframe for stats -----------------------------------------------------

## make dataframe for all years 
## make dataframe for all years 

## define critical period or season for seedling as all year is critical
non_critical <- c(1:3,10:12) ## winter months
critical <- c(4:9) ## summer months

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataLx <- new_dataLx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataRx <- new_dataRx %>%
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
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshM))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshM))/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsm


# time stats - left over bank ---------------------------------------------


### define expression for low threshold 

if(is.na(newx1aL[1])) {
  
  low_threshL <- expression(Q < 0)
  ## 1a) if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx1aL)==1 && newx1aL < peakQL){
  # sum the amount of time above threshold
  low_threshL <- expression(Q >= newx1aL)
  
  ## 1b) if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx1aL)==1 && newx1aL > peakQL){
  # sum the amount of time below the threshold
  low_threshL <- expression(Q <= newx1aL)
  
  ## 2a) if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx1aL)==2 && newx1aL[1] < peakQL) { 
  # sum the amount of time above the first and below the 2nd threshold
  low_threshL <- expression(Q >= newx1aL[1] & Q <= newx1aL[2])
  
  ## 2b) if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx1aL)==2 && (newx1aL[1] > peakQL || newx1aL[2] < peakQL) ) {
  # sum the amount of time below the first and above the 2nd threshold
  low_threshL <- expression(Q <= newx1aL[1] & Q >= newx1aL[2])
  
  ## 3a) if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx1aL) == 3 && newx1aL[3] > peakQL) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  low_threshL <- expression(Q <= newx1aL[1] | Q >= newx1aL[2] & Q <= newx1aL[3])
  
  ## 3b) if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx1aL) == 3 && newx1aL[1] < peakQL) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  low_threshL <- expression(Q >= newx1aL[1] & Q <= newx1aL[2] | Q >= newx1aL[3])
  
  ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
} else if (length(newx1aL) == 4 && newx1aL[1] < peakQL) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  low_threshL <- expression(Q >= newx1aL[1] & Q <= newx1aL[2] |  Q >= newx1aL[3] & Q <= newx1aL[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx1aL) == 4 && (newx1aL[1] < peakQL && newx1aL[2] < peakQL && newx1aL[3] < peakQL && newx1aL[4] < peakQL || newx1aL[1] > peakQL 
                                    && newx1aL[2] > peakQL && newx1aL[3] > peakQL && newx1aL[4] > peakQL  || newx1aL[2] < peakQL && newx1aL[3] > peakQL)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  low_threshL <- expression(Q <= newx1aL[1] & Q >= newx1aL[2] |  Q <= newx1aL[3] & Q >= newx1aL[4])
}

low_threshL

### medium threshold

if(is.na(newx2aL[1])) {
  med_threshL <- expression(Q < 0)
  ## if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx2aL)==1 && newx2aL < peakQL){
  # sum the amount of time above threshold
  med_threshL <- expression(Q >= newx2aL)
  
  ## if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx2aL)==1 && newx2aL > peakQL){
  # sum the amount of time below the threshold
  med_threshL <- expression(Q <= newx2aL)
  
  ## if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx2aL)==2 && newx2aL[1] < peakQL) { 
  # sum the amount of time above the first and below the 2nd threshold
  med_threshL <- expression(Q >= newx2aL[1] & Q <= newx2aL[2])
  
  ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx2aL)==2 && (newx2aL[1] > peakQL || newx2aL[2] < peakQL )) {
  # sum the amount of time below the first and above the 2nd threshold
  med_threshL <- expression(Q <= newx2aL[1] & Q >= newx2aL[2])
  
  ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx2aL) == 3 && newx2aL[3] > peakQL) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  med_threshL <- expression(Q <= newx2aL[1] | Q >= newx2aL[2] & QM <= newx2aL[3])
  
  ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx2aL) == 3 && newx2aL[1] < peakQL) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshL <- expression(Q >= newx2aL[1] & Q <= newx2aL[2] | Q >= newx2aL[3])
  ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
  
} else if (length(newx2aL) == 4 && newx2aL[1] < peakQL) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  med_threshL <- expression(Q >= newx2aL[1] & Q <= newx2aL[2] |  Q >= newx2aL[3] & Q <= newx2aL[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx2aL) == 4 && (newx2aL[1] < peakQL && newx2aL[2] < peakQL && newx2aL[3] < peakQL && newx2aL[4] < peakQL || newx2aL[1] > peakQL 
                                    && newx2aL[2] > peakQL && newx2aL[3] > peakQL && newx2aL[4] > peakQL  || newx2aL[2] < peakQL && newx2aL[3] > peakQL)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshL <- expression(Q <= newx2aL[1] & Q >= newx2aL[2] |  Q <= newx2aL[3] & Q >= newx2aL[4])
}

med_threshL

###  high threshold

if(is.na(newx3aL[1])) {
  high_threshL <- expression(Q < 0)
  ## if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx3aL)==1 && newx3aL < peakQL){
  # sum the amount of time above threshold
  high_threshL <- expression(Q >= newx3aL)
  
  ## if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx3aL)==1 && newx3aL > peakQL){
  # sum the amount of time below the threshold
  high_threshL <- expression(Q <= newx3aL)
  
  ## if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx3aL)==2 && newx3aL[1] < peakQL) { 
  # sum the amount of time above the first and below the 2nd threshold
  high_threshL <- expression(Q >= newx3aL[1] & Q <= newx3aL[2])
  
  ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx3aL)==2 && (newx3aL[1] > peakQL || newx3aL[2] < peakQL )) {
  # sum the amount of time below the first and above the 2nd threshold
  high_threshL <- expression(Q <= newx3aL[1] & Q >= newx3aL[2])
  
  ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx3aL) == 3 && newx3aL[3] > peakQL) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  high_threshL <- expression(Q <= newx3aL[1] | Q >= newx3aL[2] & Q <= newx3aL[3])
  
  ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx3aL) == 3 && newx3aL[1] < peakQL) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  high_threshL <- expression(Q >= newx3aL[1] & Q <= newx3aL[2] | QM >= newx3aL[3])
  
} else if (length(newx3aL) == 4 && newx3aL[1] < peakQL) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  high_threshL <- expression(Q >= newx3aL[1] & Q <= newx3aL[2] | Q >= newx3aL[3] & Q <= newx3aL[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx3aL) == 4 && (newx3aL[1] < peakQL && newx3aL[2] < peakQL && newx3aL[3] < peakQL && newx3aL[4] < peakQL || newx3aL[1] > peakQL 
                                    && newx3aL[2] > peakQL && newx3aL[3] > peakQL && newx3aL[4] > peakQL  || newx3aL[2] < peakQL && newx3aL[3] > peakQL)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  high_threshL <- expression(Q <= newx3aL[1] & Q >= newx3aL[2] |  Q <= newx3aL[3] & Q >= newx3aL[4])
}

high_threshL
###### calculate amount of time

time_statsl <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(eval(low_threshL))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshL))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshL))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshL))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshL))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshL))/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

time_statsl


# time stats - right over bank ---------------------------------------------

### define expression for low threshold 

if(is.na(newx1aR[1])) {
  
  low_threshR <- expression(Q < 0)
  ## 1a) if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx1aR)==1 && newx1aR < peakQR){
  # sum the amount of time above threshold
  low_threshR <- expression(Q >= newx1aR)
  
  ## 1b) if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx1aR)==1 && newx1aR > peakQR){
  # sum the amount of time below the threshold
  low_threshR <- expression(Q <= newx1aR)
  
  ## 2a) if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx1aR)==2 && newx1aR[1] < peakQR) { 
  # sum the amount of time above the first and below the 2nd threshold
  low_threshR <- expression(Q >= newx1aR[1] & Q <= newx1aR[2])
  
  ## 2b) if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx1aR)==2 && (newx1aR[1] > peakQR || newx1aR[2] < peakQR) ) {
  # sum the amount of time below the first and above the 2nd threshold
  low_threshR <- expression(Q <= newx1aR[1] & Q >= newx1aR[2])
  
  ## 3a) if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx1aR) == 3 && newx1aR[3] > peakQR) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  low_threshR <- expression(Q <= newx1aR[1] | Q >= newx1aR[2] & Q <= newx1aR[3])
  
  ## 3b) if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx1aR) == 3 && newx1aR[1] < peakQR) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  low_threshR <- expression(Q >= newx1aR[1] & Q <= newx1aR[2] | Q >= newx1aR[3])
  ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
  
} else if (length(newx1aR) == 4 && newx1aR[1] < peakQR) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  low_threshR <- expression(Q >= newx1aR[1] & Q <= newx1aR[2] |  Q >= newx1aR[3] & Q <= newx1aR[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx1aR) == 4 && (newx1aR[1] < peakQR && newx1aR[2] < peakQR && newx1aR[3] < peakQR && newx1aR[4] < peakQR || newx1aR[1] > peakQR 
                                    && newx1aR[2] > peakQR && newx1aR[3] > peakQR && newx1aR[4] > peakQR  || newx1aR[2] < peakQR && newx1aR[3] > peakQR)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  low_threshR <- expression(Q <= newx1aR[1] & Q >= newx1aR[2] |  Q <= newx1aR[3] & Q >= newx1aR[4])
}

low_threshR

### medium threshold

if(is.na(newx2aR[1])) {
  med_threshR <- expression(Q < 0)
  ## if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx2aR)==1 && newx2aR < peakQR){
  # sum the amount of time above threshold
  med_threshR <- expression(Q >= newx2aR)
  
  ## if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx2aR)==1 && newx2aR > peakQR){
  # sum the amount of time below the threshold
  med_threshR <- expression(Q <= newx2aR)
  
  ## if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx2aR)==2 && newx2aR[1] < peakQR) { 
  # sum the amount of time above the first and below the 2nd threshold
  med_threshR <- expression(Q >= newx2aR[1] & Q <= newx2aR[2])
  
  ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx2aR)==2 && (newx2aR[1] > peakQR || newx2aR[2] < peakQR )) {
  # sum the amount of time below the first and above the 2nd threshold
  med_threshR <- expression(Q <= newx2aR[1] & Q >= newx2aR[2])
  
  ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx2aR) == 3 && newx2aR[3] > peakQR) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  med_threshR <- expression(Q <= newx2aR[1] | Q >= newx2aR[2] & QM <= newx2aR[3])
  
  ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx2aR) == 3 && newx2aR[1] < peakQR) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshR <- expression(Q >= newx2aR[1] & Q <= newx2aR[2] | Q >= newx2aR[3])
  
} else if (length(newx2aR) == 4 && newx2aR[1] < peakQR) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  med_threshR <- expression(Q >= newx2aR[1] & Q <= newx2aR[2] |  Q >= newx2aR[3] & Q <= newx2aR[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx2aR) == 4 && (newx2aR[1] < peakQR && newx2aR[2] < peakQR && newx2aR[3] < peakQR && newx2aR[4] < peakQR || newx2aR[1] > peakQR 
                                    && newx2aR[2] > peakQR && newx2aR[3] > peakQR && newx2aR[4] > peakQR  || newx2aR[2] < peakQR && newx2aR[3] > peakQR)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  med_threshR <- expression(Q <= newx2aR[1] & Q >= newx2aR[2] |  Q <= newx2aR[3] & Q >= newx2aR[4])
}

med_threshR

###  high threshold

if(is.na(newx3aR[1])) {
  high_threshR <- expression(Q < 0)
  ## if 1 threshold value and it's lower than the peak (ascending slope)
} else if(length(newx3aR)==1 && newx3aR < peakQR){
  # sum the amount of time above threshold
  high_threshR <- expression(Q >= newx3aR)
  
  ## if 1 threshold value and it's higher than the peak (descending slope)
} else if (length(newx3aR)==1 && newx3aR > peakQR){
  # sum the amount of time below the threshold
  high_threshR <- expression(Q <= newx3aR)
  
  ## if 2 threshold values and the first one is lower than the peak(positive parabol)
} else if (length(newx3aR)==2 && newx3aR[1] < peakQR) { 
  # sum the amount of time above the first and below the 2nd threshold
  high_threshR <- expression(Q >= newx3aR[1] & Q <= newx3aR[2])
  
  ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
} else if(length(newx3aR)==2 && (newx3aR[1] > peakQR || newx3aR[2] < peakQR )) {
  # sum the amount of time below the first and above the 2nd threshold
  high_threshR <- expression(Q <= newx3aR[1] & Q >= newx3aR[2])
  
  ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
} else if (length(newx3aR) == 3 && newx3aR[3] > peakQR) {
  # sum the amount of time below the first and above the 2nd threshold and below the 3rd
  high_threshR <- expression(Q <= newx3aR[1] | Q >= newx3aR[2] & Q <= newx3aR[3])
  
  ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
} else if (length(newx3aR) == 3 && newx3aR[1] < peakQR) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  high_threshR <- expression(Q >= newx3aR[1] & Q <= newx3aR[2] | Q >= newx3aR[3])
  
} else if (length(newx3aR) == 4 && newx3aR[1] < peakQR) {
  # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
  high_threshR <- expression(Q >= newx3aR[1] & Q <= newx3aR[2] |  Q >= newx3aR[3] & Q <= newx3aR[4])
  
  ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
} else if (length(newx3aR) == 4 && (newx3aR[1] < peakQR && newx3aR[2] < peakQR && newx3aR[3] < peakQR && newx3aR[4] < peakQR || newx3aR[1] > peakQR 
                                    && newx3aR[2] > peakQR && newx3aR[3] > peakQR && newx3aR[4] > peakQR  || newx3aR[2] < peakQR && newx3aR[3] > peakQR)) {
  # sum the amount of time above the first and below the 2nd threshold and above the 3rd
  high_threshR <- expression(Q <= newx3aR[1] & Q >= newx3aR[2] |  Q <= newx3aR[3] & Q >= newx3aR[4])
}

high_threshR

###### calculate amount of time

time_statsr <- new_dataRx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(eval(low_threshR))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshR))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshR))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshR))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshR))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshR))/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="ROB")

time_statsr


time_stats <- rbind(time_statsm, time_statsl, time_statsr)
time_stats
## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
melt_time

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()
melt_time_ann
# head(melt_time_ann)
unique(melt_time_ann$Probability.of.Mortality)

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
head(melt_time_seas)
melt_time_seas

write.csv(melt_time, "output_data/W1_willow_LA11_seedling_depth_time_stats.csv")

## plot for annual stats - need probs in order

png("Figures/Application_curves/Depth/LA11_seedling_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "non_critical")
unique(melt_time_winter$Probability_Threshold)

png("Figures/Application_curves/Depth/LA11_seedling_depth_perc_time_above_threshold_non_critical.png", width = 500, height = 600)

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Time within discharge limit in relation to Depth (non_critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "critical")

png("Figures/Application_curves/Depth/LA11_seedling_depth_perc_time_above_threshold_critical.png", width = 500, height = 600)

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Time within discharge limit in relation to Depth (critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

## count number events within each threshold with a running total - max total is the number of consequative 
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(eval(low_threshM))) %>%
  mutate(Low = if_else(eval(low_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(eval(med_threshM))) %>%
  mutate(Medium = if_else(eval(med_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(eval(high_threshM))) %>%
  mutate(High = if_else(eval(high_threshM), row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

new_dataL <- new_dataL %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(eval(low_threshL))) %>%
  mutate(Low = if_else(eval(low_threshL), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(eval(med_threshL))) %>%
  mutate(Medium = if_else(eval(med_threshL), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(eval(high_threshL))) %>%
  mutate(High = if_else(eval(high_threshL), row_number(), 0L))

new_dataL <- mutate(new_dataL, position="LOB")

new_dataR <- new_dataR %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(eval(low_threshR))) %>%
  mutate(Low = if_else(eval(low_threshR), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(eval(med_threshR))) %>%
  mutate(Medium = if_else(eval(med_threshR), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(eval(high_threshR))) %>%
  mutate(High = if_else(eval(high_threshR), row_number(), 0L))

new_dataR <- mutate(new_dataR, position="ROB")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_dataMx <- select(new_dataM, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_dataMx)
new_dataLx <- select(new_dataL, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_dataLx)
new_dataRx <- select(new_dataR, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_dataRx)
## has some values but just becuase of the fake thresholds
# range(new_dataRx$Medium)
new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/W1_LA11_willow_seedling_depth_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

# total_hours <- total_hours %>%
#   mutate(season = ifelse(month %in% winter, "winter", "summer") ) %>%
#   select(-day)

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/W1_LA11_willow_seedling_total_days_long.csv")

library(scales)

## plot all ts

png("Figures/Application_curves/Depth/LA11_seedling_depth_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits= c(0, 31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

## plot by year

png("Figures/Application_curves/Depth/LA11_seedling_depth_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  scale_y_continuous(limits= c(0, 31)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Depth: Mid Channel",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

dev.off()

## plot by season/critical period

png("Figures/Application_curves/Depth/LA11_seedling_depth_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  scale_y_continuous(limits= c(0, 31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
