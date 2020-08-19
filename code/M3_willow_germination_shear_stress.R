## willow germination and depth

library(tidyverse)
library(lubridate)

## upload hydraulic data

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
head(hydraul)

hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100

## format date time
hyd_dep$DateTime<-as.POSIXct(hyd_dep$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "America/Los_Angeles")

## create year, month, day and hour columns

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime))

## define thresholds for germination

depth_germ_cm <- 5
depth_germ_lo_dur_days <-  85
depth_germ_hi_dur_days <- 280
# watab_adult_cm <- 300
# streampwr_lo <- 20
# streampwr_hi <- 4000



most_consecutive_val = function(vector, val = 1) { 
  with(rle(vector), max(lengths[values == val]))
}




grm <- hyd_dep %>% 
  # select(1:3, 5, 9, 13, 16:18) %>% 
  # gather(4:6, key = "position1", value = "depth_ft") %>% 
  # mutate(depth_cm = depth_ft * 30.48,
  #        position = gsub(pattern = "Depth_ft_", replacement = "", x = position1)) %>% 
  # select(-position1, -depth_ft) %>% 
  group_by(month) %>% 
  summarize(germ_depth_suitability = most_consecutive_val(vector=abs(diff(which(depth_cm >= depth_germ_cm))==1)))
head(grm)

suitability <- 
  data.frame(
    "year" = grm$year, 
    # "node" = grm$position,
    "lifestage" = "germination", 
    "variable" = "depth", 
    "suitability" = ifelse(grm$germ_depth_suitability >= depth_germ_lo_dur_days*24 & grm$germ_depth_suitability <= depth_germ_hi_dur_days*24, "suitable", "unsuitable")
    
  )

suitability <- unique(suitability)

ggplot(grm, mapping = aes(y = depth_cm, x = DateTime, color = position))+
  geom_line()+
  labs(x = "Date", y = "Depth (cm)")

