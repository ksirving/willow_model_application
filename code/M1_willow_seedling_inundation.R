## willow model application
## Jenny Rogers & Katie Irving

library(sf)
library(lubridate)
library(tidyverse)

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

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100
# head(hyd_dep)
range(hyd_dep$depth_cm)

## plot depth v Q rating curve
plot(hyd_dep$Q, hyd_dep$depth_cm,  main = "F57C: Depth ~ Q", xlab="Q (cfs)", ylab="Depth (cm)")

## add date_num and plot time series - use numbers for now

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)

## predict on node data using model
names(hyd_dep)
names(inund)

## workflow
## get probabilities for depth at each hourly time step
## get thresholds i.e. 25, 50, 75%

# ?predict
# seed_pred <- predict(depth_seedling_mod, newdata = hyd_dep)
# 
# head(seed_pred)



