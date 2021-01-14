### figures graveyard
## plot time series
png("figures/Application_curves/nodes/F57C_Depth_TS.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = date_num, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Depth ~ Time Series",
       y = "Depth (cm)",
       x = "Date") #+ theme_bw(base_size = 15)
dev.off()

## plot for annual stats - need probs in order

png("figures/Application_curves/Depth/F57C_adult_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = water_year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
unique(melt_time_winter$Probability_Threshold)

png("figures/Application_curves/Depth/F57C_adult_depth_perc_time_above_threshold_winter.png", width = 500, height = 600)

ggplot(melt_time_winter, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Time within discharge limit in relation to Depth (Winter)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

png("figures/Application_curves/Depth/F57C_adult_depth_perc_time_above_threshold_summer.png", width = 500, height = 600)

ggplot(melt_time_summer, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Time within discharge limit in relation to Depth (Summer)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

## plot all ts
png("figures/Application_curves/Depth/F57C_adult_depth_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot by year
png("figures/Application_curves/Depth/F57C_adult_depth_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~water_year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Number of days within discharge limit in relation to Depth: Mid Channel",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()
## plot by season/critical period
png("figures/Application_curves/Depth/F57C_adult_depth_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()

##  plot time series of discharge - subset to one year
new_datax_2016 <- filter(new_dataMx, year==2016)

ggplot(new_datax_2016) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## plot each season of year - just winter & summer for now

winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months
new_datax_2016_winter <- filter(new_datax_2016, month %in% winter)
new_datax_2016_summer <- filter(new_datax_2016, month %in% summer)
tail(new_datax_2016_summer$DateTime)
# break.vec <- c(as.Date("2016-05-01 00:00:00 PDT"),
#                seq(from=as.Date("2016-05-01 00:00:00 PDT"), to=as.Date("2016-10-31 23:00:00 PDT"), by="month"))
# head(new_datax_2016_summer)

ggplot(new_datax_2016_summer) +
  geom_line(aes(x =month_year, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m") +
  expand_limits(x=min(break.vec))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

ggplot(new_datax_2016_winter) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## plot each season of year - just winter & summer for now

winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months
new_datax_2016_winter <- filter(new_datax_2016, month %in% winter)
new_datax_2016_summer <- filter(new_datax_2016, month %in% summer)

ggplot(new_datax_2016_summer) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

ggplot(new_datax_2016_winter) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## time stats
time_statsl <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Medium = if(is.na(newx2bL)){
    sum(Q >= newx2aL)/length(DateTime)*100
  } else {
    sum(Q >= newx2aL & Q <= newx2bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low = if(is.na(newx1bL)){
    sum(Q >= newx1aL)/length(DateTime)*100
  } else {
    sum(Q >= newx1aL & Q <= newx1bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High = if(is.na(newx3bL)){
    sum(Q >= newx3aL)/length(DateTime)*100
  } else {
    sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100
  })  %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Medium.Seasonal = if(is.na(newx2bL)){
    sum(Q >= newx2aL)/length(DateTime)*100
  } else {
    sum(Q >= newx2aL & Q <= newx2bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low.Seasonal = if(is.na(newx1bL)){
    sum(Q >= newx1aL)/length(DateTime)*100
  } else {
    sum(Q >= newx1aL & Q <= newx1bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High.Seasonal = if(is.na(newx3bL)){
    sum(Q >= newx3aL)/length(DateTime)*100
  } else {
    sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100
  }) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

## countinf=g numb er of days
new_dataL <- arrange(new_dataL, date_num)

nas <- ifelse(!is.na(newx1aL) && !is.na(newx1bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL & Q <= newx1bL)) %>%
    mutate(Low = if_else(Q >= newx1aL & Q <= newx1bL, row_number(), 0L))
} else if (is.na(newx1aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1bL)) %>%
    mutate(Low = if_else(Q <= newx1b, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL)) %>%
    mutate(Low = if_else(Q >= newx1aL, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx2aL) && !is.na(newx2bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aL & Q <= newx2bL)) %>%
    mutate(Medium = if_else(Q >= newx2aL & Q <= newx2bL, row_number(), 0L))
} else if (is.na(newx2aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2bL)) %>%
    mutate(Medium = if_else(Q <= newx2bL, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aL)) %>%
    mutate(Medium = if_else(Q >= newx2aL, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx3aL) && !is.na(newx3bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL & Q <= newx3bL)) %>%
    mutate(High = if_else(Q >= newx3aL & Q <= newx3bL, row_number(), 0L))
} else if (is.na(newx3aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3bL)) %>%
    mutate(High = if_else(Q <= newx3bL, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL)) %>%
    mutate(High = if_else(Q >= newx2aL, row_number(), 0L)) 
}
head(new_dataL)
names(new_dataL)
new_dataL <- mutate(new_dataL, position="LOB")

file_name = paste("figures/Application_curves/Depth/", NodeName, "_SAS_adult_depth_prob_Q_thresholds_updated_hyd.png", sep ="")

png(file_name, width = 500, height = 600)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[1,2]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[2,2]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[3,2]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[4,2]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[5,2]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[6,2]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[7,2]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[8,2]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[9,2]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[10,2]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[11,2]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[12,2]), color="blue") +
  # 
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[1,1]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[2,1]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[3,1]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[4,1]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[5,1]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[6,1]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[7,1]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[8,1]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[9,1]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[10,1]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[11,1]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[12,1]), color="blue") +
  # 
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2a[2]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.1, x=limits[3,3]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.1, x=limits[4,3]), color="green") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[5,3]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[6,3]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[7,3]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[8,3]), color="red") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[9,3]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[10,3]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[11,3]), color="blue") +
  # geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[12,3]), color="blue") +
  # 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = paste(NodeName, ": Adult/Depth: Probability ~ Q", sep=""),
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### node figure for depth ~ Q
# file_name <- paste("figures/Application_curves/nodes/", NodeName, "_Depth_Q_updated_hyd.png", sep="")
# png(file_name, width = 500, height = 600)
# 
# ggplot(hyd_dep, aes(x = Q, y=value)) +
#   geom_line(aes( group = variable, lty = variable)) +
#   scale_linetype_manual(values= c("dotted", "solid", "dashed"),
#                         breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
#   facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
#   labs(title = paste(NodeName, ": Depth ~ Q"),
#        y = "Depth (cm)",
#        x = "Q (cfs)") #+ theme_bw(base_size = 15)
# 
# dev.off()
