library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(lubridate)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/Data/HOBO/Temperature")

dat_temp <- read.csv('ALLTEMP.csv')
colnames(dat_temp)[2:17] <- c("1C","1T","2C","2C_SED","2T","2T_SED","3C","3T",
                              "4C","4T","5C","5C_SED","5T","5T_SED","6C","6T")
dat_temp$DateTime <- as.POSIXct(dat_temp$DateTime, tz = "America/Jamaica", "%Y-%m-%d %H:%M")

# dat_temp_long <- dat_temp %>%
#   pivot_longer(!DateTime, names_to = "Site", values_to = "Temperature_C") %>%
#   arrange(Site) %>%
#   mutate(Location = ifelse(Site %in% c("1C","1T","2C","2C_SED","2T","2T_SED","3C","3T"),"Central","Northern"))
# 
# dat_temp_long <- dat_temp_long %>%
#   mutate(Month = month(DateTime),
#          Day = day(DateTime),
#          Year = year(DateTime),
#          Hour = hour(DateTime),
#          Minute = minute(DateTime),
#          Date = as.Date(as.POSIXct(DateTime,tz="America/Jamaica")))

dat_temp <- dat_temp %>%
  mutate(Month = month(DateTime),
         Day = day(DateTime),
         Year = year(DateTime),
         Hour = hour(DateTime),
         Minute = minute(DateTime),
         Date = as.Date(as.POSIXct(DateTime,tz="America/Jamaica")))

dat_hourly <- dat_temp[dat_temp$Minute == 0,]
dat_hourly$Central <- rowMeans(subset(dat_hourly, select = c("2T_SED","2C_SED")))
dat_hourly$Central <- ifelse(is.na(dat_hourly$Central),dat_hourly$`2T_SED`,dat_hourly$`2C_SED`)
dat_hourly$Central <- ifelse(is.na(dat_hourly$Central),dat_hourly$`2C_SED`,dat_hourly$`2T_SED`)
dat_hourly$Northern <- rowMeans(subset(dat_hourly, select = c("5T_SED","5C_SED")))
dat_hourly$Northern <- ifelse(is.na(dat_hourly$Northern),dat_hourly$`5T_SED`,dat_hourly$`5C_SED`)
dat_hourly$Northern <- ifelse(is.na(dat_hourly$Northern),dat_hourly$`5C_SED`,dat_hourly$`5T_SED`)

central_lm <- lm(`2C_SED`~`2T_SED`, data = dat_hourly)
northern_lm <- lm(`5C_SED`~`5T_SED`, data = dat_hourly)
summary(central_lm)
summary(northern_lm)

central_plot <-
  ggplot(data = dat_hourly, aes(x = `2T_SED`, y = `2C_SED`)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = "red") +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = expression(paste("Central Treatment Sed. Temp. ( ", degree, "C)")),
       y = expression(paste("Central Control Sed. Temp. ( ", degree, "C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 33, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

northern_plot <-
  ggplot(data = dat_hourly, aes(x = `5T_SED`, y = `5C_SED`)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = "red") +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = expression(paste("Northern Treatment Sed. Temp. ( ", degree, "C)")),
       y = expression(paste("Northern Control Sed. Temp. ( ", degree, "C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 33, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

### Exported as width = 900 height = 500
central_plot + northern_plot + plot_layout(ncol = 2)
round(min(dat_hourly$Central, na.rm = T),2) # 1.44 C
round(min(dat_hourly$Northern, na.rm = T),2) # 1.44 C
round(min(dat_hourly$`2C_SED`, na.rm = T),2) # 1.44 C
round(min(dat_hourly$`2T_SED`, na.rm = T),2) # 6.57 C
round(min(dat_hourly$`5C_SED`, na.rm = T),2) # 1.66 C
round(min(dat_hourly$`5T_SED`, na.rm = T),2) # 1.44 C

control_lm <- lm(`5C_SED`~`2C_SED`, data = dat_hourly)
treatment_lm <- lm(`5T_SED`~`2T_SED`, data = dat_hourly)
summary(control_lm)
summary(treatment_lm)

control_plot <-
  ggplot(data = dat_hourly, aes(x = `2C_SED`, y = `5C_SED`)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = "red") +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = expression(paste("Central Control Sed. Temp. (", degree, "C)")),
       y = expression(paste("Northern Control Sed. Temp. (  ", degree, "C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 33, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

treatment_plot <-
  ggplot(data = dat_hourly, aes(x = `2T_SED`, y = `5T_SED`)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = "red") +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = expression(paste("Central Treatment Sed. Temp. (", degree, "C)")),
       y = expression(paste("Northern Treatment Sed. Temp. (  ", degree, "C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 33, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

### Exported as width = 900 height = 500
control_plot + treatment_plot + plot_layout(ncol = 2)

spatial_lm <- lm(Northern~Central, data = dat_hourly)
summary(spatial_lm)

ggplot(data = dat_hourly, aes(x = Central, y = Northern)) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = "red") +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = expression(paste("Central Sed. Temp. (", degree, "C)")),
       y = expression(paste("Northern Sed. Temp. ( ", degree, "C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 33, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

dat_daily <- dat_hourly %>%
  group_by(Date) %>%
  summarise(MeanCentral = round(mean(Central, na.rm = T),2),
            MaxCentral = round(max(Central, na.rm = T),2),
            MinCentral = round(min(Central, na.rm = T),2),
            MeanNorth = round(mean(Northern, na.rm = T),2),
            MaxNorth = round(max(Northern, na.rm = T),2),
            MinNorth = round(min(Northern, na.rm = T),2),
            DeltaT = MeanCentral - MeanNorth)
dat_daily <- dat_daily %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)),
         across(where(is.numeric), ~na_if(., -Inf)))
dat_daily <- as.data.frame(dat_daily)
dat_daily$MeanCentral[is.nan(dat_daily$MeanCentral)] <- NA
dat_daily$MeanNorth[is.nan(dat_daily$MeanNorth)] <- NA
dat_daily$DeltaT[is.nan(dat_daily$DeltaT)] <- NA
max(dat_daily$DeltaT, na.rm = T)
min(dat_daily$DeltaT, na.rm = T) # This value happens on a day with incomplete northern data creating an inflated daily mean temp which should be converted to NA
dat_daily[671,5:8] <- NA

ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = MeanCentral), color = "red", size = 1) +
  geom_line(aes(x = Date, y = MeanNorth), size = 1, alpha = 0.7) +
  # geom_ribbon(aes(x = date, y = MeanCentral,
  #               ymin = dat_daily$MinCentral, ymax = dat_daily$MaxCentral),
  #               color = "red", fill = "red", alpha = 0.1) +
  # geom_ribbon(aes(x = date, y = MeanNorth,
  #                 ymin = dat_daily$MinNorth, ymax = dat_daily$MaxNorth),
  #             color = "black", fill = "black", alpha = 0.1) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  scale_y_continuous(breaks = seq(0,35,5)) +
  labs(x = NULL,
       y = expression(paste("Daily Mean Sed. Temp. ( ", degree, "C)"))) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-07-01", "%Y-%m-%d"),
           xmax = as.Date("2020-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-09-01", "%Y-%m-%d"),
           xmax = as.Date("2020-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-11-01", "%Y-%m-%d"),
           xmax = as.Date("2020-12-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-01-01", "%Y-%m-%d"),
           xmax = as.Date("2021-02-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-03-01", "%Y-%m-%d"),
           xmax = as.Date("2021-04-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-05-01", "%Y-%m-%d"),
           xmax = as.Date("2021-06-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-07-01", "%Y-%m-%d"),
           xmax = as.Date("2021-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-09-01", "%Y-%m-%d"),
           xmax = as.Date("2021-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-11-01", "%Y-%m-%d"),
           xmax = as.Date("2021-12-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-01-01", "%Y-%m-%d"),
           xmax = as.Date("2022-02-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-03-01", "%Y-%m-%d"),
           xmax = as.Date("2022-04-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-05-01", "%Y-%m-%d"),
           xmax = as.Date("2022-06-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-07-01", "%Y-%m-%d"),
           xmax = as.Date("2022-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-09-01", "%Y-%m-%d"),
           xmax = as.Date("2022-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "white", color = "black",
           xmin = as.Date("2020-05-25", "%Y-%m-%d"),
           xmax = as.Date("2020-09-10", "%Y-%m-%d"),
           ymin = 32, ymax = 36) +
  annotate("segment", color = "black", size = 1,
           x = as.Date("2020-06-01", "%Y-%m-%d"),
           xend = as.Date("2020-06-20", "%Y-%m-%d"),
           y = 35, yend = 35) +
  annotate("segment", color = "red", size = 1,
           x = as.Date("2020-06-01", "%Y-%m-%d"),
           xend = as.Date("2020-06-20", "%Y-%m-%d"),
           y = 33, yend = 33) +
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = 35,
           label = 'Northern',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = 33.1,
           label = "Central",
           size = 5, fontface = 1, hjust = 0) + 
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(0, 36)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = -6, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -6, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -6, label = 2022, size = 6)

ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = DeltaT)) +
  geom_point(aes(x = Date, y = DeltaT), shape = 21, fill = "white", size = 1.5) +
  geom_abline(slope = 0, intercept = 0, linetype = "longdash", color = "black") +
  # geom_smooth(aes(x = Date, y = DeltaT), method = 'loess') +
  scale_y_continuous(breaks = seq(-2,4,0.5)) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-07-01", "%Y-%m-%d"),
           xmax = as.Date("2020-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-09-01", "%Y-%m-%d"),
           xmax = as.Date("2020-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-11-01", "%Y-%m-%d"),
           xmax = as.Date("2020-12-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-01-01", "%Y-%m-%d"),
           xmax = as.Date("2021-02-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-03-01", "%Y-%m-%d"),
           xmax = as.Date("2021-04-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-05-01", "%Y-%m-%d"),
           xmax = as.Date("2021-06-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-07-01", "%Y-%m-%d"),
           xmax = as.Date("2021-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-09-01", "%Y-%m-%d"),
           xmax = as.Date("2021-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-11-01", "%Y-%m-%d"),
           xmax = as.Date("2021-12-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-01-01", "%Y-%m-%d"),
           xmax = as.Date("2022-02-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-03-01", "%Y-%m-%d"),
           xmax = as.Date("2022-04-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-05-01", "%Y-%m-%d"),
           xmax = as.Date("2022-06-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-07-01", "%Y-%m-%d"),
           xmax = as.Date("2022-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-09-01", "%Y-%m-%d"),
           xmax = as.Date("2022-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "white", color = "black",
           xmin = as.Date("2020-06-15", "%Y-%m-%d"),
           xmax = as.Date("2021-01-10", "%Y-%m-%d"),
           ymin = 3.45, ymax = 4.1) +
  annotate("text", x = as.Date("2020-07-01", "%Y-%m-%d"), y = 4,
           label = 'Pos. = Central > North',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-07-01", "%Y-%m-%d"), y = 3.6,
           label = "Neg. = North > Central",
           size = 5, fontface = 1, hjust = 0) + 
  labs(x = NULL,
       y = expression(paste(Delta~"Sediment Temp ( ", degree, "C)"))) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(-2,4.1)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = -3, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -3, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -3, label = 2022, size = 6)

sed_dat <- dat_hourly[,c(23,21,24,25)]
x <- c("date","hour","sedtemp_central","sedtemp_northern")
colnames(sed_dat) <- x

round(sum(is.na(sed_dat$sedtemp_central))/NROW(sed_dat),2)*100 # 2%
round(sum(is.na(sed_dat$sedtemp_northern))/NROW(sed_dat),2)*100 # 4%

### 1989-2022 Wachapreague water level wrangling

wl_files <- list.files("D:/School/SeagrassRecovery/Data/Wachapreague/WaterLevel",full.names=T)
wl_list<-lapply(wl_files,read.csv)
wl_dat = data.table::rbindlist(wl_list)
rm(list=setdiff(ls(), c("sed_dat","wl_dat")))
wl_dat <- wl_dat[,c(1:2,5)]
x <- c("date","time","waterlevel_ft")
colnames(wl_dat) <- x
wl_dat_1989_2020 <- wl_dat[1:262992,]
wl_dat_2021_2022 <- wl_dat[262993:NROW(wl_dat),]
wl_dat_1989_2020$date <- as.Date(wl_dat_1989_2020$date, '%Y/%m/%d')
wl_dat_2021_2022$date <- as.Date(wl_dat_2021_2022$date, '%m/%d/%Y')
wl_dat_1989_2020$hour <- as.numeric(substr(wl_dat_1989_2020$time,1,nchar(wl_dat_1989_2020$time)-3))
wl_dat_2021_2022$hour <- as.numeric(substr(wl_dat_2021_2022$time,1,nchar(wl_dat_2021_2022$time)-3))
wl_dat <- rbind(wl_dat_1989_2020,wl_dat_2021_2022)
wl_dat <- wl_dat[,c(1,4,3)]
wl_dat$waterlevel_ft <- ifelse(wl_dat$waterlevel_ft == "-",NA,wl_dat$waterlevel_ft)
wl_dat$waterlevel_ft <- as.numeric(wl_dat$waterlevel_ft)
wl_dat$waterlevel_m <- round((wl_dat$waterlevel_ft * 0.3048),2)
rm(list=setdiff(ls(), c("sed_dat","wl_dat")))

### 1994-2022 Wachapreague water temp wrangling

wt_files <- list.files("D:/School/SeagrassRecovery/Data/Wachapreague/WaterTemp",full.names=T)
wt_list<-lapply(wt_files,read.csv)
wt_dat = data.table::rbindlist(wt_list)
rm(list=setdiff(ls(), c("sed_dat","wl_dat","wt_dat")))
wt_dat <- wt_dat[,c(1:3)]
x <- c("date","time","watertemp_f")
colnames(wt_dat) <- x
wt_dat_1994_2020 <- wt_dat[1:216408,]
wt_dat_2021_2022 <- wt_dat[216409:NROW(wt_dat),]
wt_dat_1994_2020$date <- as.Date(wt_dat_1994_2020$date, '%Y/%m/%d')
wt_dat_2021_2022$date <- as.Date(wt_dat_2021_2022$date, '%m/%d/%Y')
wt_dat_1994_2020$hour <- as.numeric(substr(wt_dat_1994_2020$time,1,nchar(wt_dat_1994_2020$time)-3))
wt_dat_2021_2022$hour <- as.numeric(substr(wt_dat_2021_2022$time,1,nchar(wt_dat_2021_2022$time)-3))
wt_dat <- rbind(wt_dat_1994_2020,wt_dat_2021_2022)
wt_dat <- wt_dat[,c(1,4,3)]
wt_dat$watertemp_f <- ifelse(wt_dat$watertemp_f == "-",NA,wt_dat$watertemp_f)
wt_dat$watertemp_f <- as.numeric(wt_dat$watertemp_f)
wt_dat$watertemp_c <- round(((wt_dat$watertemp_f-32)*(5/9)),2)
rm(list=setdiff(ls(), c("sed_dat","wl_dat","wt_dat")))

start <- as.Date("1989-01-01")
end <- as.Date("2022-10-10")
end-start
hours <- as.data.frame(rep(seq(0,23,1), 12336)) # add +1 to end-start
colnames(hours)[1] <- "hour"
dates <- as.data.frame(rep(seq(start,end,by = "day"), 24))
colnames(dates)[1] <- "date"
dates <- dates %>% arrange(date)
fillout <- cbind(dates,hours)

water_level_temp <- left_join(wl_dat,wt_dat, by = c("date","hour"))
water_level_temp <- water_level_temp[,c(1:2,4,6)]
water_level_temp  <- merge(water_level_temp, fillout, by = c("date","hour"), all.y = TRUE)

wl_diff <- diff(water_level_temp$waterlevel_m)
wl_diff <- c(NA, wl_diff)
water_level_temp$wl_diff <- wl_diff

# remove leap days
remove_leap <- as.Date(c("1992-02-29","1996-02-29","2000-02-29","2004-02-29",
                         "2008-02-29","2012-02-29","2016-02-29","2020-02-29"))
water_level_temp <- water_level_temp[!water_level_temp$date %in% remove_leap,]

# day of year that does not recognize leap day
water_level_temp <- water_level_temp %>% 
  mutate(DoY = day(date),
         Month = month(date),
         Year = year(date)) %>% 
  group_by(Year, Month) %>%
  mutate(DoY = DoY - lag(DoY, default = 0)) %>%
  group_by(Year) %>%
  mutate(DoY = cumsum(DoY)) %>%
  select(-Month)
water_level_temp <- as.data.frame(water_level_temp)
water_level_temp$Time <- format(strptime(water_level_temp$hour, format="%H"), format = "%H:%M")
water_level_temp$DateTime <- ymd_hm(paste(water_level_temp$date, water_level_temp$Time))

ggplot(data = water_level_temp, aes(x = DateTime, y = wl_diff)) +
  geom_line()

sed_wtemp_wlevel <- left_join(sed_dat,water_level_temp, by = c("date","hour"))
sed_wtemp_daily <- sed_wtemp_wlevel %>%
  group_by(date) %>%
  summarise(dailymean_central = round(mean(sedtemp_central, na.rm = T),2),
            dailymean_northern = round(mean(sedtemp_northern, na.rm = T),2),
            dailymean_wtemp = round(mean(watertemp_c, na.rm = T),2),
            deltaT_central = round((dailymean_central - dailymean_wtemp),2), # Wrong, should use measured Wtemp from Central
            deltaT_northern = round((dailymean_northern - dailymean_wtemp),2)) # Wrong, should use measured Wtemp from Central
sed_wtemp_daily$DoY <- yday(sed_wtemp_daily$date)

sum(sed_wtemp_daily$deltaT_central, na.rm = T)
sum(sed_wtemp_daily$deltaT_northern, na.rm = T)
mean(sed_wtemp_daily$deltaT_central, na.rm = T)
mean(sed_wtemp_daily$deltaT_northern, na.rm = T)

# test <- approx(sed_wtemp_daily$date,sed_wtemp_daily$deltaT_central,sed_wtemp_daily$date)
# test <- do.call(cbind.data.frame,test)

deltaT_central <- sed_wtemp_daily[,c(1,5)]
deltaT_central$location <- "Central"
x <- c("date","deltaT","Location")
colnames(deltaT_central) <- x
deltaT_northern <- sed_wtemp_daily[,c(1,6)]
deltaT_northern$location <- "Northern"
colnames(deltaT_northern) <- x
deltaT <- rbind(deltaT_central,deltaT_northern)

sed_wtemp_daily %>%
  ggplot() +
  geom_line(aes(x = date, y = deltaT_central), color = "red", size = 1) +
  geom_line(aes(x = date, y = deltaT_northern), color = "black", size = 1, alpha = 0.7) +
  geom_abline(slope = 0, intercept = 0, linetype = "longdash") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  scale_y_continuous(breaks = seq(-4,3,1)) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-07-01", "%Y-%m-%d"),
           xmax = as.Date("2020-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-09-01", "%Y-%m-%d"),
           xmax = as.Date("2020-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2020-11-01", "%Y-%m-%d"),
           xmax = as.Date("2020-12-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-01-01", "%Y-%m-%d"),
           xmax = as.Date("2021-02-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-03-01", "%Y-%m-%d"),
           xmax = as.Date("2021-04-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-05-01", "%Y-%m-%d"),
           xmax = as.Date("2021-06-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-07-01", "%Y-%m-%d"),
           xmax = as.Date("2021-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-09-01", "%Y-%m-%d"),
           xmax = as.Date("2021-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-11-01", "%Y-%m-%d"),
           xmax = as.Date("2021-12-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-01-01", "%Y-%m-%d"),
           xmax = as.Date("2022-02-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-03-01", "%Y-%m-%d"),
           xmax = as.Date("2022-04-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-05-01", "%Y-%m-%d"),
           xmax = as.Date("2022-06-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-07-01", "%Y-%m-%d"),
           xmax = as.Date("2022-08-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2022-09-01", "%Y-%m-%d"),
           xmax = as.Date("2022-10-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "white", color = "black",
           xmin = as.Date("2020-05-25", "%Y-%m-%d"),
           xmax = as.Date("2020-10-01", "%Y-%m-%d"),
           ymin = 2.5, ymax = 3.1) +
  annotate("rect", fill = "white", color = "black",
           xmin = as.Date("2020-05-25", "%Y-%m-%d"),
           xmax = as.Date("2020-12-31", "%Y-%m-%d"),
           ymin = -3.8, ymax = -2.8) +
  annotate("segment", color = "black", size = 1,
           x = as.Date("2020-06-25", "%Y-%m-%d"),
           xend = as.Date("2020-07-15", "%Y-%m-%d"),
           y = 2.65, yend = 2.65) +
  annotate("segment", color = "red", size = 1,
           x = as.Date("2020-06-25", "%Y-%m-%d"),
           xend = as.Date("2020-07-15", "%Y-%m-%d"),
           y = 3, yend = 3) +
  annotate("text", x = as.Date("2020-07-20", "%Y-%m-%d"), y = 2.65,
           label = 'Northern',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-07-20", "%Y-%m-%d"), y = 3,
           label = "Central",
           size = 5, fontface = 1, hjust = 0) + 
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = -3.0,
           label = 'Pos. = Stemp > Wtemp',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = -3.5,
           label = "Neg. = Wtemp > Stemp",
           size = 5, fontface = 1, hjust = 0) +
  labs(x = NULL,
       y = expression(paste("Daily Mean"~Delta~"Sediment-Water Temp ( ", degree, "C)"))) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(-4, 3.1)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = -5.1, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -5.1, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -5.1, label = 2022, size = 6)

### Multiple linear regression to predict long-term sediment temperature
set.seed(3456)

# break down observed dataset into training (75%) and validation (25%) datasets
test_dat <- sed_wtemp_wlevel
sample <- sample.int(n = nrow(test_dat), size = floor(0.75*nrow(test_dat)), replace = F)
Train <- test_dat[sample,]
Valid <- test_dat[-sample,]

# build model using training datasets
train_central_lm_test_wldiff <- lm(sedtemp_central~watertemp_c+wl_diff+DoY+hour, data = Train)
train_northern_lm_test_wldiff <- lm(sedtemp_northern~watertemp_c+wl_diff+DoY+hour, data = Train)
summary(train_central_lm_test_wldiff)
summary(train_northern_lm_test_wldiff)

# test model fits using validation datasets
pred_Valid_central_wldiff <- predict(train_central_lm_test_wldiff, newdata = Valid)
pred_Valid_northern_wldiff <- predict(train_northern_lm_test_wldiff, newdata = Valid)
Valid$pred_central_wldiff <- pred_Valid_central_wldiff
Valid$pred_northern_wldiff <- pred_Valid_northern_wldiff
Valid_lm_central_wldiff <- lm(pred_central_wldiff~sedtemp_central, data = Valid)
Valid_lm_northern_wldiff <- lm(pred_northern_wldiff~sedtemp_northern, data = Valid)
summary(Valid_lm_central_wldiff)
summary(Valid_lm_northern_wldiff)

central_plot_wldiff <-
  ggplot(data = Valid, aes(x = sedtemp_central, y = pred_central_wldiff)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red") +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 30, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

northern_plot_wldiff <-
  ggplot(data = Valid, aes(x = sedtemp_northern, y = pred_northern_wldiff)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red") +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 30, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

central_plot_wldiff + northern_plot_wldiff + plot_layout(ncol = 2)

longterm_dat <- water_level_temp[44977:nrow(water_level_temp),]
longterm_dat$doy <- yday(longterm_dat$date)
longterm_pred_central <- predict(train_central_lm_test_wldiff, newdata = longterm_dat)
longterm_pred_northern <- predict(train_northern_lm_test_wldiff, newdata = longterm_dat)
longterm_dat$stemp_pred_central <- longterm_pred_central
longterm_dat$stemp_pred_northern <- longterm_pred_northern

central_timeseries <-
  ggplot(data = longterm_dat, aes(x = date, y = stemp_pred_central)) +
  geom_line() +
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,32,5),
                     limits = c(0,32)) +
  labs(x = NULL,
       y = expression(paste("Central - Predicted Sed. Temp. (  ", degree, "C)"))) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("1995-01-01", "%Y-%m-%d"),
           xmax = as.Date("1995-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("1997-01-01", "%Y-%m-%d"),
           xmax = as.Date("1997-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("1999-01-01", "%Y-%m-%d"),
           xmax = as.Date("1999-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2001-01-01", "%Y-%m-%d"),
           xmax = as.Date("2001-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2003-01-01", "%Y-%m-%d"),
           xmax = as.Date("2003-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2005-01-01", "%Y-%m-%d"),
           xmax = as.Date("2005-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2007-01-01", "%Y-%m-%d"),
           xmax = as.Date("2007-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2009-01-01", "%Y-%m-%d"),
           xmax = as.Date("2009-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2011-01-01", "%Y-%m-%d"),
           xmax = as.Date("2011-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2013-01-01", "%Y-%m-%d"),
           xmax = as.Date("2013-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2015-01-01", "%Y-%m-%d"),
           xmax = as.Date("2015-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2017-01-01", "%Y-%m-%d"),
           xmax = as.Date("2017-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2019-01-01", "%Y-%m-%d"),
           xmax = as.Date("2019-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-01-01", "%Y-%m-%d"),
           xmax = as.Date("2021-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"))

northern_timeseries  <-
  ggplot(data = longterm_dat, aes(x = date, y = stemp_pred_northern)) +
  geom_line() +
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,32,5),
                     limits = c(0,32)) +
  labs(x = NULL,
       y = expression(paste("Northern - Predicted Sed. Temp. (  ", degree, "C)"))) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("1995-01-01", "%Y-%m-%d"),
           xmax = as.Date("1995-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("1997-01-01", "%Y-%m-%d"),
           xmax = as.Date("1997-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("1999-01-01", "%Y-%m-%d"),
           xmax = as.Date("1999-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2001-01-01", "%Y-%m-%d"),
           xmax = as.Date("2001-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2003-01-01", "%Y-%m-%d"),
           xmax = as.Date("2003-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2005-01-01", "%Y-%m-%d"),
           xmax = as.Date("2005-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2007-01-01", "%Y-%m-%d"),
           xmax = as.Date("2007-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2009-01-01", "%Y-%m-%d"),
           xmax = as.Date("2009-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2011-01-01", "%Y-%m-%d"),
           xmax = as.Date("2011-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2013-01-01", "%Y-%m-%d"),
           xmax = as.Date("2013-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2015-01-01", "%Y-%m-%d"),
           xmax = as.Date("2015-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2017-01-01", "%Y-%m-%d"),
           xmax = as.Date("2017-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2019-01-01", "%Y-%m-%d"),
           xmax = as.Date("2019-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = as.Date("2021-01-01", "%Y-%m-%d"),
           xmax = as.Date("2021-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"))

central_timeseries + northern_timeseries + plot_layout(ncol = 2)

longterm_dat_daily <- longterm_dat %>%
  group_by(date) %>%
  summarise(Wtemp_c = round(mean(watertemp_c, na.rm = T),2),
            Stemp_central = round(mean(stemp_pred_central, na.rm = T),2),
            stemp_northern = round(mean(stemp_pred_northern, na.rm = T),2),
            deltaT_central = Stemp_central - Wtemp_c,
            deltaT_northern = stemp_northern - Wtemp_c)
longterm_dat_daily$doy <- yday(longterm_dat_daily$date)

central_longterm_daily <- longterm_dat_daily[,c(1,3)]
x <- c("date","Temp")
colnames(central_longterm_daily) <- x
central_longterm_daily$Station <- "central"
northern_longterm_daily <- longterm_dat_daily[,c(1,4)]
colnames(northern_longterm_daily) <- x
northern_longterm_daily$Station <- "northern"
water_longterm_daily <- longterm_dat_daily[,c(1:2)]
colnames(water_longterm_daily) <- x
water_longterm_daily$Station <- "water"

all_daily_data <- rbind(central_longterm_daily,
                        northern_longterm_daily,
                        water_longterm_daily)
all_daily_data$Temp[is.nan(all_daily_data$Temp)] <- NA
missing <- all_daily_data %>%
  mutate(Year = year(date)) %>%
  group_by(Year,Station) %>%
  summarise(DaysPerYear = sum(!is.na(Temp)))
View(missing)

library(heatwaveR)

zz <- unique(all_daily_data$Station)

for(i in 1:length(zz)){
  curDat = all_daily_data[all_daily_data$Station == zz[i],]
  ts_Warm = ts2clm(curDat, x = date, y = Temp,
                   climatologyPeriod = c(min(curDat$date), max(curDat$date)))
  de_Warm_Temp = detect_event(ts_Warm, x = date, y = Temp )
  cat_Warm = category(de_Warm_Temp, y = Temp, S = FALSE)
  curEventsWarm = de_Warm_Temp$event
  curEventsWarm$Station = zz[i]
  curCatWarm = cat_Warm
  curCatWarm$Station = zz[i]
  if( i == 1){
    saveDat_Temp = curEventsWarm
    saveCat_Temp = curCatWarm
  } else{
    saveDat_Temp = rbind(saveDat_Temp, curEventsWarm)
    saveCat_Temp = rbind(saveCat_Temp, curCatWarm)
  }
}

hw_table_obs <- saveDat_Temp %>%
  group_by(Station) %>%
  summarise(Total_Obs_HW = max(event_no))
hw_table_obs
saveCat_Temp_water <- saveCat_Temp[saveCat_Temp$Station == "water",]
saveCat_Temp_central <- saveCat_Temp[saveCat_Temp$Station == "central",]
saveCat_Temp_northern <- saveCat_Temp[saveCat_Temp$Station == "northern",]
saveCat_Temp_water$Year <- year(saveCat_Temp_water$peak_date)
saveCat_Temp_central$Year <- year(saveCat_Temp_central$peak_date)
saveCat_Temp_northern$Year <- year(saveCat_Temp_northern$peak_date)
dat_water <- saveCat_Temp_water %>% count(category, Year, sort = TRUE)
dat_central <- saveCat_Temp_central %>% count(category, Year, sort = TRUE)
dat_northern <- saveCat_Temp_northern %>% count(category, Year, sort = TRUE)

fillout_year <- rep(seq(from = 1994, to = 2022, by = 1), 4) # 4 heatwave categories
fillout_year <- sort(fillout_year)
fillout_cat <- rep(c("I Moderate", "II Strong", "III Severe", "IV Extreme"), 29) # 29 year time series
fillout <- cbind(fillout_year,fillout_cat)
fillout <- as.data.frame(fillout)
names(fillout)[1] <- "Year"
names(fillout)[2] <- "category"

water_fill <- merge(dat_water, fillout, by = c("Year","category"), all = TRUE)
central_fill <- merge(dat_central, fillout, by = c("Year","category"), all = TRUE)
northern_fill <- merge(dat_northern, fillout, by = c("Year","category"), all = TRUE)
water_fill$n[is.na(water_fill$n)] <- 0
central_fill$n[is.na(central_fill$n)] <- 0
northern_fill$n[is.na(northern_fill$n)] <- 0

water_sum_cat <- saveCat_Temp_water %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))
central_sum_cat <- saveCat_Temp_central %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))
northern_sum_cat <- saveCat_Temp_northern %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))

water_sum <- saveCat_Temp_water %>%
  group_by(Year) %>%
  summarise(TotalDuration = sum(duration))
central_sum <- saveCat_Temp_central %>%
  group_by(Year) %>%
  summarise(TotalDuration = sum(duration))
northern_sum <- saveCat_Temp_northern %>%
  group_by(Year) %>%
  summarise(TotalDuration = sum(duration))
fillout_sum <- as.data.frame(seq(1994,2022,1))
colnames(fillout_sum)[1] <- "Year"
water_sum <- merge(water_sum, fillout_sum, by = "Year", all = TRUE)
central_sum <- merge(central_sum, fillout_sum, by = "Year", all = TRUE)
northern_sum <- merge(northern_sum, fillout_sum, by = "Year", all = TRUE)
water_sum[is.na(water_sum)] <- 0
central_sum[is.na(central_sum)] <- 0
northern_sum[is.na(northern_sum)] <- 0
water_sum[c(13:14,24),2] <- NA # years with no data should be NA not 0 (years 2006,2007,2017)
central_sum[c(13:14,24),2] <- NA # years with no data should be NA not 0 (years 2006,2007,2017)
northern_sum[c(13:14,24),2] <- NA # years with no data should be NA not 0 (years 2006,2007,2017)

saveDat_Temp %>%
  group_by(Station) %>%
  summarise(TotalEvent = NROW(duration),
            MeanDuration = round(mean(duration)),
            MaxDuration = max(duration),
            MeanInt_relThres = round(mean(intensity_max_relThresh),digits = 1),
            MaxInt_relThres = round(max(intensity_max_relThresh),digits = 1),
            MeanInt = round(mean(intensity_max),digits = 1),
            MaxInt = round(max(intensity_max),digits = 1))

library(forcats)

figA <- ggplot(data = water_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  xlab("Year") +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  annotate("text", x = 1994, y = 8, label = "a)", size = 6) +
  ylab("Number of MHW Events") +
  guides(fill=guide_legend(title="Category")) +
  annotate("text", x = c(2006.5,2017), y = c(0.5,6.5),
           label = "NA",
           size = 5, fontface = 1, hjust = 0.5) +
  geom_segment(aes(x = 2017,
                   y = 6,
                   xend = 2017,
                   yend = 2),
               arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = c(0.25,0.81),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

figB <- ggplot(data = central_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  annotate("text", x = 1994, y = 8, label = "b)", size = 6) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  ylab("Number of SHW Events - Central") +
  guides(fill=guide_legend(title="Category")) +
  annotate("text", x = c(2006.5,2017), y = c(0.5,6.5),
           label = "NA",
           size = 5, fontface = 1, hjust = 0.5) +
  geom_segment(aes(x = 2017,
                   y = 6,
                   xend = 2017,
                   yend = 3),
               arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

figC <- ggplot(data = northern_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  annotate("text", x = 1994, y = 8, label = "c)", size = 6) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  ylab("Number of SHW Events - Northern") +
  guides(fill=guide_legend(title="Category")) +
  annotate("text", x = c(2006.5,2017), y = c(0.5,6.5),
           label = "NA",
           size = 5, fontface = 1, hjust = 0.5) +
  geom_segment(aes(x = 2017,
                   y = 6,
                   xend = 2017,
                   yend = 2),
               arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

# water_sum_lm <- lm(water_sum$TotalDuration~water_sum$Year)
# summary(water_sum_lm)

figD <- ggplot(data = water_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(d", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  ylab("Total MHW Days") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1995, label.y = 50, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

# central_sum_lm <- lm(central_sum$TotalDuration~central_sum$Year)
# summary(central_sum_lm)

figE <- ggplot(data = central_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(e", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  ylab("Total SHW Days - Central") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1995, label.y = 50, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

# northern_sum_lm <- lm(northern_sum$TotalDuration~northern_sum$Year)
# summary(northern_sum_lm)

figF <- ggplot(data = northern_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(f", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  ylab("Total SHW Days - Northern") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1995, label.y = 50, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

figA + figD + figB + figE + figC + figF + plot_layout(ncol = 2, nrow = 3)

library(Kendall)
library(trend)
ts = ts(data = water_sum[, 2],
        frequency = 1,
        start = 1,
        end = 29)
MannKendall(ts)
ss$estimates
