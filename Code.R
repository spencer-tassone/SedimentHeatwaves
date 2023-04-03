library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(lubridate)
library(heatwaveR)
library(tidyr)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/Data/HOBO/Temperature")

# pull in observed water and sediment temperature from SG resil experiment
dat_temp <- read.csv('ALLTEMP.csv')
colnames(dat_temp)[2:17] <- c("1C","1T","2C","2C_SED","2T","2T_SED","3C","3T",
                              "4C","4T","5C","5C_SED","5T","5T_SED","6C","6T")
dat_temp$DateTime <- as.POSIXct(dat_temp$DateTime, tz = "America/Jamaica", "%m/%d/%Y %H:%M")

dat_temp <- dat_temp %>%
  mutate(Month = month(DateTime),
         Day = day(DateTime),
         Year = year(DateTime),
         Hour = hour(DateTime),
         Minute = minute(DateTime),
         Date = as.Date(as.POSIXct(DateTime,tz="America/Jamaica")))

dat_hourly <- dat_temp[dat_temp$Minute == 0,]
dat_hourly$central_sedtemp <- rowMeans(subset(dat_hourly, select = c("2T_SED","2C_SED")), na.rm = T)
dat_hourly$northern_sedtemp <- rowMeans(subset(dat_hourly, select = c("5T_SED","5C_SED")), na.rm = T)
dat_hourly$central_watertemp <- rowMeans(subset(dat_hourly, select = c("1C","1T","2C","2T","3C","3T")), na.rm = T)
dat_hourly$northern_watertemp <- rowMeans(subset(dat_hourly, select = c("4C","4T","5C","5T","6C","6T")), na.rm = T)
dat_hourly <- dat_hourly %>%
  mutate(delta_sediment = round(central_sedtemp - northern_sedtemp,2), # positive = central warmer
         delta_water = round(central_watertemp - northern_watertemp,2), # positive = central warmer
         delta_sed_water_north = round( northern_watertemp - northern_sedtemp,2), # positive = water warmer
         delta_sed_water_central = round(central_watertemp - central_sedtemp,2)) # positive = water warmer

# how much data is missing?
round(sum(is.na(dat_hourly$central_sedtemp))/NROW(dat_hourly),3)*100 # 1.9% for central sediment temperature
round(sum(is.na(dat_hourly$northern_sedtemp))/NROW(dat_hourly),3)*100 # 3.9% for northern sediment temperature
round(sum(is.na(dat_hourly$central_watertemp))/NROW(dat_hourly),3)*100 # 0.4% for central sediment temperature
round(sum(is.na(dat_hourly$northern_watertemp))/NROW(dat_hourly),3)*100 # 0.7% for northern sediment temperature

dat_daily <- dat_hourly %>%
  group_by(Date) %>%
  summarise(MeanCentral_sedtemp = round(mean(central_sedtemp, na.rm = T),2),
            MeanNorth_sedtemp = round(mean(northern_sedtemp, na.rm = T),2),
            MeanCentral_watertemp = round(mean(central_watertemp, na.rm = T),2),
            MeanNorth_watertemp = round(mean(northern_watertemp, na.rm = T),2),
            DeltaT_sedtemp = round(mean(delta_sediment, na.rm = T),2),
            DeltaT_watertemp = round(mean(delta_water, na.rm = T),2),
            DeltaT_sedwater_north = round(mean(delta_sed_water_north, na.rm = T),2),
            DeltaT_sedwater_central = round(mean(delta_sed_water_central, na.rm = T),2))

dat_daily <- dat_daily %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)),
         across(where(is.numeric), ~na_if(., -Inf)),
         across(where(is.numeric), ~ifelse(is.nan(.),NA,.)))
dat_daily <- as.data.frame(dat_daily)

central_lm <- lm(`2C_SED`~`2T_SED`, data = dat_hourly)
northern_lm <- lm(`5C_SED`~`5T_SED`, data = dat_hourly)
# summary(central_lm)
# summary(northern_lm)

# Test Figure 1: Control v. Treatment Sediment Temperature ----
# compare treatment (i.e., seagrass shoot removal sites) and
# control (i.e., no seagrass shoot removal sites) sediment
# temperature at central and northern locations
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

control_lm <- lm(`5C_SED`~`2C_SED`, data = dat_hourly)
treatment_lm <- lm(`5T_SED`~`2T_SED`, data = dat_hourly)
# summary(control_lm)
# summary(treatment_lm)

# Test Figure 2: Control v. Treatment Sediment Temperature ----
# compare sediment temperature between treatment (i.e., seagrass shoot removal)
# sites and control (i.e., no seagrass shoot removal) sites among
# central and northern locations
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

spatial_lm <- lm(northern_sedtemp~central_sedtemp, data = dat_hourly)
# summary(spatial_lm)

# Test Figure 3: Compare sediment temperature between central and northern locations ----
ggplot(data = dat_hourly, aes(x = central_sedtemp, y = northern_sedtemp)) +
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

setwd("D:/School/SeagrassRecovery/Data/Sediment")

# Figure 1: Hourly mean sediment temperature from central and northern locations ----
round(max(dat_hourly$central_sedtemp, na.rm = T),1) # 32.1 oC
round(max(dat_hourly$northern_sedtemp, na.rm = T),1) # 31.6 oC
round(min(dat_hourly$central_sedtemp, na.rm = T),1) # 1.4 oC
round(min(dat_hourly$northern_sedtemp, na.rm = T),1) # 1.5 oC
round(max(dat_hourly$central_sedtemp, na.rm = T),1) - round(min(dat_hourly$central_sedtemp, na.rm = T),1) # 30.7 oC
round(max(dat_hourly$northern_sedtemp, na.rm = T),1) - round(min(dat_hourly$northern_sedtemp, na.rm = T),1) # 30.1 oC

Fig1a <- ggplot(data = dat_hourly) +
  geom_line(aes(x = Date, y = central_sedtemp), color = "red", size = 1) +
  geom_line(aes(x = Date, y = northern_sedtemp), size = 1, alpha = 0.7) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b",
               limits = as.Date(c('2020-06-01','2022-11-01')),
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,30,5),
                     limits = c(0,33)) +
  labs(x = NULL,
       y = expression(paste("Hourly Sediment Temperature (  ", degree, "C)"))) +
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
           xmin = as.Date("2020-06-25", "%Y-%m-%d"),
           xmax = as.Date("2020-10-25", "%Y-%m-%d"),
           ymin = 1.85, ymax = 6) +
  annotate("segment", color = "black", size = 1,
           x = as.Date("2020-07-05", "%Y-%m-%d"),
           xend = as.Date("2020-07-25", "%Y-%m-%d"),
           y = 5, yend = 5) +
  annotate("segment", color = "red", size = 1,
           x = as.Date("2020-07-05", "%Y-%m-%d"),
           xend = as.Date("2020-07-25", "%Y-%m-%d"),
           y = 3, yend = 3) +
  annotate("text", x = as.Date("2020-07-30", "%Y-%m-%d"), y = 5,
           label = 'Edge',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-07-30", "%Y-%m-%d"), y = 3.1,
           label = "Central",
           size = 5, fontface = 1, hjust = 0) + 
  annotate("text", x = as.Date("2022-10-02", "%Y-%m-%d"), y = 32,
           label = "(a",
           size = 6, fontface = 1, hjust = 0) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

# Difference in daily sediment temperature between central and northern locations
# width = 1000 height = 700
max(dat_daily$DeltaT_sedtemp, na.rm = T) # 4.1 C
abs(min(dat_daily$DeltaT_sedtemp, na.rm = T)) # 1.1 C

Fig1b <- ggplot(data = dat_daily) +
  geom_smooth(aes(x = Date, y = DeltaT_sedtemp), method = 'loess', span = 0.4, color = 'black', se = F) +
  # geom_line(aes(x = Date, y = DeltaT_sedtemp)) +
  geom_point(aes(x = Date, y = DeltaT_sedtemp), shape = 21, fill = "white", size = 2) +
  geom_abline(slope = 0, intercept = 0, linetype = "longdash", color = "black") +
  scale_y_continuous(breaks = seq(-1,4,0.5)) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b",
               limits = as.Date(c('2020-06-01','2022-11-01')),
               expand = c(0, 0)) +
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
           xmax = as.Date("2021-02-05", "%Y-%m-%d"),
           ymin = 3.4, ymax = 4.2) +
  annotate("text", x = as.Date("2020-07-01", "%Y-%m-%d"), y = 4,
           label = 'Pos. = Central > Edge',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-07-01", "%Y-%m-%d"), y = 3.6,
           label = "Neg. = Edge > Central",
           size = 5, fontface = 1, hjust = 0) + 
  annotate("text", x = as.Date("2022-10-02", "%Y-%m-%d"), y = 4,
           label = "(b",
           size = 6, fontface = 1, hjust = 0) +
  labs(x = NULL,
       y = expression(paste(Delta~"Daily Mean Sediment Temperature (  ",  degree, "C)"))) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(-1,4.1)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = -2.1, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -2.1, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -2.1, label = 2022, size = 6)

# width = 900 height = 1000
Fig1a + Fig1b + plot_layout(ncol = 1)

# Figure 2: Hourly water and sediment temperature (including difference) for both location ----
central <- dat_hourly[,c(1,24,26,31)]
northern <- dat_hourly[,c(1,25,27,30)]

# dat_hourly %>%
#   summarise(max_central_water = round(max(central_watertemp, na.rm = T),1),
#             min_central_water = round(min(central_watertemp, na.rm = T),1),
#             max_central_sed = round(max(central_sedtemp, na.rm = T),1),
#             min_central_sed = round(min(central_sedtemp, na.rm = T),1),
#             max_northern_water = round(max(northern_watertemp, na.rm = T),1),
#             min_northern_water = round(min(northern_watertemp, na.rm = T),1),
#             max_northern_sed = round(max(northern_sedtemp, na.rm = T),1),
#             min_northern_sed = round(min(northern_sedtemp, na.rm = T),1),
#             range_central_water = max_central_water-min_central_water,
#             range_central_sed = max_central_sed-min_central_sed,
#             range_northern_water = max_northern_water-min_northern_water,
#             range_northern_sed = max_northern_sed-min_northern_sed,
#             range_central_watersed = range_central_water-range_central_sed,
#             range_northern_watersed = range_northern_water-range_northern_sed,
#             max_central_diff = max(delta_sed_water_central, na.rm = T),
#             min_central_diff = min(delta_sed_water_central, na.rm = T),
#             max_northern_diff = max(delta_sed_water_north, na.rm = T),
#             min_northern_diff = min(delta_sed_water_north, na.rm = T),
#             range_central_diff = round(max_central_diff - min_central_diff,1),
#             range_northern_diff = round(max_northern_diff - min_northern_diff,1)) 

central <- central %>%
  mutate(Location = "Central",
         Date = date(DateTime)) %>%
  rename("Sediment" = "central_sedtemp",
         "Water" = "central_watertemp",
         "Difference" = "delta_sed_water_central") %>%
  tidyr::pivot_longer(cols = c('Sediment','Water','Difference'),
                      names_to = 'Variable',
                      values_to = 'Temperature')

northern <- northern %>%
  mutate(Location = "Edge",
         Date = date(DateTime)) %>%
  rename("Sediment" = "northern_sedtemp",
         "Water" = "northern_watertemp",
         "Difference" = "delta_sed_water_north") %>%
  tidyr::pivot_longer(cols = c('Sediment','Water','Difference'),
                      names_to = 'Variable',
                      values_to = 'Temperature')

df1 <- rbind(central,northern)
df1$Variable <- factor(df1$Variable, levels = c("Water","Sediment","Difference"))

p1 <- df1 %>%
  subset(!Variable == "Difference") %>%
  mutate(var_water_sed = 
           if_else(Variable %in% c("Water", "Sediment"), 
                   true = "Water & Sediment", 
                   false = "Difference")) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Temperature, group = Variable, color = Variable)) +
  scale_color_manual(values = alpha(c("blue", "chocolate4"), c(0.4,1)),
                     breaks = c('Water','Sediment')) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b",
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,30,5)) +
  labs(x = NULL,
       y = expression(paste("Temperature ( ", degree, "C)"))) +
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
  theme_bw() +
  theme(plot.margin = unit(c(0, 1, 2, 0), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.15,0.56),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(0,33)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-10-15", "%Y-%m-%d"), y = -9.8, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -9.8, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -9.8, label = 2022, size = 6) +
  facet_grid(forcats::fct_rev(Location)~var_water_sed)

dat_text1 <- data.frame(
  label = c("(a","(b"),
  Location = c('Edge','Central'),
  x = c(as.Date("2022-08-15", "%Y-%m-%d"),
        as.Date("2022-08-15", "%Y-%m-%d")),
  y = c(3,3))

p1 <- p1 + geom_text(
  data    = dat_text1,
  mapping = aes(x = x, y = y, label = label,
                hjust   = 0,
                vjust   = 1),
  size = 5)

p2 <- df1 %>%
  mutate(var_water_sed = 
           if_else(Variable %in% c("Water", "Sediment"), 
                   true = "Water & Sediment", 
                   false = "Difference")) %>%
  subset(!var_water_sed == c('Water & Sediment'),) %>%
  ggplot() +
  geom_line(aes(x = Date, y = Temperature, group = Variable, color = Variable)) +
  geom_hline(yintercept = 0, color = "red", linetype = 'longdash') +
  scale_color_manual(values = "black") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b",
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-4,4,1)) +
  labs(x = NULL,
       y = expression(paste(Delta~" Temperature ( ",  degree, "C)"))) +
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
  theme_bw() +
  theme(plot.margin = unit(c(0, 1, 2, 0), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = 'none',
        strip.background = element_rect(fill = "gray90"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 16)) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(-4.5,4.1)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-10-15", "%Y-%m-%d"), y = -7.05, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -7.05, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -7.05, label = 2022, size = 6) +
  facet_grid(forcats::fct_rev(Location)~var_water_sed)

dat_text2 <- data.frame(
  label = c("Positive = Water > Sediment", "Negative = Sediment > Water", "(c","(d"),
  Location = c('Edge','Edge','Edge','Central'),
  x = c(as.Date("2020-08-01", "%Y-%m-%d"),
        as.Date("2020-08-01", "%Y-%m-%d"),
        as.Date("2020-08-01", "%Y-%m-%d"),
        as.Date("2020-08-01", "%Y-%m-%d")),
  y = c(4.2,3.5,-3.7,-3.7))

p2 <- p2 + geom_text(
  data    = dat_text2,
  mapping = aes(x = x, y = y, label = label,
                hjust   = 0,
                vjust   = 1,
                size = 10),
  size = 5)

# width = 1000 height = 700
p1 + p2 + plot_layout(ncol = 2)

# how much hourly sediment temperature is missing?
round(sum(is.na(dat_hourly$central_sedtemp))/NROW(dat_hourly),2)*100 # 2%
round(sum(is.na(dat_hourly$northern_sedtemp))/NROW(dat_hourly),2)*100 # 4%

# Sediment Temperature Model ----
# Need long-term sediment temperature time series for heatwave analysis,
# therefore we build a statistical model using water temp, water level, DoY, and hour as covariates
## 1989-2022 Wachapreague water level wrangling (https://tidesandcurrents.noaa.gov/waterlevels.html?id=8631044)

wl_files <- list.files("D:/School/SeagrassRecovery/Data/Wachapreague/WaterLevel",full.names=T)
wl_list<-lapply(wl_files,read.csv)
wl_dat = data.table::rbindlist(wl_list)
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

### 1994-2022 Wachapreague water temp wrangling (https://tidesandcurrents.noaa.gov/physocean.html?id=8631044)

wt_files <- list.files("D:/School/SeagrassRecovery/Data/Wachapreague/WaterTemp",full.names=T)
wt_list<-lapply(wt_files,read.csv)
wt_dat = data.table::rbindlist(wt_list)
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

water_level_temp <- water_level_temp %>%
  filter(date >= as.Date('1994-01-01') &
           date <= as.Date('2022-10-10'))

# how much missing data is there
sum(is.na(water_level_temp$wl_diff)) # 34348
sum(is.na(water_level_temp$watertemp_c)) # 53143

# linear interpolate over gaps <= 5 hours
water_level_temp <- water_level_temp %>%
  mutate(watertemp_c = zoo::na.approx(watertemp_c, maxgap = 5, na.rm = F),
         wl_diff = zoo::na.approx(wl_diff, maxgap = 5, na.rm = F))

# how much missing data is there
sum(is.na(water_level_temp$wl_diff)) # 34348
sum(is.na(water_level_temp$watertemp_c)) # 50374

sed_wtemp_wlevel <- dat_hourly[,c(23,21,24:25)]
colnames(sed_wtemp_wlevel)[1:2] <- c('date','hour')
sed_wtemp_wlevel$hour <- as.numeric(sed_wtemp_wlevel$hour)
sed_wtemp_wlevel <- merge(sed_wtemp_wlevel,water_level_temp, by = c('date', 'hour'))

### Multiple linear regression to predict long-term sediment temperature
set.seed(3456)

# break down observed dataset into training (75%) and validation (25%) datasets
test_dat <- sed_wtemp_wlevel
sample <- sample.int(n = nrow(test_dat), size = floor(0.75*nrow(test_dat)), replace = F)
Train <- test_dat[sample,]
Valid <- test_dat[-sample,]

# build model using training datasets
train_central_lm <- lm(central_sedtemp~watertemp_c+wl_diff+DoY+hour, data = Train)
train_northern_lm <- lm(northern_sedtemp~watertemp_c+wl_diff+DoY+hour, data = Train)

# Table 2: Multiple Linear Regression Model Results ----
library(kableExtra)

cen <- summary(train_central_lm)
edge <- summary(train_northern_lm)

table2 <- data.frame(matrix(ncol = 6, nrow = 10))
colnames(table2)[1:6] <- c("Location","Variable","Estimate ± SE","Variable p-val.","Adj. R<sup> 2</sup>","Model p-val.")
table2$Variable <- c("Intercept",
                     "Water Temp. (<sup> o</sup>C)",
                     "Water Level (m)",
                     "Day of Year",
                     "Hour",
                     "Intercept",
                     "Water Temp. (<sup> o</sup>C)",
                     "Water Level (m)",
                     "Day of Year",
                     "Hour")
table2$Location <- c('Edge',"","","","",
                     'Central',"","","","")
table2$`Estimate ± SE` <- c(paste(round(edge$coefficients[1,1],3),round(edge$coefficients[1,2],3), sep = " ± "),
                            paste(round(edge$coefficients[2,1],3),round(edge$coefficients[2,2],3), sep = " ± "),
                            paste(round(edge$coefficients[3,1],3),round(edge$coefficients[3,2],3), sep = " ± "),
                            paste(round(edge$coefficients[4,1],3),round(edge$coefficients[4,2],3), sep = " ± "),
                            paste(round(edge$coefficients[5,1],3),round(edge$coefficients[5,2],3), sep = " ± "),
                            paste(round(cen$coefficients[1,1],3),round(cen$coefficients[1,2],3), sep = " ± "),
                            paste(round(cen$coefficients[2,1],3),round(cen$coefficients[2,2],3), sep = " ± "),
                            paste(round(cen$coefficients[3,1],3),round(cen$coefficients[3,2],3), sep = " ± "),
                            paste(round(cen$coefficients[4,1],3),round(cen$coefficients[4,2],3), sep = " ± "),
                            paste(round(cen$coefficients[5,1],3),round(cen$coefficients[5,2],3), sep = " ± "))
table2$`Variable p-val.` <- c('< 0.001','< 0.001','< 0.001','< 0.001','< 0.001',
                              '< 0.001','< 0.001',round(cen$coefficients[3,4],3),'< 0.001','< 0.001')
table2$`Adj. R<sup> 2</sup>` <- c(round(edge$adj.r.squared,3),"","","","",
                                  round(cen$adj.r.squared,3),"","","","")
table2$`Model p-val.` <- c('< 0.001',"","","","",
                           '< 0.001',"","","","")

setwd("D:/School/SeagrassRecovery/Data/Sediment")

table2 %>%
  kbl(escape =  F, align = 'llcccc') %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'left') %>%
  kable_classic(html_font = "Cambria") %>%
  column_spec(1:5, background = 'white') %>%
  save_kable(file = 'Table2.html')

# test model fits using validation datasets
pred_Valid_central <- predict(train_central_lm, newdata = Valid)
pred_Valid_northern <- predict(train_northern_lm, newdata = Valid)
Valid$pred_central <- pred_Valid_central
Valid$pred_northern <- pred_Valid_northern
Valid_lm_central <- lm(pred_central~central_sedtemp, data = Valid)
Valid_lm_northern <- lm(pred_northern~northern_sedtemp, data = Valid)
round(summary(Valid_lm_central)$adj.r.squared,2) # adj. R2 = 0.98
round(summary(Valid_lm_northern)$adj.r.squared,2) # adj. R2 = 0.97

cent <- Valid[,c(3,12)]
cent <- as.data.frame(cent)
colnames(cent)[1:2] <- c('obs_sedtemp','pred_sedtemp')
cent$site <- 'Central'

north <- Valid[,c(4,13)]
north <- as.data.frame(north)
colnames(north)[1:2] <- c('obs_sedtemp','pred_sedtemp')
north$site <- 'Edge'

Fig4 <- rbind(cent,north)

# Figure 4: Linear regression between predicted and observed sediment temperature ----
# width = 600 height = 900
Fig4 %>%
  mutate(across(site, factor, levels = c('Edge', 'Central'))) %>%
  ggplot(aes(x = obs_sedtemp, y = pred_sedtemp)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red", size = 1) +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,30,5),
                     limits = c(0,32)) +
  scale_y_continuous(breaks = seq(0,30,5),
                     limits = c(0,32)) +
  ylab(expression(paste("Predicted Sediment Temperature (  ", degree,"C)"))) +
  xlab(expression(paste("Observed Sediment Temperature (", degree,"C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 29, size = 5) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 31, size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 16)) +
  facet_wrap(~site, ncol = 1, strip.position = 'right')

# Test Figure 4: Model fit for warm vs. cool sediment temperatures ----
test_warm <- Fig4 %>%
  filter(obs_sedtemp >= 25)
test_cool <- Fig4 %>%
  filter(obs_sedtemp < 25)

test_warm %>%
  mutate(across(site, factor, levels = c('Edge', 'Central'))) %>%
  ggplot(aes(x = obs_sedtemp, y = pred_sedtemp)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red", size = 1) +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(25,32,1),
                     limits = c(25,32)) +
  scale_y_continuous(breaks = seq(25,32,1),
                     limits = c(25,32)) +
  ylab(expression(paste("Predicted Sediment Temperature (  ", degree,"C)"))) +
  xlab(expression(paste("Observed Sediment Temperature (", degree,"C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 25, label.y = 31.5, size = 5) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 25, label.y = 32, size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 16)) +
  facet_wrap(~site, ncol = 1, strip.position = 'right')

test_cool %>%
  mutate(across(site, factor, levels = c('Edge', 'Central'))) %>%
  ggplot(aes(x = obs_sedtemp, y = pred_sedtemp)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red", size = 1) +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,25,5),
                     limits = c(0,25)) +
  scale_y_continuous(breaks = seq(0,25,5),
                     limits = c(0,25)) +
  ylab(expression(paste("Predicted Sediment Temperature (  ", degree,"C)"))) +
  xlab(expression(paste("Observed Sediment Temperature (", degree,"C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 0, label.y = 23, size = 5) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 0, label.y = 25, size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 16)) +
  facet_wrap(~site, ncol = 1, strip.position = 'right')

longterm_dat <- water_level_temp
longterm_dat$doy <- yday(longterm_dat$date)
longterm_pred_central <- predict(train_central_lm, newdata = longterm_dat)
longterm_pred_northern <- predict(train_northern_lm, newdata = longterm_dat)
longterm_dat$stemp_pred_central <- longterm_pred_central
longterm_dat$stemp_pred_northern <- longterm_pred_northern

central <- longterm_dat[,c(1:11)]
north <- longterm_dat[,c(1:10,12)]
central$Location <- "Central"
north$Location <- "Edge"
colnames(central)[11] <- "Temperature"
colnames(north)[11] <- "Temperature"
Fig5 <- rbind(central, north)

# Figure 5: Hourly modeled sediment temperature ----
# width = 1200 height = 900
Fig5 %>%
  ggplot(aes(x = date, y = Temperature)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,32,5),
                     limits = c(0,32)) +
  labs(x = NULL,
       y = expression(paste("Modeled Hourly Sediment Temperature (   ", degree, "C)"))) +
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
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 16)) +
  facet_wrap(~forcats::fct_rev(Location), ncol = 1, strip.position = 'right')

longterm_dat_daily <- longterm_dat %>%
  group_by(date) %>%
  summarise(Wtemp_c = round(mean(watertemp_c, na.rm = T),2),
            stemp_central = round(mean(stemp_pred_central, na.rm = T),2),
            stemp_northern = round(mean(stemp_pred_northern, na.rm = T),2),
            deltaT_central = stemp_central - Wtemp_c,
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

# Atmospheric temperature time series ----
setwd("D:/School/SeagrassRecovery/Data/Sediment")

# download gap-filled meteorological data from the VCR @ http://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.337
# knb-lter-vcr.337.1
# Groleger, N., J. Morreale and JH. Porter. 2021. Gap-filled Meteorological Data for the Virginia Coast Reserve LTER - 1989-2021. Virginia Coast Reserve Long-Term Ecological Research Project Data Publication knb-lter-vcr.337.1 (doi:10.6073/pasta/811cfcf82679e7969856a0d33f3d4194 ).
dat <- read.csv("VCR_DailyGapFilled_v2.csv")

# isolate air temperature data from Oyster between 1994-2022
atm_longterm_daily <- dat %>%
  filter(STATION == "OYSM") %>%
  mutate(date = as.Date(DATE, format = "%m/%d/%Y"),
         Temp = as.numeric(AVG_T),
         Station = 'air') %>%
  filter(date >= as.Date('1994-01-01'),
         date <= as.Date('2022-12-31')) %>%
  select(date,Temp,Station)

# create daily time series of dates between Jan 1 1994 and Dec 31 2022
fill <- as.data.frame(matrix(nrow = NROW(seq(as.Date('1994-01-01'),
                                             as.Date('2022-12-31'),1)),
                             ncol = 1))
colnames(fill)[1] <- 'date'
fill$date <- seq(as.Date('1994-01-01'),
                 as.Date('2022-12-31'),1)

# how many days should there be in the daily time series?
NROW(fill) # there are 10,592 days in time series

# how many days are there be in the time series?
NROW(atm_longterm_daily) # there are 10,619 days in time series = 27 'extra' days

# how many days are duplicated?
sum(duplicated(atm_longterm_daily$date), na.rm = T) # 27

# which rows in the time series are duplicated?
which(duplicated(atm_longterm_daily$date))

# remove duplicated rows in time series
atm_longterm_daily <- atm_longterm_daily[!duplicated(atm_longterm_daily$date),]

# double check to see if there are any duplicates
which(duplicated(atm_longterm_daily$date)) # 0 - all duplicates have been removed

# visualize time series
atm_longterm_daily %>%
  ggplot(aes(x = date, y = Temp)) +
  geom_line() +
  labs(x = "",
       y = "VCR Daily Mean Air Temperature (C)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

# Are there long-term trends in atmospheric and water temperature? ----
library(wql) # Seasonal Kendall trend test

# convert daily atmo temperature time series into monthly
atm_longterm_monthly <- atm_longterm_daily %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year,month) %>%
  summarise(Temp_monthly = round(mean(Temp, na.rm = T),2)) %>%
  ungroup() %>%
  select(Temp_monthly)

# convert dataframe to ts object
atemp_ts = ts(data = atm_longterm_monthly,
              start = c(1994,1),
              end = c(2022,12),
              frequency = 12)
round(seaKen(atemp_ts)$sen.slope,3) # annual rate of air temperature increase of 0.032 C per year
seaKen(atemp_ts)$p.value # p-value < 0.001

# convert daily water temperature time series into monthly
water_longterm_monthly <- water_longterm_daily %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year,month) %>%
  summarise(Total_obs = sum(is.nan(Temp)),
            Temp_monthly = round(mean(Temp, na.rm = T),2))

# convert months with >= 10 days of missing daily data to NaN
water_longterm_monthly$Temp_monthly[water_longterm_monthly$Total_obs >= 10] <- NaN

# isolate monthly mean water temperature column
water_longterm_monthly <- water_longterm_monthly %>%
  ungroup() %>%
  select(Temp_monthly)

# convert dataframe to ts object
wtemp_ts = ts(data = water_longterm_monthly,
              start = c(1994,1),
              end = c(2022,12),
              frequency = 12)
round(seaKen(wtemp_ts)$sen.slope,3) # annual rate of water temperature increase of 0.041 C per year
seaKen(wtemp_ts)$p.value # p-value < 0.001

# cannot add atmospheric time series to water and sediment b/c difference in HW definition
water_sed_daily_data <- rbind(central_longterm_daily,
                        northern_longterm_daily,
                        water_longterm_daily)
water_sed_daily_data$Temp[is.nan(water_sed_daily_data$Temp)] <- NA

yy <- unique(atm_longterm_daily$Station)
zz <- unique(water_sed_daily_data$Station)

# Heatwave detection ----
for(i in 1:length(yy)){
  curDat = atm_longterm_daily[atm_longterm_daily$Station == yy[i],]
  ts_Warm = ts2clm(curDat, x = date, y = Temp,
                   climatologyPeriod = c(min(curDat$date), max(curDat$date)))
  de_Warm_Temp = detect_event(ts_Warm, x = date, y = Temp, minDuration = 3, maxGap = 0)
  cat_Warm = category(de_Warm_Temp, y = Temp, S = FALSE)
  curEventsWarm = de_Warm_Temp$event
  curEventsWarm$Station = yy[i]
  curCatWarm = cat_Warm
  curCatWarm$Station = yy[i]
  if( i == 1){
    saveDat_Temp_air = curEventsWarm
    saveCat_Temp_air = curCatWarm
  } else{
    saveDat_Temp_air = rbind(saveDat_Temp_air, curEventsWarm)
    saveCat_Temp_air = rbind(saveCat_Temp_air, curCatWarm)
  }
}
for(i in 1:length(zz)){
  curDat = water_sed_daily_data[water_sed_daily_data$Station == zz[i],]
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

# now combine atmo HW output with water and sediment HW output
saveDat_Temp <- rbind(saveDat_Temp_air,saveDat_Temp)

saveDat_Temp %>%
  group_by(Station) %>%
  summarise(Total_Obs_HW = max(event_no)) # 125 air, 66 central, 64 northern, 67 water

saveCat_Temp_water <- saveCat_Temp[saveCat_Temp$Station == "water",]
saveCat_Temp_central <- saveCat_Temp[saveCat_Temp$Station == "central",]
saveCat_Temp_northern <- saveCat_Temp[saveCat_Temp$Station == "northern",]
saveCat_Temp_air$Year <- year(saveCat_Temp_air$peak_date)
saveCat_Temp_water$Year <- year(saveCat_Temp_water$peak_date)
saveCat_Temp_central$Year <- year(saveCat_Temp_central$peak_date)
saveCat_Temp_northern$Year <- year(saveCat_Temp_northern$peak_date)

# What percentile was the 14-day duration of the June 2015 MHW?
quantile(saveCat_Temp_water$duration, probs = seq(.90, .99, by = .01)) # 94th percentile

# Top 10% most extreme heatwaves ----

saveCat_Temp_air %>%
  filter(rank(desc(i_max)) <= round(NROW(saveCat_Temp_air)*0.1)) %>%
  mutate(Month = month(peak_date),
         Season = ifelse(Month >=12, "Winter",
                         ifelse(Month >= 9, "Fall",
                                ifelse(Month >= 6, "Summer",
                                       ifelse(Month >= 3, "Spring",
                                              ifelse(Month >=1, "Winter", NA)))))) %>%
  select(peak_date,category,i_max,duration,Year,Month,Season) %>%
  arrange(desc(i_max)) # 12 total - 8 in winter, 2 in spring, 2 in fall - all between Nov-March

saveCat_Temp_water %>%
  filter(rank(desc(i_max)) <= round(NROW(saveCat_Temp_water)*0.1)) %>%
  mutate(Month = month(peak_date),
         Season = ifelse(Month >=12, "Winter",
                         ifelse(Month >= 9, "Fall",
                                ifelse(Month >= 6, "Summer",
                                       ifelse(Month >= 3, "Spring",
                                              ifelse(Month >=1, "Winter", NA)))))) %>%
  select(peak_date,category,i_max,duration,Year,Month,Season) %>%
  arrange(desc(i_max)) # 7 total - 4 in winter, 3 in spring - all between Dec-April

saveCat_Temp_northern %>%
  filter(rank(desc(i_max)) <= round(NROW(saveCat_Temp_northern)*0.1)) %>%
  mutate(Month = month(peak_date),
         Season = ifelse(Month >=12, "Winter",
                         ifelse(Month >= 9, "Fall",
                                ifelse(Month >= 6, "Summer",
                                       ifelse(Month >= 3, "Spring",
                                              ifelse(Month >=1, "Winter", NA)))))) %>%
  select(peak_date,category,i_max,duration,Year,Month,Season) %>%
  arrange(desc(i_max)) # 6 total - 3 in winter, 3 in spring - all between Dec-April

saveCat_Temp_central %>%
  filter(rank(desc(i_max)) <= round(NROW(saveCat_Temp_central)*0.1)) %>%
  mutate(Month = month(peak_date),
         Season = ifelse(Month >=12, "Winter",
                         ifelse(Month >= 9, "Fall",
                                ifelse(Month >= 6, "Summer",
                                       ifelse(Month >= 3, "Spring",
                                              ifelse(Month >=1, "Winter", NA)))))) %>%
  select(peak_date,category,i_max,duration,Year,Month,Season) %>%
  arrange(desc(i_max)) # 7 total - 3 in winter, 3 in spring, 1 in fall - all between Nov-April

dat_air <- saveCat_Temp_air %>% count(category, Year, sort = TRUE)
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

air_fill <- merge(dat_air, fillout, by = c("Year","category"), all = TRUE) 
water_fill <- merge(dat_water, fillout, by = c("Year","category"), all = TRUE)
central_fill <- merge(dat_central, fillout, by = c("Year","category"), all = TRUE)
northern_fill <- merge(dat_northern, fillout, by = c("Year","category"), all = TRUE)
air_fill$n[is.na(air_fill$n)] <- 0
water_fill$n[is.na(water_fill$n)] <- 0
central_fill$n[is.na(central_fill$n)] <- 0
northern_fill$n[is.na(northern_fill$n)] <- 0

air_sum_cat <- saveCat_Temp_air %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))
water_sum_cat <- saveCat_Temp_water %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))
central_sum_cat <- saveCat_Temp_central %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))
northern_sum_cat <- saveCat_Temp_northern %>%
  group_by(Year, category) %>%
  summarise(TotalDuration = sum(duration))

air_sum <- saveCat_Temp_air %>%
  group_by(Year) %>%
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
air_sum <- merge(air_sum, fillout_sum, by = "Year", all = TRUE)
water_sum <- merge(water_sum, fillout_sum, by = "Year", all = TRUE)
central_sum <- merge(central_sum, fillout_sum, by = "Year", all = TRUE)
northern_sum <- merge(northern_sum, fillout_sum, by = "Year", all = TRUE)
air_sum[is.na(air_sum)] <- 0
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

# Figure 7: Total heatwaves by location and season ----
# width = 800 height = 600

saveCat_Temp$Year <- year(saveCat_Temp$peak_date)
saveCat_Temp <- rbind(saveCat_Temp_air,saveCat_Temp)
saveCat_Temp$Station <- stringr::str_to_title(saveCat_Temp$Station)
saveCat_Temp$Station <- ifelse(saveCat_Temp$Station == "Northern","Edge",saveCat_Temp$Station)
saveCat_Temp$Station <- ifelse(saveCat_Temp$Station == "Air","Atmosphere",saveCat_Temp$Station)

cols <- c("Atmosphere" = "black", "Water" = "#525252", "Edge" = "#cccccc", "Central" = "white")
saveCat_Temp %>%
  mutate(Month = month(peak_date),
         Season = ifelse(Month >=12, "Winter",
                         ifelse(Month >= 9, "Fall",
                                ifelse(Month >= 6, "Summer",
                                       ifelse(Month >= 3, "Spring",
                                              ifelse(Month >=1, "Winter", NA)))))) %>%
  select(peak_date,category,i_max,duration,Month,Season,Station) %>%
  group_by(Station,Season) %>%
  summarise(Total = n()) %>%
  ggplot(aes(x = Season, y = Total, fill = (Station), group = factor(Station, levels = c("Central","Edge","Water","Atmosphere")))) +
  geom_bar(stat = 'identity',position = position_dodge(), color = 'black') +
  scale_fill_manual(values = cols) +
  labs(x = "Season", y = "Total Heatwaves") +
  scale_y_continuous(breaks = seq(0,45,5),
                     limits = c(0,46)) +
  scale_x_discrete(limits=c("Spring", "Winter", "Fall", "Summer")) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = c(0.85,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.background = element_blank())

library(tidyverse)

saveCat_Temp %>%
  mutate(Month = month(peak_date),
         Season = ifelse(Month >=12, "Winter",
                         ifelse(Month >= 9, "Fall",
                                ifelse(Month >= 6, "Summer",
                                       ifelse(Month >= 3, "Spring",
                                              ifelse(Month >=1, "Winter", NA)))))) %>%
  select(Season,Station) %>%
  group_by(Station,Season) %>%
  summarise(Total = n()) %>%
  pivot_wider(names_from = Station,
              values_from = Total) %>%
  mutate(frac_air = round((Atmosphere/sum(Atmosphere))*100,),
         frac_water = round((Water/sum(Water))*100),
         frac_edge = round((Edge/sum(Edge))*100),
         frac_cent = round((Central/sum(Central))*100))

# Lag between atmo HW and pelagic HW, and lag between pelagic HW and sediment HW ----

lag_air <- saveDat_Temp %>%
  filter(Station == 'air') %>%
  select(date_start,date_end,Station)
lag_water <- saveDat_Temp %>%
  filter(Station == 'water') %>%
  select(date_start,date_end,Station)
lag_northern <- saveDat_Temp %>%
  filter(Station == 'northern') %>%
  select(date_start,date_end,Station)
lag_central <- saveDat_Temp %>%
  filter(Station == 'central') %>%
  select(date_start,date_end,Station)

lag_air <- as.data.frame(lag_air)
lag_water <- as.data.frame(lag_water)
lag_northern <- as.data.frame(lag_northern)
lag_central <- as.data.frame(lag_central)

# Find overlap of dates
lag_water$Overlap <- lag_water$date_start %in% unlist(Map(':', lag_air$date_start, lag_air$date_end))
lag_northern$Overlap <- lag_northern$date_start %in% unlist(Map(':', lag_water$date_start, lag_water$date_end))
lag_central$Overlap <- lag_central$date_start %in% unlist(Map(':', lag_water$date_start, lag_water$date_end))

# Water temperature
# Loop through rows
for (i in 1:nrow(lag_water)) {
  
  # Go through only those that overlap
  if (lag_water[i, "Overlap"]) {
    
    # Loop through all rows in other data frame
    for (j in 1:nrow(lag_air)) {
      
      # Check if within range of lag_water
      sec_date_range <- lag_air[j, "date_start"]:lag_air[j, "date_end"]
      if (lag_water[i, "date_start"] %in% sec_date_range) {
        
        # Find absolute difference in start dates
        lag_water[i, "diff"] <- lag_water[i, "date_start"] - lag_air[j, "date_start"]
        lag_water[i, "diff"] <- abs(lag_water[i, "diff"])
      }
    }
  }
}

# Northern edge sediment
# Loop through rows
for (i in 1:nrow(lag_northern)) {
  
  # Go through only those that overlap
  if (lag_northern[i, "Overlap"]) {
    
    # Loop through all rows in other data frame
    for (j in 1:nrow(lag_water)) {
      
      # Check if within range of lag_northern
      sec_date_range <- lag_water[j, "date_start"]:lag_water[j, "date_end"]
      if (lag_northern[i, "date_start"] %in% sec_date_range) {
        
        # Find absolute difference in start dates
        lag_northern[i, "diff"] <- lag_northern[i, "date_start"] - lag_water[j, "date_start"]
        lag_northern[i, "diff"] <- abs(lag_northern[i, "diff"])
      }
    }
  }
}

# Central sediment
# Loop through rows
for (i in 1:nrow(lag_central)) {
  
  # Go through only those that overlap
  if (lag_central[i, "Overlap"]) {
    
    # Loop through all rows in other data frame
    for (j in 1:nrow(lag_water)) {
      
      # Check if within range of lag_central
      sec_date_range <- lag_water[j, "date_start"]:lag_water[j, "date_end"]
      if (lag_central[i, "date_start"] %in% sec_date_range) {
        
        # Find absolute difference in start dates
        lag_central[i, "diff"] <- lag_central[i, "date_start"] - lag_water[j, "date_start"]
        lag_central[i, "diff"] <- abs(lag_central[i, "diff"])
      }
    }
  }
}

# Filter and print result
lag_water[lag_water$Overlap, ]
lag_northern[lag_northern$Overlap, ]
lag_central[lag_central$Overlap, ]

summary(lag_water$Overlap) # 22 (33%) pelagic HW overlapped with atmo HW, 45 pelagic HW did not overlap with atmo HW
summary(lag_northern$Overlap) # 53 (79%) sed HW overlapped with pelagic HW, 11 sed HW did not overlap with pelagic HW
summary(lag_central$Overlap) # 54 (81%) sed HW overlapped with pelagic HW, 12 sed HW did not overlap with pelagic HW

lag_water %>%
  summarise(Max_lag = max(diff, na.rm = T),
            Min_lag = min(diff, na.rm = T),
            Avg_lag = round(mean(diff, na.rm = T)),
            SD_lag = round(sd(diff, na.rm = T))) # max = 4 days, min = 0 days, avg = 1 day, sd = 1 day

lag_northern %>%
  summarise(Max_lag = max(diff, na.rm = T),
            Min_lag = min(diff, na.rm = T),
            Avg_lag = round(mean(diff, na.rm = T)),
            SD_lag = round(sd(diff, na.rm = T))) # max = 0 days, min = 0 days, avg = 0 days, sd = 0 days

lag_central %>%
  summarise(Max_lag = max(diff, na.rm = T),
            Min_lag = min(diff, na.rm = T),
            Avg_lag = round(mean(diff, na.rm = T)),
            SD_lag = round(sd(diff, na.rm = T))) # max = 1 days, min = 0 days, avg = 0 days, sd = 0 days

library(forcats)

# Figure 6: Heatwave results ----

fig6A <- ggplot(data = air_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  xlab("Year") +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  annotate("text", x = 1994, y = 8, label = "a)", size = 6) +
  labs(y = expression(atop("Atmosphere",
                           paste("Total HW Events"))),
       x = NULL) +
  guides(fill=guide_legend(title="Category")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

fig6C <- ggplot(data = water_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  xlab("Year") +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  annotate("text", x = 1994, y = 8, label = "c)", size = 6) +
  labs(y = expression(atop("Water Column",
                           paste("Total HW Events"))),
       x = NULL) +
  guides(fill=guide_legend(title="Category")) +
  annotate("text", x = c(2006.5,2017), y = c(0.5,6.5),
           label = "NA",
           size = 5, fontface = 1, hjust = 0.5) +
  geom_segment(aes(x = 2017, y = 6,
                   xend = 2017, yend = 2),
               arrow = ggplot2::arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = c(0.24,0.79),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

fig6E <- ggplot(data = northern_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  annotate("text", x = 1994, y = 8, label = "e)", size = 6) +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  labs(y = expression(atop("Sediment - Edge",
                           paste("Total HW Events"))),
       x = NULL) +
  guides(fill=guide_legend(title="Category")) +
  annotate("text", x = c(2006.5,2017), y = c(0.5,6.5),
           label = "NA",
           size = 5, fontface = 1, hjust = 0.5) +
  geom_segment(aes(x = 2017,
                   y = 6,
                   xend = 2017,
                   yend = 2),
               arrow = ggplot2::arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

fig6G <- ggplot(data = central_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  annotate("text", x = 1994, y = 8, label = "g)", size = 6) +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  labs(y = expression(atop("Sediment - Central",
                           paste("Total HW Events"))),
       x = NULL) +
  guides(fill=guide_legend(title="Category")) +
  annotate("text", x = c(2006.5,2017), y = c(0.5,6.5),
           label = "NA",
           size = 5, fontface = 1, hjust = 0.5) +
  geom_segment(aes(x = 2017,
                   y = 6,
                   xend = 2017,
                   yend = 3),
               arrow = ggplot2::arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

test <- lm(TotalDuration~Year, data = air_sum)
summary(test)
round((0.2084 * 1994) - 402.3813) # 13 days
round((0.2084 * 2022) - 402.3813) # 19 days
plot(cooks.distance(test))
abline(h = 4/NROW(central_sum), col="red") # https://stats.stackexchange.com/questions/164099/removing-outliers-based-on-cooks-distance-in-r-language

fig6B <- ggplot(data = air_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 34, label = "(b", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0,30,5),
                     limits = c(0,34)) +
  labs(y = expression(atop("Atmosphere",
                           paste("Total HW Days"))),
       x = NULL) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 34, size = 5) +
  annotate("text", x = 1995, y = 30.5,
           label = 'y = 0.2084x - 402.3813',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

test <- lm(TotalDuration~Year, data = water_sum)
summary(test)
round((0.6713 * 1994) - 1327.7352) # 11 days
round((0.6713 * 2022) - 1327.7352) # 30 days
plot(cooks.distance(test))
abline(h = 4/NROW(central_sum), col="red") # https://stats.stackexchange.com/questions/164099/removing-outliers-based-on-cooks-distance-in-r-language

fig6D <- ggplot(data = water_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(d", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  labs(y = expression(atop("Water Column",
                           paste("Total HW Days"))),
       x = NULL) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 5) +
  annotate("text", x = 1995, y = 53,
           label = 'y = 0.6713x - 1327.7352',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

test <- lm(TotalDuration~Year, data = northern_sum)
summary(test)
round((0.6702 * 1994) - 1325.1681) # 11 days
round((0.6702 * 2022) - 1325.1681) # 30 days
plot(cooks.distance(test))
abline(h = 4/NROW(central_sum), col="red") # https://stats.stackexchange.com/questions/164099/removing-outliers-based-on-cooks-distance-in-r-language

fig6F <- ggplot(data = northern_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(f", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  labs(y = expression(atop("Sediment - Edge",
                           paste("Total HW Days"))),
       x = NULL) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 5) +
  annotate("text", x = 1995, y = 53,
           label = 'y = 0.6702x - 1325.1681',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

test <- lm(TotalDuration~Year, data = central_sum)
summary(test)
round((0.6749 * 1994) - 1334.4366) # 11 days
round((0.6749 * 2022) - 1334.4366) # 30 days
plot(cooks.distance(test))
abline(h = 4/NROW(central_sum), col="red") # https://stats.stackexchange.com/questions/164099/removing-outliers-based-on-cooks-distance-in-r-language

fig6H <- ggplot(data = central_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(h", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 2)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  labs(y = expression(atop("Sediment - Central",
                           paste("Total HW Days"))),
       x = NULL) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 5) +
  annotate("text", x = 1995, y = 53,
           label = 'y = 0.6749x - 1334.4366',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

# width = 1200 height = 1500
fig6A + fig6B + fig6C + fig6D + fig6E + fig6F + fig6G + fig6H + plot_layout(ncol = 2, nrow = 4)

# Co-occurring heatwaves in June 2015

ts_air = ts2clm(atm_longterm_daily, x = date, y = Temp,
                  climatologyPeriod = c(min(atm_longterm_daily$date), max(atm_longterm_daily$date)))
de_air_temp = detect_event(ts_air, x = date, y = Temp, minDuration = 3, maxGap = 0)

ts_water = ts2clm(water_longterm_daily, x = date, y = Temp,
                  climatologyPeriod = c(min(water_longterm_daily$date), max(water_longterm_daily$date)))
de_water_temp = detect_event(ts_water, x = date, y = Temp)

ts_northern = ts2clm(northern_longterm_daily, x = date, y = Temp,
                     climatologyPeriod = c(min(northern_longterm_daily$date), max(northern_longterm_daily$date)))
de_northern_temp = detect_event(ts_northern, x = date, y = Temp)

ts_central = ts2clm(central_longterm_daily, x = date, y = Temp,
                    climatologyPeriod = c(min(central_longterm_daily$date), max(central_longterm_daily$date)))
de_central_temp = detect_event(ts_central, x = date, y = Temp)

air_clim_cat <- de_air_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7822:7866)

water_clim_cat <- de_water_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7822:7866)

northern_clim_cat <- de_northern_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7822:7866)

central_clim_cat <- de_central_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7822:7866)

# Set category fill colours
fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000")

# Figure 8: Concurrent heatwaves in atmosphere, water column, and sediments ----

fig8_air <- ggplot(data = air_clim_cat, aes(x = date, y = Temp, y2 = thresh)) +
  geom_flame(n = 3, n_gap = 0, fill = "#ffc866") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1, color = "gray80", linetype = 'solid') +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1, color = "red", linetype = 'longdash') +
  geom_line(aes(y = Temp, col = "Temperature"), size = 1, color = "black", linetype = 'solid') +
  geom_point(aes(y = Temp, col = "Temperature"), size = 3, color = "black") +
  scale_fill_manual(name = NULL, values = fillColCat, guide = 'none') +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(15,31,2),
                     limits = c(15,31)) +
  labs(y = expression(atop("Atmosphere",
                           paste("Temperature ( ", degree, "C)"))),
       x = NULL) +
  annotate("text", x = as.Date('2015-06-01'), y = 31, label = "(a", size = 6, hjust = 0) +
  annotate("segment", color = "gray80", size = 1,
           x = as.Date("2015-06-18", "%Y-%m-%d"),
           xend = as.Date("2015-06-22", "%Y-%m-%d"),
           y = 20, yend = 20) +
  annotate("text", color = "black", label = "Climatological Mean", size = 5,
           x = as.Date("2015-06-23", "%Y-%m-%d"),
           y = 20,
           hjust = 0) +
  annotate("segment", color = "red", size = 1, linetype = 'longdash',
           x = as.Date("2015-06-18", "%Y-%m-%d"),
           xend = as.Date("2015-06-22", "%Y-%m-%d"),
           y = 18.5, yend = 18.5) +
  annotate("text", color = "black", label = "90th Percentile Threshold", size = 5,
           x = as.Date("2015-06-23", "%Y-%m-%d"),
           y = 18.5,
           hjust = 0) +
  annotate("segment", color = "black", size = 1,
           x = as.Date("2015-06-18", "%Y-%m-%d"),
           xend = as.Date("2015-06-22", "%Y-%m-%d"),
           y = 17, yend = 17) +
  annotate("point", color = 'black', size = 3,
           x = as.Date("2015-06-20", "%Y-%m-%d"),
           y = 17) +
  annotate("text", color = "black", label = "Daily Mean Temperature", size = 5,
           x = as.Date("2015-06-23", "%Y-%m-%d"),
           y = 17,
           hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

fig8_water <- ggplot(data = water_clim_cat, aes(x = date, y = Temp, y2 = thresh)) +
  geom_flame(n = 5, n_gap = 2, fill = "#ffc866") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1, color = "gray80", linetype = 'solid') +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1, color = "red", linetype = 'longdash') +
  geom_line(aes(y = Temp, col = "Temperature"), size = 1, color = "black", linetype = 'solid') +
  geom_point(aes(y = Temp, col = "Temperature"), size = 3, color = "black") +
  scale_fill_manual(name = NULL, values = fillColCat, guide = 'none') +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(17,31,2),
                     limits = c(17,31)) +
  labs(y = expression(atop("Wachapreague",
                           paste("Water Temperature ( ", degree, "C)"))),
       x = NULL) +
  annotate("text", x = as.Date('2015-06-01'), y = 31, label = "(b", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

fig8_north <- ggplot(data = northern_clim_cat, aes(x = date, y = Temp, y2 = thresh)) +
  geom_flame(n = 5, n_gap = 2, fill = "#ffc866") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1, color = "gray80", linetype = 'solid') +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1, color = "red", linetype = 'longdash') +
  geom_line(aes(y = Temp, col = "Temperature"), size = 1, color = "black", linetype = 'solid') +
  geom_point(aes(y = Temp, col = "Temperature"), size = 3, color = "black") +
  scale_fill_manual(name = NULL, values = fillColCat, guide = 'none') +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(17,31,2),
                     limits = c(17,31)) +
  labs(y = expression(atop("Edge",
                           paste("Sediment Temperature (  ", degree, "C)"))),
       x = NULL) +
  annotate("text", x = as.Date('2015-06-01'), y = 31, label = "(c", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

fig8_central <- ggplot(data = central_clim_cat, aes(x = date, y = Temp, y2 = thresh)) +
  geom_flame(n = 5, n_gap = 2, fill = "#ffc866") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1, color = "gray80", linetype = 'solid') +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1, color = "red", linetype = 'longdash') +
  geom_line(aes(y = Temp, col = "Temperature"), size = 1, color = "black", linetype = 'solid') +
  geom_point(aes(y = Temp, col = "Temperature"), size = 3, color = "black") +
  scale_fill_manual(name = NULL, values = fillColCat, guide = 'none') +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(17,31,2),
                     limits = c(17,31)) +
  labs(y = expression(atop("Central",
                           paste("Sediment Temperature (  ", degree, "C)"))),
       x = "2015") +
  annotate("text", x = as.Date('2015-06-01'), y = 31, label = "(d", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

# width = 600 height = 1300
fig8_air + fig8_water + fig8_north + fig8_central + plot_layout(ncol = 1)

# heatwave trend analyses

# hw_air <- saveDat_Temp_air
# hw_water <- saveDat_Temp[saveDat_Temp$Station == 'water',]
# hw_central <- saveDat_Temp[saveDat_Temp$Station == 'central',]
# hw_northern <- saveDat_Temp[saveDat_Temp$Station == 'northern',]

saveDat_Temp <- saveDat_Temp %>%
  mutate(year = year(date_peak),
         month = month(date_peak),
         season = ifelse(month >=12, "Winter",
                         ifelse(month >= 9, "Fall",
                                ifelse(month >= 6, "Summer",
                                       ifelse(month >= 3, "Spring",
                                              ifelse(month >=1, "Winter", NA))))))

hw_time <- saveDat_Temp %>%
  group_by(Station, year) %>%
  summarise(Avg.Duration = round(mean(duration, na.rm = T)),
            Avg.CuInt = round(mean(intensity_cumulative_relThresh, na.rm = T),1),
            frequency = length(duration))

hw_season <- saveDat_Temp %>%
  group_by(Station,season,year) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Frequency = length(duration))

fill <- data.frame(matrix(ncol = 2, nrow = 29*4)) # 29 years * 4 sites
colnames(fill)[1:2] <- c("Station","year")
fill$year <- rep(seq(1994,2022,1),4)
fill$Station <- rep(c("air","central","northern","water"), each = 29)

hw_time <- merge(hw_time,fill, by = c('Station',"year"), all = TRUE)
hw_time[is.na(hw_time)] <- 0
hw_time_air <- hw_time[hw_time$Station == 'air',]
hw_time_else <- hw_time[!hw_time$Station == 'air',]
hw_time_else[hw_time_else$year %in% c(2006,2007,2017),3:5] <- NA
hw_time <- rbind(hw_time_air,hw_time_else)
kable_hw_time <- hw_time %>%
  group_by(Station) %>%
  summarise(meanFrequency = round(mean(frequency, na.rm = T)),
            sdFrequency = round(sd(frequency, na.rm = T)))
hw_time <- na.omit(hw_time)

fill2 <- data.frame(matrix(ncol = 3, nrow = 4*29*4)) # 4 locations * 29 years * 4 seasons
colnames(fill2)[1:3] <- c("Station","season","year")
fill2$year <- rep(seq(1994,2022,1),4*4) # 4 locations * 4 seasons
fill2$season <- rep(c("Winter","Spring","Summer","Fall"), each = 4*29) # 4 seasons * 29 years
fill2$Station <- rep(c("air","central","northern","water"), times = 116)
hw_season <- merge(hw_season, fill2, by = c("year","season","Station"), all = TRUE)
hw_season[is.na(hw_season)] <- 0
hw_season_air <- hw_season[hw_season$Station == 'air',]
hw_season_else <- hw_season[!hw_season$Station == 'air',]
hw_season_else[hw_season_else$year %in% c(2006,2007,2017),3:5] <- NA
hw_season <- rbind(hw_season_air,hw_season_else)
hw_season <- na.omit(hw_season)
hw_season_air <- hw_season %>%
  filter(Station == 'air') %>%
  select(-Station)
hw_season_water <- hw_season %>%
  filter(Station == 'water') %>%
  select(-Station)
hw_season_northern <- hw_season %>%
  filter(Station == 'northern') %>%
  select(-Station)
hw_season_central <- hw_season %>%
  filter(Station == 'central') %>%
  select(-Station)

aa <- unique(hw_time$Station)
bb <- unique(hw_season$season)

library(Kendall)
library(trend)

# Mann Kendall and sen's slope heatwave trend analysis ----
# Looking for temporal trends in HW avg. duration, avg. cuInt, and frequency
# in water temperature, central, and northern sediment temperature
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Station == aa[i],3]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Station = aa[i]
  cur_hw_time = data.frame(Station = Station,
                           Variable = "Avg.Duration",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_duration = cur_hw_time
  } else{
    hw_time_duration = rbind(hw_time_duration, cur_hw_time)
  }
}
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Station == aa[i],4]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Station = aa[i]
  cur_hw_time = data.frame(Station = Station,
                           Variable = "Avg.CuInt",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_intensity = cur_hw_time
  } else{
    hw_time_intensity = rbind(hw_time_intensity, cur_hw_time)
  }
}
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Station == aa[i],5]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Station = aa[i]
  cur_hw_time = data.frame(Station = Station,
                           Variable = "Frequency",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_freq = cur_hw_time
  } else{
    hw_time_freq = rbind(hw_time_freq, cur_hw_time)
  }
}

# Mann Kendall and sen's slope heatwave trend analysis
# Looking for temporal trends among seasons in HW avg. duration, avg. cuInt, and frequency
# in air and water temperature and central and northern sediment temperature
for(i in 1:length(bb)){
  curDat = hw_season_air[hw_season_air$season == bb[i],3]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_air = data.frame(TestType = Season,
                                   Variable = "Avg.Duration",
                                   Category = Season,
                                   slope = round(slope, 3),
                                   p.val = round(p.val, 4))
  if( i == 1){
    hw_season_air_duration = cur_hw_season_air
  } else{
    hw_season_air_duration = rbind(hw_season_air_duration, cur_hw_season_air)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_air[hw_season_air$season == bb[i],4]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_air = data.frame(TestType = Season,
                                   Variable = "Avg.CuInt",
                                   Category = Season,
                                   slope = round(slope, 3),
                                   p.val = round(p.val, 4))
  if( i == 1){
    hw_season_air_intensity = cur_hw_season_air
  } else{
    hw_season_air_intensity = rbind(hw_season_air_intensity, cur_hw_season_air)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_air[hw_season_air$season == bb[i],5]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_air = data.frame(TestType = Season,
                                   Variable = "Frequency",
                                   Category = Season,
                                   slope = round(slope, 3),
                                   p.val = round(p.val, 4))
  if( i == 1){
    hw_season_air_freq = cur_hw_season_air
  } else{
    hw_season_air_freq = rbind(hw_season_air_freq, cur_hw_season_air)
  }
}

for(i in 1:length(bb)){
  curDat = hw_season_water[hw_season_water$season == bb[i],3]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_water = data.frame(TestType = Season,
                                   Variable = "Avg.Duration",
                                   Category = Season,
                                   slope = round(slope, 3),
                                   p.val = round(p.val, 4))
  if( i == 1){
    hw_season_water_duration = cur_hw_season_water
  } else{
    hw_season_water_duration = rbind(hw_season_water_duration, cur_hw_season_water)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_water[hw_season_water$season == bb[i],4]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_water = data.frame(TestType = Season,
                                   Variable = "Avg.CuInt",
                                   Category = Season,
                                   slope = round(slope, 3),
                                   p.val = round(p.val, 4))
  if( i == 1){
    hw_season_water_intensity = cur_hw_season_water
  } else{
    hw_season_water_intensity = rbind(hw_season_water_intensity, cur_hw_season_water)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_water[hw_season_water$season == bb[i],5]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_water = data.frame(TestType = Season,
                                   Variable = "Frequency",
                                   Category = Season,
                                   slope = round(slope, 3),
                                   p.val = round(p.val, 4))
  if( i == 1){
    hw_season_water_freq = cur_hw_season_water
  } else{
    hw_season_water_freq = rbind(hw_season_water_freq, cur_hw_season_water)
  }
}

for(i in 1:length(bb)){
  curDat = hw_season_northern[hw_season_northern$season == bb[i],3]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_northern = data.frame(TestType = Season,
                                      Variable = "Avg.Duration",
                                      Category = Season,
                                      slope = round(slope, 3),
                                      p.val = round(p.val, 4))
  if( i == 1){
    hw_season_northern_duration = cur_hw_season_northern
  } else{
    hw_season_northern_duration = rbind(hw_season_northern_duration, cur_hw_season_northern)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_northern[hw_season_northern$season == bb[i],4]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_northern = data.frame(TestType = Season,
                                      Variable = "Avg.CuInt",
                                      Category = Season,
                                      slope = round(slope, 3),
                                      p.val = round(p.val, 4))
  if( i == 1){
    hw_season_northern_intensity = cur_hw_season_northern
  } else{
    hw_season_northern_intensity = rbind(hw_season_northern_intensity, cur_hw_season_northern)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_northern[hw_season_northern$season == bb[i],5]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_northern = data.frame(TestType = Season,
                                      Variable = "Frequency",
                                      Category = Season,
                                      slope = round(slope, 3),
                                      p.val = round(p.val, 4))
  if( i == 1){
    hw_season_northern_freq = cur_hw_season_northern
  } else{
    hw_season_northern_freq = rbind(hw_season_northern_freq, cur_hw_season_northern)
  }
}

for(i in 1:length(bb)){
  curDat = hw_season_central[hw_season_central$season == bb[i],3]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_central = data.frame(TestType = Season,
                                     Variable = "Avg.Duration",
                                     Category = Season,
                                     slope = round(slope, 3),
                                     p.val = round(p.val, 4))
  if( i == 1){
    hw_season_central_duration = cur_hw_season_central
  } else{
    hw_season_central_duration = rbind(hw_season_central_duration, cur_hw_season_central)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_central[hw_season_central$season == bb[i],4]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_central = data.frame(TestType = Season,
                                     Variable = "Avg.CuInt",
                                     Category = Season,
                                     slope = round(slope, 3),
                                     p.val = round(p.val, 4))
  if( i == 1){
    hw_season_central_intensity = cur_hw_season_central
  } else{
    hw_season_central_intensity = rbind(hw_season_central_intensity, cur_hw_season_central)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season_central[hw_season_central$season == bb[i],5]
  ManKen = MannKendall(curDat)
  ss = sens.slope(curDat)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season_central = data.frame(TestType = Season,
                                     Variable = "Frequency",
                                     Category = Season,
                                     slope = round(slope, 3),
                                     p.val = round(p.val, 4))
  if( i == 1){
    hw_season_central_freq = cur_hw_season_central
  } else{
    hw_season_central_freq = rbind(hw_season_central_freq, cur_hw_season_central)
  }
}

hw_time_output <- rbind(hw_time_duration, hw_time_intensity, hw_time_freq)
hw_time_output <- hw_time_output %>% arrange(-desc(p.val))

fdr_table <- data.frame(matrix(ncol = 2, nrow = 12))
x <- c("Rank", "FDR_0.1")
colnames(fdr_table) <- x
fdr_table[1:12,1] <- seq(1,12,1)
fdr_table[1:12,2] <- (fdr_table$Rank[1:12]/12)*.1

hw_time_output <- cbind(hw_time_output,fdr_table)
hw_time_output$SigTest <- ifelse(hw_time_output$p.val < hw_time_output$FDR_0.1,
                                 "Sig", "NS")
View(hw_time_output)

hw_season_air_output <- rbind(hw_season_air_duration, hw_season_air_intensity, hw_season_air_freq)
hw_season_air_output <- hw_season_air_output %>% arrange(-desc(p.val))
hw_season_water_output <- rbind(hw_season_water_duration, hw_season_water_intensity, hw_season_water_freq)
hw_season_water_output <- hw_season_water_output %>% arrange(-desc(p.val))
hw_season_northern_output <- rbind(hw_season_northern_duration, hw_season_northern_intensity, hw_season_northern_freq)
hw_season_northern_output <- hw_season_northern_output %>% arrange(-desc(p.val))
hw_season_central_output <- rbind(hw_season_central_duration, hw_season_central_intensity, hw_season_central_freq)
hw_season_central_output <- hw_season_central_output %>% arrange(-desc(p.val))

View(hw_season_air_output)
View(hw_season_water_output)
View(hw_season_northern_output)
View(hw_season_central_output)

# hw characteristic table

library(kableExtra)

test <- saveDat_Temp %>%
  group_by(Station) %>%
  summarise(TotalEvent = NROW(duration),
            MeanDuration = round(mean(duration)),
            MaxDuration = max(duration),
            MeanInt_relThres = round(mean(intensity_mean_relThresh),digits = 1),
            MaxInt_relThres = round(max(intensity_max_relThresh),digits = 1))
test$Station <- as.factor(test$Station)
test <- test[order(test$Station, decreasing = T),]

test %>%
  kbl(escape =  F, align=rep('c', 5)) %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'center') %>%
  kable_classic(html_font = "Cambria")

# Table 1: Heatwave characteristics ----
table1 <- data.frame(matrix(ncol = 5, nrow = 6))
colnames(table1)[1:5] <- c("Variable","Atmosphere","Water","Edge","Central")
table1$Variable <- c("Total Events",
                 "Frequency (events year<sup> -1</sup>)",
                 "Mean Duration (days)",
                 "Max Duration (days)",
                 "Mean Intensity - Rel. Thres. (<sup> o</sup>C)",
                 "Max Intensity - Rel. Thres. (<sup> o</sup>C)")
table1$Atmosphere <- c(test[4,2],
              paste(kable_hw_time[1,2], kable_hw_time[1,3], sep = " ± "),
              test[4,3],
              test[4,4],
              test[4,5],
              test[4,6])
table1$Water <- c(test[1,2],
              paste(kable_hw_time[4,2], kable_hw_time[4,3], sep = " ± "),
              test[1,3],
              test[1,4],
              test[1,5],
              test[1,6])
table1$Edge <- c(test[2,2],
                 paste(kable_hw_time[3,2], kable_hw_time[3,3], sep = " ± "),
                 test[2,3],
                 test[2,4],
                 test[2,5],
                 test[2,6])
table1$Central <- c(test[3,2],
                paste(kable_hw_time[2,2], kable_hw_time[2,3], sep = " ± "),
                test[3,3],
                test[3,4],
                test[3,5],
                test[3,6])

setwd("D:/School/SeagrassRecovery/Data/Sediment")

table1 %>%
  kbl(escape =  F, align = 'rcccc') %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'left') %>%
  kable_classic(html_font = "Cambria") %>%
  column_spec(1:5, background = 'white') %>%
  save_kable(file = 'Table1.html')

# Gap filling observed water and sediment temperature for Wavelet Analysis ----

dat_wide <- dat_hourly[,c(1,24:27)]

water_level_temp <- water_level_temp %>%
  filter(date >= as.Date('2020-06-20') &
           date <= as.Date('2022-10-10')) %>%
  select(!c(date,Year,Time))

# statistical model building - used for gap filling missing data
# Multiple linear regression to predict long-term sediment temperature
set.seed(3456)

dat_wide <- merge(dat_wide,water_level_temp, by = 'DateTime')

# break down observed dataset into training (75%) and validation (25%) datasets
test_dat <- dat_wide
sample <- sample.int(n = nrow(test_dat), size = floor(0.75*nrow(test_dat)), replace = F)
Train <- test_dat[sample,]
Valid <- test_dat[-sample,]

# build model using training datasets
train_central_sed_lm <- lm(central_sedtemp~watertemp_c+wl_diff+DoY+hour, data = Train)
train_central_water_lm <- lm(central_watertemp~watertemp_c+wl_diff+DoY+hour, data = Train)
train_northern_sed_lm <- lm(northern_sedtemp~watertemp_c+wl_diff+DoY+hour, data = Train)
train_northern_water_lm <- lm(northern_watertemp~watertemp_c+wl_diff+DoY+hour, data = Train)
summary(train_central_sed_lm)
summary(train_central_water_lm)
summary(train_northern_sed_lm)
summary(train_northern_water_lm)

# test model fits using validation datasets
pred_Valid_central_sed <- predict(train_central_sed_lm, newdata = Valid)
pred_Valid_central_water <- predict(train_central_water_lm, newdata = Valid)
pred_Valid_northern_sed <- predict(train_northern_sed_lm, newdata = Valid)
pred_Valid_northern_water <- predict(train_northern_water_lm, newdata = Valid)
Valid$pred_central_sed <- pred_Valid_central_sed
Valid$pred_central_water <- pred_Valid_central_water
Valid$pred_northern_sed <- pred_Valid_northern_sed
Valid$pred_northern_water <- pred_Valid_northern_water
Valid_lm_central_sed <- lm(pred_central_sed~central_sedtemp, data = Valid)
Valid_lm_central_water <- lm(pred_central_water~central_watertemp, data = Valid)
Valid_lm_northern_sed <- lm(pred_northern_sed~northern_sedtemp, data = Valid)
Valid_lm_northern_water <- lm(pred_northern_water~northern_watertemp, data = Valid)
round(summary(Valid_lm_central_sed)$adj.r.square,2) # adj. R2 = 0.99
round(summary(Valid_lm_central_water)$adj.r.square,2) # adj. R2 = 0.99
round(summary(Valid_lm_northern_sed)$adj.r.square,2) # adj. R2 = 0.98
round(summary(Valid_lm_northern_water)$adj.r.square,2) # adj. R2 = 0.98

# visually inspect fits
Valid %>%
  ggplot(aes(x = central_sedtemp, y = pred_central_sed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'longdash')
Valid %>%
  ggplot(aes(x = central_watertemp, y = pred_central_water)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'longdash')
Valid %>%
  ggplot(aes(x = northern_sedtemp, y = pred_northern_sed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'longdash')
Valid %>%
  ggplot(aes(x = northern_watertemp, y = pred_northern_water)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'longdash')

# count missing data
sum(is.na(dat_wide$central_sedtemp)) # 386
sum(is.na(dat_wide$central_watertemp)) # 80
sum(is.na(dat_wide$northern_sedtemp)) # 792
sum(is.na(dat_wide$northern_watertemp)) # 143

# interpolate over missing data for gaps <= 5
dat_wide <- dat_wide %>%
  mutate(central_sedtemp = zoo::na.approx(central_sedtemp, maxgap = 5, na.rm = F),
         central_watertemp = zoo::na.approx(central_watertemp, maxgap = 5, na.rm = F),
         northern_sedtemp = zoo::na.approx(northern_sedtemp, maxgap = 5, na.rm = F),
         northern_watertemp = zoo::na.approx(northern_watertemp, maxgap = 5, na.rm = F))

# count missing data
sum(is.na(dat_wide$central_sedtemp)) # 335
sum(is.na(dat_wide$central_watertemp)) # 29
sum(is.na(dat_wide$northern_sedtemp)) # 739
sum(is.na(dat_wide$northern_watertemp)) # 89

# interpolate missing data based on statistical model predictions
dat_wide <- dat_wide %>%
  mutate(pred_central_sed = predict(train_central_sed_lm, newdata = dat_wide),
         pred_central_water = predict(train_central_water_lm, newdata = dat_wide),
         pred_northern_sed = predict(train_northern_sed_lm, newdata = dat_wide),
         pred_northern_water = predict(train_northern_water_lm, newdata = dat_wide),
         central_sedtemp = ifelse(is.na(central_sedtemp),pred_central_sed,central_sedtemp),
         central_watertemp = ifelse(is.na(central_watertemp),pred_central_water,central_watertemp),
         northern_sedtemp = ifelse(is.na(northern_sedtemp),pred_northern_sed,northern_sedtemp),
         northern_watertemp = ifelse(is.na(northern_watertemp),pred_northern_water,northern_watertemp))

# count missing data
sum(is.na(dat_wide$central_sedtemp)) # 2
sum(is.na(dat_wide$central_watertemp)) # 1
sum(is.na(dat_wide$northern_sedtemp)) # 0
sum(is.na(dat_wide$northern_watertemp)) # 0

# which rows have missing data (answer: last two rows)
which(is.na(dat_wide$central_sedtemp))
which(is.na(dat_wide$central_watertemp))

# remove last two rows so time series no longer have gaps
dat_wide <- dat_wide %>%
  filter(!row_number() %in% c(20223:20224))

# convert wide data frame to long dataframe
central <- dat_wide[,c(1,2,4)]
northern <- dat_wide [,c(1,3,5)]
central <- central %>%
  mutate(Location = "Central") %>%
  rename("Sediment" = "central_sedtemp",
         "Water" = "central_watertemp") %>%
  tidyr::pivot_longer(cols = c('Sediment','Water'),
                      names_to = 'Variable',
                      values_to = 'Temperature')
northern <- northern %>%
  mutate(Location = "Northern") %>%
  rename("Sediment" = "northern_sedtemp",
         "Water" = "northern_watertemp") %>%
  tidyr::pivot_longer(cols = c('Sediment','Water'),
                      names_to = 'Variable',
                      values_to = 'Temperature')
dat_long <- rbind(central,northern)

# visualize the time series
dat_long %>%
  ggplot(aes(x = DateTime, y = Temperature)) +
  geom_line() +
  theme_bw() +
  theme(text = element_text(size = 16, color = "black")) +
  facet_grid(Location~Variable)

water <- dat_long %>%
  filter(Variable == 'Water',
         DateTime > as.POSIXct("2022-07-19 00:00") &
           DateTime < as.POSIXct("2022-08-07 23:45"))

lims <- as.POSIXct(strptime(c("2022-07-19 00:00","2022-08-07 23:45"), format = "%Y-%m-%d %H:%M"))

water %>%
  ggplot(aes(x = DateTime, y = Temperature, group = Location, color = Location)) +
  geom_line() +
  labs(x = '2022',
       y = expression(Water~Temperature~(degree*C))) +
  scale_color_manual(values = c('red','black')) +
  scale_y_continuous(breaks = seq(21,33,2)) +
  scale_x_datetime(date_labels = "%b %d",
                   date_breaks = "1 day",
                   limits = lims,
                   expand = expansion(add = c(0,0))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position = c(0.1,0.15),
        legend.text = element_text(size = 16, color = 'black'))

# Figure 3: Wavelet analysis ----

library(biwavelet)

central_all <- as.data.frame(dat_wide[,c(1,2,4)])
northern_all <- as.data.frame(dat_wide[,c(1,3,5)])

central_water_all <- as.data.frame(central_all$central_watertemp)
colnames(central_water_all)[1] <- 'watertemp'
northern_water_all <- as.data.frame(northern_all$northern_watertemp)
colnames(northern_water_all)[1] <- 'watertemp'
central_sed_all <- as.data.frame(central_all$central_sedtemp)
colnames(central_sed_all)[1] <- 'sedtemp'
northern_sed_all <- as.data.frame(northern_all$northern_sedtemp)
colnames(northern_sed_all)[1] <- 'sedtemp'

time <- as.numeric(seq(1,NROW(dat_wide),1))

wt_central_water_dat <- cbind(time,central_water_all)
wt_northern_water_dat <- cbind(time,northern_water_all)
wt_central_sed_dat <- cbind(time,central_sed_all)
wt_northern_sed_dat <- cbind(time,northern_sed_all)

xx <- format(seq(as.Date('2020-07-01'),
                 as.Date('2022-10-01'),
                 by = '3 months'), '%b')

par(mfrow = c(2,2), xpd = F)
par(mar = c(1,1,1.1,1.1) + 0.1,
    oma = c(3,3,0.5,0.5) + 0.1)

# top-left plot
plot(wt(wt_northern_water_dat),
     plot.cb = F, plot.phase = F,
     plot.sig = T, lwd.sig = 1,
     alpha.coi = 1,
     col.coi = 'black',
     xaxt = 'n',
     xlab = "",
     ylab = "",
     cex.axis = 1.5,
     cex.lab = 1.5)
axis(1, at = seq(265, 19993, by = 24*(365/12)), cex.axis = 1.5, col.axis = 'NA', tck = -0.02)
axis(1, at = seq(265, 19993, by = 24*(365/12)*3), cex.axis = 1.5, labels = xx, col.axis = 'white', tck = -0.04)
mtext("Water", side = 3, cex = 1.5, line = 0.1)

# top-right plot
plot(wt(wt_northern_sed_dat),
     plot.cb = F, plot.phase = F,
     plot.sig = T, lwd.sig = 1,
     alpha.coi = 1,
     col.coi = 'black',
     xaxt = 'n',
     col.axis = 'white',
     xlab = '',
     ylab = '')
axis(1, at = seq(265, 19993, by = 24*(365/12)), cex.axis = 1.5, col.axis = 'NA', tck = -0.02)
axis(1, at = seq(265, 19993, by = 24*(365/12)*3), cex.axis = 1.5, labels = xx, col.axis = 'white', tck = -0.04)
mtext("Sediment", side = 3, cex = 1.5, line = 0.1)
text("Edge", x = 20800, y = 7, srt = 270, cex = 1.5, xpd = NA)

# bottom-left plot
plot(wt(wt_central_water_dat),
     plot.cb = F, plot.phase = F,
     plot.sig = T, lwd.sig = 1,
     alpha.coi = 1,
     col.coi = 'black',
     xaxt = 'n',
     ylab = '',
     xlab = '',
     cex.axis = 1.5)
axis(1, at = seq(265, 19993, by = 24*(365/12)), cex.axis = 1.5, col.axis = 'NA', tck = -0.02)
axis(1, at = seq(265, 19993, by = 24*(365/12)*3), cex.axis = 1.5, labels = xx, tck = -0.04)
mtext(c("2020","2021","2022"), side = 1, cex = 1.5, line = 2.6, at = c(24*(365/12)*3, 24*(365/12)*11.5, 24*(365/12)*22.5))

# bottom-right plot
plot(wt(wt_central_sed_dat),
     plot.cb = F, plot.phase = F,
     plot.sig = T, lwd.sig = 1,
     alpha.coi = 1,
     col.coi = 'black',
     col.axis = 'white',
     xaxt = 'n',
     xlab = '',
     ylab = '')
axis(1, at = seq(265, 19993, by = 24*(365/12)), cex.axis = 1.5, col.axis = 'NA', tck = -0.02)
axis(1, at = seq(265, 19993, by = 24*(365/12)*3), cex.axis = 1.5, labels = xx, tck = -0.04)
mtext(c("2020","2021","2022"), side = 1, cex = 1.5, line = 2.5, at = c(24*(365/12)*3, 24*(365/12)*11.5, 24*(365/12)*22.5))
text("Central", x = 20800, y = 7.5, srt = 270, cex = 1.5, xpd = NA)

# width = 1400 height = 900
title(xlab = "",
      ylab = "Period (hours)",
      outer = TRUE, line = 1.8,
      cex.lab = 1.5)

# Coherence analysis ----

library(wsyn)

dat1 <- matrix(nrow = NROW(wt_northern_water_dat), ncol = 2)
dat1[,1] <- seq(1:NROW(wt_northern_water_dat))
dat1[,2] <- wt_northern_water_dat$watertemp

dat2 <- matrix(nrow = NROW(wt_northern_sed_dat), ncol = 2)
dat2[,1] <- seq(1:NROW(wt_northern_sed_dat))
dat2[,2] <- wt_northern_sed_dat$sedtemp

dat3 <- matrix(nrow = NROW(wt_central_water_dat), ncol = 2)
dat3[,1] <- seq(1:NROW(wt_central_water_dat))
dat3[,2] <- wt_central_water_dat$watertemp

dat4 <- matrix(nrow = NROW(wt_central_sed_dat), ncol = 2)
dat4[,1] <- seq(1:NROW(wt_central_sed_dat))
dat4[,2] <- wt_central_sed_dat$sedtemp

times <- 1:NROW(dat1)
dat1 <- dat1[,2]
dat2 <- dat2[,2]
dat3 <- dat3[,2]
dat4 <- dat4[,2]
dat1 <- cleandat(dat1, times = times, clev = 3)$cdat
dat2 <- cleandat(dat2, times = times, clev = 3)$cdat
dat3 <- cleandat(dat3, times = times, clev = 3)$cdat
dat4 <- cleandat(dat4, times = times, clev = 3)$cdat

dev.off()
par(mfrow = c(2,3), xpd = F)
par(mar = c(1,1,1.1,1.1) + 0.1,
    oma = c(3,3,0.5,0.5) + 0.1)

plotmag(coh(dat1, dat2,
            scale.min = 10,
            scale.max.input = 14,
            times = 1:20222,
            norm = 'powall',
            sigmethod = 'fast'))
mtext("Focal Band 10-14 hours", side = 3, line = 0.1)

plotmag(coh(dat1, dat2,
            scale.min = 20,
            scale.max.input = 28,
            times = 1:20222,
            norm = 'powall',
            sigmethod = 'fast'))
mtext("Focal Band 20-28 hours", side = 3, line = 0.1)

plotmag(coh(dat1, dat2,
            scale.min = 72,
            scale.max.input = 504,
            times = 1:20222,
            norm = 'powall',
            sigmethod = 'fast'))
mtext("Focal Band 72-504 hours", side = 3, line = 0.1)
text("Northern Edge", x = 72, y = 0.99, srt = 270, xpd=NA)

plotmag(coh(dat3, dat4,
            scale.min = 10,
            scale.max.input = 14,
            times = 1:20222,
            norm = 'powall',
            sigmethod = 'fast'))

plotmag(coh(dat3, dat4,
            scale.min = 20,
            scale.max.input = 28,
            times = 1:20222,
            norm = 'powall',
            sigmethod = 'fast'))

plotmag(coh(dat3, dat4,
            scale.min = 72,
            scale.max.input = 504,
            times = 1:20222,
            norm = 'powall',
            sigmethod = 'fast'))

title(xlab = "Timescales",
      ylab = "Coherence",
      outer = TRUE, line = 2,
      cex.lab = 1.5)

# Coherence significance testing ----
northern_coh <- coh(dat1, dat2,
                    times = 1:20222,
                    norm = 'powall',
                    sigmethod = 'fast')

central_coh <- coh(dat3, dat4,
                   times = 1:20222,
                   norm = 'powall',
                   sigmethod = 'fast')

northern_coh_smallFocal <- bandtest(northern_coh, band = c(10,14))
northern_coh_mediumFocal <- bandtest(northern_coh, band = c(20,28))
northern_coh_largeFocal <- bandtest(northern_coh, band = c(72,504))

central_coh_smallFocal <- bandtest(central_coh, band = c(10,14))
central_coh_mediumFocal <- bandtest(central_coh, band = c(20,28))
central_coh_largeFocal <- bandtest(central_coh, band = c(72,504))

round(northern_coh_smallFocal$bandp[3],3)
round(northern_coh_mediumFocal$bandp[3],3)
round(northern_coh_largeFocal$bandp[3],3)

round(central_coh_smallFocal$bandp[3],3)
round(central_coh_mediumFocal$bandp[3],3)
round(central_coh_largeFocal$bandp[3],3)
