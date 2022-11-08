(data.table)
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
summary(central_lm)
summary(northern_lm)

# compare treatment and control sediment temperature at central
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

# compare treatment and control sediment temperature at northern
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
summary(control_lm)
summary(treatment_lm)

# compare control plots between central and northern sites
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

# compare treatment plots between central and northern sites
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
summary(spatial_lm)

# compare central and northern sediment temperature
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

# Daily mean sediment temperature from central and northern locations
# width = 1000 height = 700
ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = MeanCentral_sedtemp), color = "red", size = 1) +
  geom_line(aes(x = Date, y = MeanNorth_sedtemp), size = 1, alpha = 0.7) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  scale_y_continuous(breaks = seq(0,35,5)) +
  labs(x = NULL,
       y = expression(paste("Daily Mean Sediment Temp. (  ", degree, "C)"))) +
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

# Daily mean water and sediment temperature from central location
# width = 1000 height = 700
ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = MeanCentral_sedtemp), color = "red", size = 1) +
  geom_line(aes(x = Date, y = MeanCentral_watertemp), size = 1, alpha = 0.7) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  scale_y_continuous(breaks = seq(0,35,5)) +
  labs(x = NULL,
       y = expression(paste("Central - Daily Mean Temp. (  ", degree, "C)"))) +
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
           label = 'Water',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = 33.1,
           label = "Sediment",
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

# Daily mean water and sediment temperature from northern location
# width = 1000 height = 700
ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = MeanNorth_sedtemp), color = "red", size = 1) +
  geom_line(aes(x = Date, y = MeanNorth_watertemp), size = 1, alpha = 0.7) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  scale_y_continuous(breaks = seq(0,35,5)) +
  labs(x = NULL,
       y = expression(paste("Northern - Daily Mean Temp. (  ", degree, "C)"))) +
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
           label = 'Water',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = 33.1,
           label = "Sediment",
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

# Difference in daily sediment temperature between central and northern locations
# width = 1000 height = 700
ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = DeltaT_sedtemp)) +
  geom_point(aes(x = Date, y = DeltaT_sedtemp), shape = 21, fill = "white", size = 1.5) +
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
           xmax = as.Date("2021-01-25", "%Y-%m-%d"),
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

# Difference in daily water temperature between central and northern locations
# width = 1000 height = 700
ggplot(data = dat_daily) +
  geom_line(aes(x = Date, y = DeltaT_watertemp)) +
  geom_point(aes(x = Date, y = DeltaT_watertemp), shape = 21, fill = "white", size = 1.5) +
  geom_abline(slope = 0, intercept = 0, linetype = "longdash", color = "black") +
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
           xmax = as.Date("2021-01-25", "%Y-%m-%d"),
           ymin = 3.45, ymax = 4.1) +
  annotate("text", x = as.Date("2020-07-01", "%Y-%m-%d"), y = 4,
           label = 'Pos. = Central > North',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-07-01", "%Y-%m-%d"), y = 3.6,
           label = "Neg. = North > Central",
           size = 5, fontface = 1, hjust = 0) + 
  labs(x = NULL,
       y = expression(paste(Delta~"Water Temp ( ", degree, "C)"))) +
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

# sed_dat <- dat_hourly[,c(23,21,24,25)]
# x <- c("date","hour","sedtemp_central","sedtemp_northern")
# colnames(sed_dat) <- x

round(sum(is.na(dat_hourly$central_sedtemp))/NROW(dat_hourly),2)*100 # 2%
round(sum(is.na(dat_hourly$northern_sedtemp))/NROW(dat_hourly),2)*100 # 4%

### 1989-2022 Wachapreague water level wrangling (https://tidesandcurrents.noaa.gov/waterlevels.html?id=8631044)

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

ggplot(data = water_level_temp, aes(x = DateTime, y = wl_diff)) +
  geom_line()

colnames(dat_daily)[1] <- "date"

dat_daily %>%
  ggplot() +
  geom_line(aes(x = date, y = DeltaT_sedwater_central), color = "red", size = 1) +
  geom_line(aes(x = date, y = DeltaT_sedwater_north), color = "black", size = 1, alpha = 0.7) +
  geom_abline(slope = 0, intercept = 0, linetype = "longdash") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  scale_y_continuous(breaks = seq(-1.5,1.5,0.5)) +
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
           ymin = 1.4, ymax = 1.8) +
  annotate("rect", fill = "white", color = "black",
           xmin = as.Date("2020-05-25", "%Y-%m-%d"),
           xmax = as.Date("2020-12-31", "%Y-%m-%d"),
           ymin = -1.8, ymax = -1.4) +
  annotate("segment", color = "black", size = 1,
           x = as.Date("2020-06-25", "%Y-%m-%d"),
           xend = as.Date("2020-07-15", "%Y-%m-%d"),
           y = 1.5, yend = 1.5) +
  annotate("segment", color = "red", size = 1,
           x = as.Date("2020-06-25", "%Y-%m-%d"),
           xend = as.Date("2020-07-15", "%Y-%m-%d"),
           y = 1.7, yend = 1.7) +
  annotate("text", x = as.Date("2020-07-20", "%Y-%m-%d"), y = 1.5,
           label = 'Northern',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-07-20", "%Y-%m-%d"), y = 1.7,
           label = "Central",
           size = 5, fontface = 1, hjust = 0) + 
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = -1.5,
           label = 'Pos. = Wtemp > Stemp',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-06-25", "%Y-%m-%d"), y = -1.7,
           label = "Neg. = Stemp > Wtemp",
           size = 5, fontface = 1, hjust = 0) +
  labs(x = NULL,
       y = expression(paste("Daily Mean"~Delta~"Water-Sediment Temp ( ", degree, "C)"))) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(-1.75, 1.75)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = -2.3, label = 2020, size = 6) +
  annotate(geom = "text", x = as.Date("2021-06-15", "%Y-%m-%d"), y = -2.3, label = 2021, size = 6) +
  annotate(geom = "text", x = as.Date("2022-06-15", "%Y-%m-%d"), y = -2.3, label = 2022, size = 6)

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
summary(train_central_lm)
summary(train_northern_lm)

# test model fits using validation datasets
pred_Valid_central <- predict(train_central_lm, newdata = Valid)
pred_Valid_northern <- predict(train_northern_lm, newdata = Valid)
Valid$pred_central <- pred_Valid_central
Valid$pred_northern <- pred_Valid_northern
Valid_lm_central <- lm(pred_central~central_sedtemp, data = Valid)
Valid_lm_northern <- lm(pred_northern~northern_sedtemp, data = Valid)
summary(Valid_lm_central) # adj. R2 = 0.98
summary(Valid_lm_northern) # adj. R2 = 0.97

central_plot <-
  ggplot(data = Valid, aes(x = central_sedtemp, y = pred_central)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red") +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  xlab(expression(paste("Observed Sediment Temperature (", degree,"C)"))) +
  ylab(expression(paste("Predicted Sediment Temperature (  ", degree,"C)"))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 33, size = 5) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 31, size = 5) +
  annotate("text", x = 1, y = 35,
           label = 'Central',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

northern_plot <-
  ggplot(data = Valid, aes(x = northern_sedtemp, y = pred_northern)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = "red") +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  xlab(expression(paste("Observed Sediment Temperature (", degree,"C)"))) +
  ylab("") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x = 1, label.y = 33, size = 5) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 31, size = 5) +
  annotate("text", x = 1, y = 35,
           label = 'Northern',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

# width = 600 height = 1000
central_plot + northern_plot + plot_layout(ncol = 2)

longterm_dat <- water_level_temp[44977:nrow(water_level_temp),]
longterm_dat$doy <- yday(longterm_dat$date)
longterm_pred_central <- predict(train_central_lm, newdata = longterm_dat)
longterm_pred_northern <- predict(train_northern_lm, newdata = longterm_dat)
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
       y = expression(paste("Central - Predicted Sediment Temp. (  ", degree, "C)"))) +
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
       y = expression(paste("Northern - Predicted Sediment Temp. (  ", degree, "C)"))) +
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

# width = 1400 height = 600
central_timeseries + northern_timeseries + plot_layout(ncol = 2)

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

all_daily_data <- rbind(central_longterm_daily,
                        northern_longterm_daily,
                        water_longterm_daily)
all_daily_data$Temp[is.nan(all_daily_data$Temp)] <- NA
missing <- all_daily_data %>%
  mutate(Year = year(date)) %>%
  group_by(Year,Station) %>%
  summarise(DaysPerYear = sum(!is.na(Temp)))
# View(missing)

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

# water_frequency <- water_fill %>%
#   group_by(Year) %>%
#   summarise(TotalHWevents = sum(n))
# water_frequency[c(13:14,24),2] <- NA
# water_frequency$Year <- as.numeric(water_frequency$Year)
# water_freq_lm <- lm(TotalHWevents~Year, data = water_frequency)
# summary(water_freq_lm)
# round((0.1109*1994)-220.111) # 1 event
# round((0.1109*2022)-220.111) # 4 events

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
  labs(y = expression(atop("Water Column",
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

# northern_frequency <- northern_fill %>%
#   group_by(Year) %>%
#   summarise(TotalHWevents = sum(n))
# northern_frequency[c(13:14,24),2] <- NA
# northern_frequency$Year <- as.numeric(northern_frequency$Year)
# northern_freq_lm <- lm(TotalHWevents~Year, data = northern_frequency)
# summary(northern_freq_lm)
# round((0.11313*1994)-224.67859) # 1 event
# round((0.11313*2022)-224.67859) # 4 events

figB <- ggplot(data = northern_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  annotate("text", x = 1994, y = 8, label = "b)", size = 6) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  labs(y = expression(atop("Northern",
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
               arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

# central_frequency <- central_fill %>%
#   group_by(Year) %>%
#   summarise(TotalHWevents = sum(n))
# central_frequency[c(13:14,24),2] <- NA
# central_frequency$Year <- as.numeric(central_frequency$Year)
# central_freq_lm <- lm(TotalHWevents~Year, data = central_frequency)
# summary(central_freq_lm)
# round((0.11119*1994)-220.74448) # 1 event
# round((0.11119*2022)-220.74448) # 4 events

figC <- ggplot(data = central_fill) +
  geom_col(aes(x = as.numeric(Year), y = n,
               fill = forcats::fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  # scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) + # For gray-scale
  annotate("text", x = 1994, y = 8, label = "c)", size = 6) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  labs(y = expression(atop("Central",
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
               arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

# test <- lm(TotalDuration~Year, data = water_sum)
# summary(test) 
# round((0.6713 * 1994) - 1327.7352) # 11 days
# round((0.6713 * 2022) - 1327.7352) # 30 days

figD <- ggplot(data = water_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(d", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
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
           label = 'y = 0.67x - 1328',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

# test <- lm(TotalDuration~Year, data = northern_sum)
# summary(test)
# round((0.6674 * 1994) - 1319.6003) # 11 days
# round((0.6674 * 2022) - 1319.6003) # 30 days

figE <- ggplot(data = northern_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(e", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  labs(y = expression(atop("Northern",
                           paste("Total HW Days"))),
       x = NULL) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 5) +
  annotate("text", x = 1995, y = 53,
           label = 'y = 0.66x - 1319',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

# test <- lm(TotalDuration~Year, data = central_sum)
# summary(test)
# round((0.627 * 1994) - 1238.3508) # 12 days
# round((0.627 * 2022) - 1238.3508) # 29 days

figF <- ggplot(data = central_sum, aes(x = Year, y = TotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  annotate("text", x = 2017, y = 60, label = "(f", size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1994, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, 65, 10),
                     limits = c(0,65)) +
  labs(y = expression(atop("Central",
                           paste("Total HW Days"))),
       x = NULL) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1995, label.y = 60, size = 5) +
  annotate("text", x = 1995, y = 53,
           label = 'y = 0.62x - 1238',
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

# width = 1200 height = 1100
figA + figD + figB + figE + figC + figF + plot_layout(ncol = 2, nrow = 3)

# Co-occurring heatwaves in June 2015

ts_water = ts2clm(water_longterm_daily, x = date, y = Temp,
                  climatologyPeriod = c(min(water_longterm_daily$date), max(water_longterm_daily$date)))
de_water_temp = detect_event(ts_water, x = date, y = Temp )

ts_northern = ts2clm(northern_longterm_daily, x = date, y = Temp,
                     climatologyPeriod = c(min(northern_longterm_daily$date), max(northern_longterm_daily$date)))
de_northern_temp = detect_event(ts_northern, x = date, y = Temp )

ts_central = ts2clm(central_longterm_daily, x = date, y = Temp,
                    climatologyPeriod = c(min(central_longterm_daily$date), max(central_longterm_daily$date)))
de_central_temp = detect_event(ts_central, x = date, y = Temp )

water_clim_cat <- de_water_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7774:7817)

northern_clim_cat <- de_northern_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7774:7817)

central_clim_cat <- de_central_temp$climatology %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(7774:7817)


# Set category fill colours
fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)

water_fig <- ggplot(data = water_clim_cat, aes(x = date, y = Temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
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
  annotate("segment", color = "gray80", size = 1,
           x = as.Date("2015-06-18", "%Y-%m-%d"),
           xend = as.Date("2015-06-22", "%Y-%m-%d"),
           y = 22, yend = 22) +
  annotate("text", color = "black", label = "Climatological Mean", size = 5,
           x = as.Date("2015-06-23", "%Y-%m-%d"),
           y = 22,
           hjust = 0) +
  annotate("segment", color = "red", size = 1, linetype = 'longdash',
           x = as.Date("2015-06-18", "%Y-%m-%d"),
           xend = as.Date("2015-06-22", "%Y-%m-%d"),
           y = 20.5, yend = 20.5) +
  annotate("text", color = "black", label = "90th Percentile Threshold", size = 5,
           x = as.Date("2015-06-23", "%Y-%m-%d"),
           y = 20.5,
           hjust = 0) +
  annotate("segment", color = "black", size = 1,
           x = as.Date("2015-06-18", "%Y-%m-%d"),
           xend = as.Date("2015-06-22", "%Y-%m-%d"),
           y = 19, yend = 19) +
  annotate("point", color = 'black', size = 3,
           x = as.Date("2015-06-20", "%Y-%m-%d"),
           y = 19) +
  annotate("text", color = "black", label = "Daily Mean Temperature", size = 5,
           x = as.Date("2015-06-23", "%Y-%m-%d"),
           y = 19,
           hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

north_sed_fig <- ggplot(data = northern_clim_cat, aes(x = date, y = Temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_line(aes(y = seas, col = "Climatology"), size = 1, color = "gray80", linetype = 'solid') +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1, color = "red", linetype = 'longdash') +
  geom_line(aes(y = Temp, col = "Temperature"), size = 1, color = "black", linetype = 'solid') +
  geom_point(aes(y = Temp, col = "Temperature"), size = 3, color = "black") +
  scale_fill_manual(name = NULL, values = fillColCat, guide = 'none') +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(17,31,2),
                     limits = c(17,31)) +
  labs(y = expression(atop("Northern",
                           paste("Sediment Temperature (  ", degree, "C)"))),
       x = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

central_sed_fig <- ggplot(data = central_clim_cat, aes(x = date, y = Temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
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
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

# width = 600 height = 1000
water_fig + north_sed_fig + central_sed_fig + plot_layout(ncol = 1)

# heatwave trend analyses

hw_water <- saveDat_Temp[saveDat_Temp$Station == 'water',]
hw_central <- saveDat_Temp[saveDat_Temp$Station == 'central',]
hw_northern <- saveDat_Temp[saveDat_Temp$Station == 'northern',]

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

fill <- data.frame(matrix(ncol = 2, nrow = 87))
colnames(fill)[1:2] <- c("Station","year")
fill$year <- rep(seq(1994,2022,1),3)
fill$Station <- rep(c("central","northern","water"), each = 29)

hw_time <- merge(hw_time,fill, by = c('Station',"year"), all = TRUE)
hw_time[is.na(hw_time)] <- 0
hw_time[hw_time$year %in% c(2006,2007,2017),3:5] <- NA
hw_time <- hw_time[!(hw_time$year == 2022),] # 2022 does not have a complete year of data
kable_hw_time <- hw_time %>%
  group_by(Station) %>%
  summarise(meanDuration = round(mean(Avg.Duration, na.rm = T),),
            meanCuInt = round(mean(Avg.CuInt, na.rm = T),1),
            meanFrequency = round(mean(frequency, na.rm = T),),
            sdFrequency = round(sd(frequency, na.rm = T)))
hw_time <- na.omit(hw_time)

fill2 <- data.frame(matrix(ncol = 3, nrow = 348))
colnames(fill2)[1:3] <- c("Station","season","year")
fill2$year <- rep(seq(1994,2022,1),12)
fill2$season <- rep(c("Winter","Spring","Summer","Fall"), each = 87)
fill2$Station <- rep(c("central","northern","water"), times = 116)
hw_season <- merge(hw_season, fill2, by = c("year","season","Station"), all = TRUE)
hw_season[is.na(hw_season)] <- 0
hw_season[hw_season$year %in% c(2006,2007,2017),3:5] <- NA
hw_season <- hw_season[!(hw_season$year == 2022),] # 2022 does not have a complete year of data
hw_season <- na.omit(hw_season)

aa <- unique(hw_time$Station)
bb <- unique(hw_season$season)

library(Kendall)
library(trend)

for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Station == aa[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$year),
          end = max(curDat$year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
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
  curDat = hw_time[hw_time$Station == aa[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$year),
          end = max(curDat$year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
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
  curDat = hw_time[hw_time$Station == aa[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$year),
          end = max(curDat$year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
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

for(i in 1:length(bb)){
  curDat = hw_season[hw_season$season == bb[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$year),
          end = max(curDat$year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season = data.frame(TestType = Season,
                             Variable = "Avg.Duration",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_duration = cur_hw_season
  } else{
    hw_season_duration = rbind(hw_season_duration, cur_hw_season)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season[hw_season$season == bb[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$year),
          end = max(curDat$year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season = data.frame(TestType = Season,
                             Variable = "Avg.CuInt",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_intensity = cur_hw_season
  } else{
    hw_season_intensity = rbind(hw_season_intensity, cur_hw_season)
  }
}
for(i in 1:length(bb)){
  curDat = hw_season[hw_season$season == bb[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$year),
          end = max(curDat$year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = bb[i]
  cur_hw_season = data.frame(TestType = Season,
                             Variable = "Frequency",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_freq = cur_hw_season
  } else{
    hw_season_freq = rbind(hw_season_freq, cur_hw_season)
  }
}

hw_time_output <- rbind(hw_time_duration, hw_time_intensity, hw_time_freq)
hw_time_output <- hw_time_output %>% arrange(-desc(p.val))

hw_season_output <- rbind(hw_season_duration, hw_season_intensity, hw_season_freq)
hw_season_output <- hw_season_output %>% arrange(-desc(p.val))

# hw characteristic table

library(kableExtra)

test <- saveDat_Temp %>%
  group_by(Station) %>%
  summarise(TotalEvent = NROW(duration),
            MeanDuration = round(mean(duration)),
            MaxDuration = max(duration),
            MeanInt_relThres = round(mean(intensity_max_relThresh),digits = 1),
            MaxInt_relThres = round(max(intensity_max_relThresh),digits = 1))
test$Station <- as.factor(test$Station)
test <- test[order(test$Station, decreasing = T),]

test %>%
  kbl(escape =  F, align=rep('c', 5)) %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'center') %>%
  kable_classic(html_font = "Cambria")


df <- data.frame(matrix(ncol = 4, nrow = 6))
colnames(df)[1:4] <- c("Variable","Water","Northern","Central")
df$Variable <- c("Total Events",
                 "Frequency (events year<sup> -1</sup>)",
                 "Mean Duration (days)",
                 "Max Duration (days)",
                 "Mean Intensity - Rel. Thres. (<sup> o</sup>C)",
                 "Max Intensity - Rel. Thres. (<sup> o</sup>C)")
df$Water <- c(test[1,2],
              paste(kable_hw_time[3,4], kable_hw_time[3,5], sep = " ± "),
              test[1,3],
              test[1,4],
              test[1,5],
              test[1,6])
df$Northern <- c(test[2,2],
                 paste(kable_hw_time[2,4], kable_hw_time[2,5], sep = " ± "),
                 test[2,3],
                 test[2,4],
                 test[2,5],
                 test[2,6])
df$Central <- c(test[3,2],
                paste(kable_hw_time[1,4], kable_hw_time[1,5], sep = " ± "),
                test[3,3],
                test[3,4],
                test[3,5],
                test[3,6])

setwd("D:/School/SeagrassRecovery/Data/Sediment")
df %>%
  kbl(escape =  F, align = 'rccc') %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'left') %>%
  kable_classic(html_font = "Cambria") %>%
  save_kable(file = 'hw_table.html', self_contained = T)
