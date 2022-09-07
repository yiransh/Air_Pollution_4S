
# 01 STATIC DESCRIPTION ==================================================
# ____________________________________________________________________________

## TABLE ---------------------------------------------------------------------
str(env[env$year != "2016", ])
summary(env[env$year != "2016", ])
sapply(env[env$year != "2016", ][, c(3:10)], sd, na.rm = T)

## BOXPLOT ---------------------------------------------------------------------
### PM2.5 -----
ggplot(env, aes(factor(""), y = pm25)) +
  geom_boxplot(color =brewer.pal(9,"Blues")[c(5)], 
               fill = brewer.pal(9,"Blues")[c(3)],
               outlier.color = brewer.pal(9,"Blues")[c(9)], 
               outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, 
               color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 35, linetype = "dashed" ,
             color = "dodgerblue3") +
  geom_hline(yintercept = 75, linetype = "dashed", 
             color = "cyan3") +
  theme_classic() +
  ylim(0, 80) +
  labs(
    x = "PM2.5",
    y = "Concentration (µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))


### PM10 -----
ggplot(env, aes(factor(""), y = pm10)) +
  geom_boxplot(color =brewer.pal(9,"Blues")[c(5)], 
               fill = brewer.pal(9,"Blues")[c(3)],
               outlier.color = brewer.pal(9,"Blues")[c(9)], 
               outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, 
               color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 50, linetype = "dashed" ,
             color = "dodgerblue3") +
  geom_hline(yintercept = 150, linetype = "dashed", 
             color = "cyan3") +
  theme_classic() +
  ylim(0, 180) +
  labs(
    x = "PM10",
    y = "Concentration (µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))


### CO -----
ggplot(env, aes(factor(""), y = co)) +
  geom_boxplot(color =brewer.pal(9,"Blues")[c(5)], 
               fill = brewer.pal(9,"Blues")[c(3)],
               outlier.color = brewer.pal(9,"Blues")[c(9)], 
               outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, 
               color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 4, linetype = "dashed" ,
             color = "dodgerblue3") +
  theme_classic() +
  labs(
    x = "CO",
    y = "Concentration (mg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))


### NO2 -----
ggplot(env, aes(factor(""), y = no2)) +
  geom_boxplot(color =brewer.pal(9,"Blues")[c(5)], 
               fill = brewer.pal(9,"Blues")[c(3)],
               outlier.color = brewer.pal(9,"Blues")[c(9)], 
               outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, 
               color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 80, linetype = "dashed" ,
             color = "dodgerblue3") +
  theme_classic() +
  labs(
    x = "NO2",
    y = "Concentration (µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))



### SO2 -----
ggplot(env, aes(factor(""), y = so2)) +
  geom_boxplot(color =brewer.pal(9,"Blues")[c(5)], 
               fill = brewer.pal(9,"Blues")[c(3)],
               outlier.color = brewer.pal(9,"Blues")[c(9)], 
               outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, 
               color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 50, linetype = "dashed" ,
             color = "dodgerblue3") +
  ylim(0, 52) +
  theme_classic() +
  labs(
    x = "SO2",
    y = "Concentration (µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))



### O3 -----
ggplot(env, aes(factor(""), y = o3)) +
  geom_boxplot(color =brewer.pal(9,"Blues")[c(5)], 
               fill = brewer.pal(9,"Blues")[c(3)],
               outlier.color = brewer.pal(9,"Blues")[c(9)], 
               outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, 
               color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 100, linetype = "dashed" ,
             color = "dodgerblue3") +
  geom_hline(yintercept = 160, linetype = "dashed", 
             color = "cyan3") +
  theme_classic() +
  ylim(0, 200) +
  labs(
    x = "O3",
    y = "Concentration (µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))




# 02 DYNAMIC DESCRIPTION =================================================
# ____________________________________________________________________________

## PM2.5 ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = pm25)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 35, color = "#3182bd") +
  geom_hline(yintercept = 75, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of PM2.5 Over Years", 
       x = "Date", y = "Daily mean concentration of PM2.5") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = pm25)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 35, color = "#3182bd") +
  geom_hline(yintercept = 75, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of PM2.5", 
       x = "Month", y = "Daily mean concentration of PM2.5") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

### dotplot -----
# Plotting time trend of the mean value over the entire study period (dotplot)
ggplot(data = env, aes(x = date, y = pm25)) +
  geom_point(color = "#08519c", size = 0.3) +
  geom_hline(yintercept = 35, color = "#3182bd") +
  geom_hline(yintercept = 75, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of PM2.5 Over Years", 
       x = "Date", y = "Daily mean concentration of PM2.5 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 90)))


## PM10 ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = pm10)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 50, color = "#3182bd") +
  geom_hline(yintercept = 150, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of PM10 Over Years", 
       x = "Date", y = "Daily mean concentration of PM10 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = pm10)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 50, color = "#3182bd") +
  geom_hline(yintercept = 150, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of PM10", 
       x = "Month", y = "Daily mean concentration of PM10 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

## CO ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = co)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of CO Over Years", 
       x = "Date", y = "Daily mean concentration of CO (mg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))
## CO standards < 4mg/m3: ALL Observations meet standards

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = co)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of CO", 
       x = "Month", y = "Daily mean concentration of CO (mg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

## NO2 ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = no2)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 80, color = "#3182bd") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of NO2 Over Years", 
       x = "Date", y = "Daily mean concentration of NO2 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = no2)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 80, color = "#3182bd") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of NO2", 
       x = "Month", y = "Daily mean concentration of NO2 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

## SO2 ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = so2)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 50, color = "#3182bd") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of SO2 Over Years", 
       x = "Date", y = "Daily mean concentration of SO2 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))
## SO standards < 150µg/m3 for Type II AREA: ALL Observations meet standards

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = so2)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of SO2", 
       x = "Month", y = "Daily mean concentration of SO2 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

## O3 ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = o3)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 100, color = "#3182bd") +
  geom_hline(yintercept = 160, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of O3 Over Years", 
       x = "Date", y = "Daily mean concentration of O3 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = o3)) +
  geom_line(color = "#08519c", size = 0.6) +
  geom_hline(yintercept = 100, color = "#3182bd") +
  geom_hline(yintercept = 160, color = "#6baed6") +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of O3", 
       x = "Month", y = "Daily mean concentration of O3 (µg/m3)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

## TEMPERATURE ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = temp)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of Temperature Over Years", 
       x = "Date", y = "daily mean temperature (ºC)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = temp)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of Temperature", 
       x = "Month", y = "daily mean temperature (ºC)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")

## RELATIVE HUMIDITY ---------------------------------------------------------------------
# Plotting time trend of the mean value over the entire study period
ggplot(data = env, aes(x = date, y = rh)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Time Trend of Relative Humidty Over Years", 
       x = "Date", y = "daily mean relative humidty (%)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150)))

# Plotting seasonal trend of the mean value
ggplot(data = env, aes(x = CommonDate, y = rh)) +
  geom_line(color = "#08519c", size = 0.6) +
  stat_smooth(
    color = "#bdd7e7", fill = "#eff3ff",
    method = "loess"
  ) +
  theme_classic() +
  labs(title = "Seasonal Trend of Relative Humidity", 
       x = "Month", y = "daily mean relative humidity (%)") + 
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)),
        panel.background = element_rect(fill = rgb(247,247,247, 
                                                   max = 255, alpha = 150))) +
  scale_x_date(date_labels = "%B")



# 03 DESCRIPTION BY SITE =================================================
# ____________________________________________________________________________

## STATIC BOXPLOT ---------------------------------------------------------
### PM2.5 -----
# x = forcats::fct_reorder(factor(sites), pm25, .fun = 'mean') doesn't work
# summary data
df <- ddply(PM25[!(PM25$sites %in% c("mean", "SD")), ], .(sites, urban), summarize, mean = mean(pm25, na.rm=T), sd = sd(pm25, na.rm=T))
# manually order
pm25order <- df[order(df$urban, df$mean), ][, 1]

# boxplot
ggplot(PM25[!(PM25$sites %in% c("mean", "SD")), ], 
       aes(x = sites, y = pm25, fill=urban)) +
  geom_boxplot(width = 0.5, color =brewer.pal(9,"Blues")[c(3)], 
               outlier.color = brewer.pal(9,"Blues")[c(3)], outlier.size = 0.5) +
  scale_x_discrete(limits=pm25order) +
  stat_summary(fun="mean", geom="point", size=1, color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 35, linetype = "dashed" ,color = "dodgerblue3") +
  geom_hline(yintercept = mean(PM25[PM25$sites == "mean", "pm25"], na.rm=T), 
             color = "cyan3") +
  theme_classic() +
  ylim(0, 75) +
  labs(
    title = "Daily Mean PM2.5 of Shanghai",
    x = "Daily Mean",
    y = "PM2.5 Concentra.(µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))

## test normality among sites
tapply(PM25$pm25, PM25$sites, shapiro.test)                      # all normal
kruskal.test(pm25 ~ sites, data = PM25[!(PM25$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 198.5, df = 9, p-value < 2.2e-16
# significant difference


## test normality among urban
# >5000, assume normality
t.test(pm25 ~ urban, data = PM25[!(PM25$sites %in% c("mean", "SD")), ])
# t = -0.24492, df = 19784, p-value = 0.8065
# mean in group suburban    mean in group urban 
#                36.18877               36.27326 
# non-sig difference
ggplot(PM25[!(PM25$sites %in% c("mean", "SD")), ], 
       aes(x = urban, y = pm25, fill=urban)) +
  geom_boxplot() +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("urban", "suburban"))

## test normality among urban3
# >5000, assume normality
kruskal.test(pm25 ~ urban3, data = PM25[!(PM25$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 101.75, df = 2, p-value < 2.2e-16
# 1  control 40.49787
# 2 suburban 35.14009
# 3    urban 36.27326
# sig difference: control > urban > suburban 
ggplot(PM25[!(PM25$sites %in% c("mean", "SD")), ], 
       aes(x = urban3, y = pm25, fill=urban)) +
  geom_boxplot() +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("control", "urban", "suburban"))

### PM10 -----
# summary data
df <- ddply(PM10[!(PM10$sites %in% c("mean", "SD")), ], .(sites, urban), 
            summarize, 
            mean = mean(pm10, na.rm=T), 
            sd = sd(pm10, na.rm=T))
# manually order
pm10order <- df[order(df$urban, df$mean), ][, 1]

# boxplot
ggplot(PM10[!(PM10$sites %in% c("mean", "SD")), ], 
       aes(x = sites, y = pm10, fill=urban)) +
  geom_boxplot(width = 0.5, color =brewer.pal(9,"Blues")[c(3)], 
               outlier.color = brewer.pal(9,"Blues")[c(3)], outlier.size = 0.5) +
  stat_summary(fun="mean", geom="point", size=1, color=(brewer.pal(9,"Blues")[c(8)])) + 
  geom_hline(yintercept = 50, linetype = "dashed" ,color = "dodgerblue3") +
  geom_hline(yintercept = mean(PM10[PM10$sites == "mean", "pm10"], na.rm=T), 
             color = "cyan3") +
  scale_x_discrete(limits=pm10order) +
  theme_classic() +
  ylim(0, 150) +
  labs(
    title = "Daily Mean PM10 of Shanghai",
    x = "Daily Mean",
    y = "PM10 Concentra.(µg/m3)"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 1,                     # Vertical adjustment
                                  lineheight = 1,                # Line spacing
                                  margin = margin(10, 0, 0, 0)))

## test normality among sites
tapply(PM10$pm10, PM10$sites, shapiro.test)                      # all normal
kruskal.test(pm10 ~ sites, data = PM10[!(PM10$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 168.75, df = 9, p-value < 2.2e-16
# significant difference

## test normality among urban
# >5000, assume normality
t.test(pm10 ~ urban, data = PM10[!(PM10$sites %in% c("mean", "SD")), ])
# t = -4.328, df = 20209, p-value = 1.512e-05
# significant difference: urban > suburban
# mean in group suburban    mean in group urban 
#               50.67381               52.52110 
ggplot(PM10[!(PM10$sites %in% c("mean", "SD")), ], 
       aes(x = urban, y = pm10, fill=urban)) +
  geom_boxplot() +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("urban", "suburban"))

## test normality among urban3
# >5000, assume normality
kruskal.test(pm10 ~ urban3, data = PM10[!(PM10$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 62.427, df = 2, p-value = 2.78e-14
# 1  control 53.46779
# 2 suburban 50.00243
# 3    urban 52.52110
# sig difference: control > urban > suburban
ggplot(PM10[!(PM10$sites %in% c("mean", "SD")), ], 
       aes(x = urban3, y = pm10, fill=urban)) +
  geom_boxplot() +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("control", "urban", "suburban"))

### CO ----------------------------------------------------------------
## test normality among sites
tapply(CO$co, CO$sites, shapiro.test)                      # all normal
kruskal.test(co ~ sites, data = CO[!(CO$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 307.08, df = 9, p-value < 2.2e-16
# significant difference

## test normality among urban
# >5000, assume normality
t.test(co ~ urban, data = CO[!(CO$sites %in% c("mean", "SD")), ])
# t = -4.8232, df = 19922, p-value = 1.423e-06
# significant difference: urban > suburban
# mean in group suburban    mean in group urban 
#               0.684762               0.702819  

## test normality among urban3
# >5000, assume normality
kruskal.test(co ~ urban3, data = CO[!(CO$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 50.057, df = 2, p-value = 1.349e-11
ddply(CO[!(CO$sites %in% c("mean", "SD")), ], .(urban3), 
      summarize, 
      mean = mean(co, na.rm=T))
# 1  control 0.7111811
# 2 suburban 0.6783079
# 3    urban 0.7028190
# sig difference: control > urban > suburban
ggplot(CO[!(CO$sites %in% c("mean", "SD")), ], 
       aes(x = urban3, y = co, fill=urban3)) +
  geom_boxplot()

### NO2 ----------------------------------------------------------------
## test normality among sites
tapply(NO2$NO2, NO2$sites, shapiro.test)                      # all normal
kruskal.test(NO2 ~ sites, data = NO2[!(NO2$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 400.61, df = 9, p-value < 2.2e-16
# significant difference

## test normality among urban
# >5000, assume normality
t.test(NO2 ~ urban, data = NO2[!(NO2$sites %in% c("mean", "SD")), ])
# t = -13.359, df = 20320, p-value < 2.2e-16
# significant difference: urban > suburban
# mean in group suburban    mean in group urban 
#               38.57364               42.15095  
ggplot(NO2[!(NO2$sites %in% c("mean", "SD")), ], 
       aes(x = urban, y = NO2, fill=urban)) +
  geom_boxplot() +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("urban", "suburban"))

## test normality among urban3
# >5000, assume normality
kruskal.test(NO2 ~ urban3, data = NO2[!(NO2$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 227.02, df = 2, p-value < 2.2e-16
ddply(NO2[!(NO2$sites %in% c("mean", "SD")), ], .(urban3), 
      summarize, 
      mean = mean(NO2, na.rm=T))
# 1  control 38.70377
# 2 suburban 38.54106
# 3    urban 42.15095
# sig difference: urban > control > suburban
ggplot(NO2[!(NO2$sites %in% c("mean", "SD")), ], 
       aes(x = urban3, y = NO2, fill=urban3)) +
  geom_boxplot() +
  ylim(0, 100) +
  theme_classic() +
  scale_fill_brewer(palette = "Blues") + 
  scale_x_discrete(limits=c("urban", "control", "suburban"))

### SO2 ----------------------------------------------------------------
## test normality among sites
tapply(SO2$SO2, SO2$sites, shapiro.test)                      # all normal
kruskal.test(SO2 ~ sites, data = SO2[!(SO2$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 456.59, df = 9, p-value < 2.2e-16
# significant difference

## test normality among urban
# >5000, assume normality
t.test(SO2 ~ urban, data = SO2[!(SO2$sites %in% c("mean", "SD")), ])
# t = 4.928, df = 19862, p-value = 8.374e-07
# significant difference: urban < suburban
# mean in group suburban    mean in group urban 
#               9.302306               8.927230  
ggplot(SO2[!(SO2$sites %in% c("mean", "SD")), ], 
       aes(x = urban, y = SO2, fill=urban)) +
  geom_boxplot() +
  ylim(0, 30) +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("urban", "suburban"))

## test normality among urban3
# >5000, assume normality
kruskal.test(SO2 ~ urban3, data = SO2[!(SO2$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 261.6, df = 2, p-value < 2.2e-16
ddply(SO2[!(SO2$sites %in% c("mean", "SD")), ], .(urban3), 
      summarize, 
      mean = mean(SO2, na.rm=T))
# 1  control 8.174611
# 2 suburban 9.577668
# 3    urban 8.927230
# sig difference: suburban > urban > control
ggplot(SO2[!(SO2$sites %in% c("mean", "SD")), ], 
       aes(x = urban3, y = SO2, fill=urban3)) +
  geom_boxplot(outlier.size = 0.5) +
  ylim(0, 40) +
  theme_classic() +
  scale_fill_brewer(palette = "Blues") + 
  scale_x_discrete(limits=c("control", "urban", "suburban"))

### O3 ----------------------------------------------------------------
## test normality among sites
tapply(O3$o3, O3$sites, shapiro.test)                      # all normal
kruskal.test(o3 ~ sites, data = O3[!(O3$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 181.11, df = 9, p-value < 2.2e-16
# significant difference

## test normality among urban
# >5000, assume normality
t.test(o3 ~ urban, data = O3[!(O3$sites %in% c("mean", "SD")), ])
# t = 7.1856, df = 20279, p-value = 6.924e-13
# significant difference: urban < suburban
# mean in group suburban    mean in group urban 
#              102.82914               98.38237  
ggplot(O3[!(O3$sites %in% c("mean", "SD")), ], 
       aes(x = urban, y = o3, fill=urban)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() +
  scale_x_discrete(limits=c("urban", "suburban"))

## test normality among urban3
# >5000, assume normality
kruskal.test(o3 ~ urban3, data = O3[!(O3$sites %in% c("mean", "SD")), ])
# Kruskal-Wallis chi-squared = 78.241, df = 2, p-value < 2.2e-16
ddply(O3[!(O3$sites %in% c("mean", "SD")), ], .(urban3), 
      summarize, 
      mean = mean(o3, na.rm=T))
# 1  control 106.43795
# 2 suburban 101.95277
# 3    urban  98.38237
# sig difference: suburban < urban < control
ggplot(O3[!(O3$sites %in% c("mean", "SD")), ], 
       aes(x = urban3, y = o3, fill=urban3)) +
  geom_boxplot(outlier.size = 0.5) +
  theme_classic() +
  scale_fill_brewer(palette = "Blues") + 
  scale_x_discrete(limits=c("control", "suburban", "urban"))


## DYNAMIC TREND ----------------------------------------------------------

# 04 CORRELATION =======================================================

library(GGally)
ggcorr(env[, c(3, 6:10)],
       method = c("pairwise.complete.obs", "spearman"),
       low = "steelblue",
       mid = "white",
       high = "darkred",
       geom = "tile", 
       label = TRUE,
       label_alpha = TRUE) 

env[which(env$year %in% c("2017", "2018", "2019")), c(4:9)] %>% 
  rename(PM2.5 = pm25, PM10 = pm10, CO = co, NO2 = no2, SO2 = so2, O3 = o3) %>%
  ggcorr(method = c("pairwise.complete.obs", "spearman"),
         low = "steelblue",
         mid = "white",
         high = "darkred",
         geom = "tile", 
         label = TRUE,
         label_size = 3,
         label_alpha = TRUE) 

env[ , c(4:9)] %>% 
  rename(PM2.5 = pm25, PM10 = pm10, CO = co, NO2 = no2, SO2 = so2, O3 = o3) %>%
  ggcorr(method = c("pairwise.complete.obs", "spearman"),
         low = "steelblue",
         mid = "white",
         high = "darkred",
         geom = "tile", 
         label = TRUE,
         label_size = 3,
         label_alpha = TRUE) 

# unused ------------------------------------------------------------
env.cor <- cor(env[, c(3, 6:10)], use = "complete.obs", method = "spearman")
write.csv(pd.cor, file = "pd cor.csv") #export your cor data
library(ggcorrplot)
ggcorrplot(env.cor,tl.cex = 23)
+theme(legend.text=element_text(size=21))
+theme(axis.text=element_text(size=28,face="bold"),axis.title=element_text(size=12,face="bold"))
+theme(legend.text=element_text(size=14,face="bold"))
+theme(legend.key.size = unit(2.3,"line"))
View(pd.cor)
