# LOAD THE PACKAGES
library(dlnm) ; library(splines) ; library(MASS) ; library(tsModel); library(survival)
library(RColorBrewer)
library(ggplot2)

################################################################################
# 00 DATA PREPRARTION ----------------------------------------------------------
################################################################################

# POPULATION
# - TOTAL (AIS + TIA)  airstr
# - AIS                airstr_ais
# - LAA                laa
# - CE                 ce
# - SVD                svd
pop <- airstr_ais
##[which(airstr$year %in% c("2017", "2018", "2019")), ]

# INTERVENTION/CONTROL
# - AIR POLLUTANTS: pm25, pm10, o3, no2, so2, co


# OUTCOME
# - STATUS

# PARAMATERS
# - POLLUTANTS: linear (lag=1/2/.../01/02/...)
# - TEMP:       ns     (lag=03, df=3)
# - RH:         ns     (lag=03, df=3)
# - HOLIDAY:    dummy/linear
# - STRATUM:    linear


################################################################################
# 01 MODEL FIT AND PREDICTION --------------------------------------------------
################################################################################

# ORIGINAL MODEL
# model <- clogit(status ~ pop[, which(names(pop)==aplag)] + ns(temp03, df=3) + ns(rh03, df=3) +
#                   strata(id) + as.factor(holiday),
#                 data = pop,
#                 na.action = na.exclude)

lageffect <- function(ap) {
  lagtable <- data.frame(NULL)
  for (i in 0:6) {
    aplag <- paste0(ap, "lag", i)
    model <- clogit(status ~ pop[, which(names(pop)==aplag)] + ns(temp03, df=3) + ns(rh03, df=3) +
                      strata(id) + as.factor(holiday),
                    data = pop,
                    na.action = na.exclude)
    lag_temp <- data.frame(aplag,
                           (summary(model)$conf.int[1, 1])^10,
                           (summary(model)$conf.int[1, 3])^10,
                           (summary(model)$conf.int[1, 4])^10,
                  summary(model)$coeff[1, 5])
    names(lag_temp) <- c("aplag", "beta", "lb", "ub", "pval")
    lagtable <- rbind(lagtable, lag_temp)
  }
  for (i in 1:6) {
    aplag <- paste0(ap, "lag0", i)
    model <- clogit(status ~ pop[, which(names(pop)==aplag)] + ns(temp03, df=3) + ns(rh03, df=3) +
                      strata(id) + as.factor(holiday),
                    data = pop,
                    na.action = na.exclude)
    lag_temp <- data.frame(aplag,
                           (summary(model)$conf.int[1, 1])^10,
                           (summary(model)$conf.int[1, 3])^10,
                           (summary(model)$conf.int[1, 4])^10,
                           summary(model)$coeff[1, 5])
    names(lag_temp) <- c("aplag", "beta", "lb", "ub", "pval")
    lagtable <- rbind(lagtable, lag_temp)
  }
  lagtable
}

################################################################################
# 02 PLOTS AND OUTPUT ----------------------------------------------------------
################################################################################
library(stringr)

lageffect_pm25 <- lageffect("pm25")
lageffect_pm25$lagday <- gsub("pm25lag", "", lageffect_pm25$aplag)
lageffect_pm25 <- lageffect_pm25[, c(1, 6, 2:5)]
lageffect_pm25$group <- nchar(lageffect_pm25$lagday)
# lageffect_pm25$group <- ifelse(grepl("^0",lageffect_pm25$lagday), 2, 1) 

ggplot(lageffect_pm25, aes(x = reorder(lagday, group), beta, col = factor(group))) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub, width = .3)) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  scale_colour_brewer(palette = "Set1") +
  theme(plot.margin = margin(0.8, 0.8, 1, 1, "cm")) +
  labs(title = "Lag Response Curve for PM2.5", 
       x = "lag days", y = "Odds Ratio (per 10 µg/m3 increase)") + 
  theme(plot.title = element_text(
                                  size = 15,                     # Font size
                                  hjust = 0.5,                   # Horizontal adjustment
                                  vjust = 2,                     # Vertical adjustment
                                  lineheight = 10,                # Line spacing
                                  margin = margin(10, 0, 10, 0)),
        legend.position="none")

## FACET_WRAP PLOT OF LAG-EFFECT
lageffect_total <- NULL
for (i in c("pm25", "pm10", "o3", "no2", "so2","co")) {
  lageffect_temp <- lageffect(i)
  lageffect_temp$ap <- toupper(i)
  lageffect_temp$lagday <- gsub(paste0(i,"lag"), "", lageffect_temp$aplag)
  lageffect_temp <- lageffect_temp[, c(6:7, 1:5)]
  lageffect_total <- rbind(lageffect_total, lageffect_temp)
}
lageffect_total$ap[which(lageffect_total$ap == "PM25")] <- "PM2.5"

# lageffect_pm25$lagday <- gsub("pm25lag", "", lageffect_pm25$aplag)
# lageffect_pm25 <- lageffect_pm25[, c(1, 6, 2:5)]
lageffect_total$group <- nchar(lageffect_total$lagday)
lageffect_total$ap <- factor(lageffect_total$ap, 
                             levels = c("PM2.5", "PM10", "O3", "NO2", "SO2","CO"))

lagtable <- ggplot(lageffect_total, aes(x = reorder(lagday, group), beta, col = factor(group))) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub, width = .3)) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  scale_colour_brewer(palette = "Set1") +
  theme(plot.margin = margin(0.8, 0.8, 1, 1, "cm"))

lagtable + facet_wrap(~ap, ncol = 3, scales = "free_y") +
  xlab("Lag Days") + ylab("Odds Ratio") +
  theme(legend.position="none")
