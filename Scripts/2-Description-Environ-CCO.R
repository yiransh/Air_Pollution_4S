###############################################################################
# 0 TARGET DATASET  ----------------------------------------------------------
###############################################################################

daterange_stroke <- range(date(stroke_t$onset_dt))
alldates_stroke <- as.Date(format(seq(from = daterange_stroke[1], 
                                      to = daterange_stroke[2], 'days'), 
                                  "%Y%m%d"), "%Y%m%d")
stroke_cal <- data.frame(date = alldates_stroke, case = NA, ctrl = NA)
stroke_cal$case <- as.numeric(stroke_cal$case)
stroke_cal$ctrl <- as.numeric(stroke_cal$ctrl)
stroke_cal$case[which(stroke_cal$date 
                        %in% airstr$tdate[which(airstr$status == 1)])] <- 1
table(stroke_cal$case)
## 1832/1833 dates were casedates
stroke_cal$ctrl[which(stroke_cal$date 
                      %in% airstr$tdate[which(airstr$status == 0)])] <- 1
stroke_cal$case <- as.factor(stroke_cal$case)
stroke_cal$ctrl <- as.factor(stroke_cal$ctrl)

# MERGE WITH ENV
env_cal <- merge(stroke_cal, env, by = "date", all.x = TRUE)
env_cal$precovid <- ifelse(env_cal$year %in% c("2016","2017", "2018", "2019"), 1, 0)

# TARGET DATASET: env_cal

###############################################################################
# 1 TABLEONE: ENVIRON VARIABLES  ----------------------------------------------
###############################################################################

library(tableone)
listvars_env <- c("o3", "temp", "rh", "pm25", "pm10", "co", "no2", "so2")

# TEST FOR NORMALITY
shapiro.test(env_cal$o3)    # p < 2.2e-16
shapiro.test(env_cal$temp)  # p < 2.2e-16
shapiro.test(env_cal$rh)    # p < 3.724e-13
shapiro.test(env_cal$pm25)  # p < 2.2e-16
shapiro.test(env_cal$pm10)  # p < 2.2e-16
shapiro.test(env_cal$co)    # p < 2.2e-16
shapiro.test(env_cal$no2)   # p < 2.2e-16
shapiro.test(env_cal$so2)   # p < 2.2e-16

###############################################################################
# 2 TABLEONE: EXPORT OUTPUT  -------------------------------------------------
###############################################################################

table1_env_total <- CreateTableOne(vars = listvars_env, data = env_cal)
table1_env_case <- CreateTableOne(vars = listvars_env, 
                                  data = env_cal[which(env_cal$case == 1), ])
table1_env_ctrl <- CreateTableOne(vars = listvars_env, 
                                  data = env_cal[which(env_cal$ctrl == 1), ])
write.csv(print(table1_env_total), file = "Output/Description_env_total.csv")
write.csv(print(table1_env_case), file = "Output/Description_env_case.csv")
write.csv(print(table1_env_ctrl), file = "Output/Description_env_ctrl.csv")

table1_env_covid <- CreateTableOne(vars = listvars_env, data = env_cal, strata = c('precovid'))
write.csv(print(table1_env_covid), file = "Output/Description_env_covid.csv")
