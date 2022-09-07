###############################################################################
# 0 PREPARING DATA: STROKE AND ENV  ------------------------------------------
###############################################################################

# STROKE =====
stroke_t$inhospital_stroke_cr <- as.factor(stroke_t$inhospital_stroke_cr)
stroke_t$date <- date(stroke_t$onset_dt) # set date of onset as the case date
stroke_t <- subset(stroke_t, select = -c(birth_dt, age_ori, age_comp,
                          residence, date_onset, arrival_dt, discharge_dt,
                          hospt_name, bnihss, IVT, EVT, m6, disch_mrs,
                          inhospital_stroke, temp))

# DAILY POLLUTION =====
holiday_date <- c( ymd("2016-01-01"), 
                   ymd("2016-2-7"):ymd("2016-2-13"), 
                   ymd("2016-4-4"),
                   ymd("2016-5-1"),
                   ymd("2016-6-9"):ymd("2016-6-11"), 
                   ymd("2016-9-15"):ymd("2016-9-17"),
                   ymd("2016-10-1"):ymd("2016-10-7"),
                   ymd("2017-01-01"), 
                   ymd("2017-1-27"):ymd("2017-2-2"), 
                   ymd("2017-4-2"):ymd("2017-4-4"), 
                   ymd("2017-5-1"),
                   ymd("2017-5-28"), ymd("2017-5-30"),
                   ymd("2017-10-1"):ymd("2017-10-7"),
                  ymd("2018-01-01"), 
                  ymd("2018-2-15"):ymd("2018-2-21"), 
                  ymd("2018-4-5"):ymd("2018-4-7"), 
                  ymd("2018-4-29"):ymd("2018-5-1"),
                  ymd("2018-6-18"), ymd("2018-9-24"),
                  ymd("2018-10-1"):ymd("2018-10-7"),
                  ymd("2018-12-30"):ymd("2019-1-1"),
                  ymd("2019-2-4"):ymd("2019-2-10"),
                  ymd("2019-4-5"), 
                  ymd("2019-5-1"), ymd("2019-6-7"), 
                  ymd("2019-9-13"), ymd("2019-10-1"):ymd("2019-10-7"), 
                  ymd("2020-1-1"), ymd("2020-1-24"):ymd("2020-1-30"),
                  ymd("2020-4-4"):ymd("2020-4-6"),
                  ymd("2020-5-1"):ymd("2020-5-5"),
                  ymd("2020-6-25"):ymd("2020-6-27"),
                  ymd("2020-10-1"):ymd("2020-10-8"),
                  ymd("2021-1-1"):ymd("2021-1-3"), 
                  ymd("2021-2-11"):ymd("2021-2-17"),
                  ymd("2021-4-3"):ymd("2021-4-5"),
                  ymd("2021-5-1"):ymd("2021-5-5"),
                  ymd("2021-6-12"):ymd("2020-6-14"),
                  ymd("2021-9-19"):ymd("2021-9-21"),
                  ymd("2021-10-1"):ymd("2021-10-7")
                  )
env$holiday <- 0
env$holiday[env$date %in% holiday_date] <- 1

## CREATING LAG FUNCTION
lagn <- function(ap_name, n){
  for (i in 0:n) {
    varname <- paste0(ap_name,"lag", i)
    env$varname <- NA
    names(env)[length(names(env))] <- varname
    for (j in (i+1):nrow(env)) {
      if (!is.na(env[which(env$date == (env$date[j] - i)), ap_name])) {
        env[j, varname] <- env[which(env$date == (env$date[j] - i)), ap_name]
      }
    }
  }
  env
}
env <- lagn("pm25", 6)


env <- arrange(env, date)
lag0n <- function(ap_name, n){
  for (i in 1:n) {
    varname <- paste0(ap_name, "lag0", i)
    env$varname <- NA
    names(env)[length(names(env))] <- varname
    env[, varname] <- stats::filter(env[, ap_name], rep(1/(i+1), i+1), sides = 1)
  }
  env
}
env <- lag0n("pm25", 6)


env <- arrange(env, date)
mav <- function(ap_name, n){
  varname <- paste0(ap_name, "0", n)
  env$varname <- NA
  names(env)[length(names(env))] <- varname
  env[, varname] <- stats::filter(env[, ap_name], rep(1/(n+1), n+1), sides = 1)
  env
}
env <- mav("temp", 3)
env <- mav("rh", 3)

env <- lagn("pm10", 6)
env <- lag0n("pm10", 6)
env <- lagn("co", 6)
env <- lag0n("co", 6)
env <- lagn("no2", 6)
env <- lag0n("no2", 6)
env <- lagn("so2", 6)
env <- lag0n("so2", 6)
env <- lagn("o3", 6)
env <- lag0n("o3", 6)

###############################################################################
# 2 STROKE: CREATING CASE-CONRTOL DATES ------------------------------------------
###############################################################################

## SETTING ONSET_DATE(date) AS THE CASE DATE
## CREATING A FUNCTION AND ADD 4 COLUMNS FOR CONTROL DATES
ctrldate <- function(dataset, casedate_name) {
  n <- nrow(dataset)
  dataset[, c("ctrl1", "ctrl2", "ctrl3", "ctrl4")] <- NA
  for (i in 1:n){
    casedate <- as_date(as.numeric(dataset[i, casedate_name]))
    casey <- year(casedate)
    casem <- month(casedate)
    casedow <- wday(casedate) 
    casem_i <-as.Date(paste(casey,casem,"1", sep="-"))
    casem_i_dow <- wday(casem_i)
    ctrl1 <- as.Date(casem_i + casedow - casem_i_dow)
    ctrl2 <- as.Date(ctrl1 + 7)
    ctrl3 <- as.Date(ctrl2 + 7)
    ctrl4 <- as.Date(ctrl3 + 7)
    ctrl5 <- as.Date(ctrl4 + 7)
    if (month(ctrl5) != casem) {ctrl5 = NULL}
    if (month(ctrl1) != casem) {ctrl1 = NULL}
    ctrldate_array <- c(ctrl1, ctrl2, ctrl3, ctrl4, ctrl5)
    if (casedate %in% ctrldate_array) {
      ctrldate_array <- ctrldate_array[-which(ctrldate_array==casedate)]
    }
    dataset$ctrl1[i] <- ctrldate_array[1]
    dataset$ctrl2[i] <- ctrldate_array[2]
    dataset$ctrl3[i] <- ctrldate_array[3]
    dataset$ctrl4[i] <- ctrldate_array[4]
  }
  dataset
}

## APPLY THE FUNCTION
stroke_cco <- ctrldate(stroke_t, "date")

## DATE FORMAT CONVERSION
stroke_cco$ctrl1 <- as_date(stroke_cco$ctrl1)
stroke_cco$ctrl2 <- as_date(stroke_cco$ctrl2)
stroke_cco$ctrl3 <- as_date(stroke_cco$ctrl3)
stroke_cco$ctrl4 <- as_date(stroke_cco$ctrl4)

###############################################################################
# 2 DATASET CONVERSION ( WIDE -> LONG ) ---------------------------------------
###############################################################################

library(reshape2)
stroke_cco <- melt(stroke_cco, 
               id.vars= names(stroke_cco)[-c(31, 33:36)],
               variable.name = "tdate_status", value.name = "tdate")
levels(stroke_cco$tdate_status) <- c("case","ctrl1", "ctrl2", "ctrl3", "ctrl4")
stroke_cco$tdate <- as_date(stroke_cco$tdate)

###############################################################################
# 3 DATA MERGING: AIRSTR  ------------------------------------------
###############################################################################

airstr <- merge(stroke_cco, env, by.x = "tdate", by.y = "date", all.x = TRUE)
airstr <- airstr %>% rename(id = ID)
airstr$status <- ifelse(airstr$tdate_status == "case", 1, 0)

# REARRANGE THE DATAFRAME
airstr <- airstr %>% dplyr::select(id, status, tdate, everything())

# EXCLUDE NA CTRL4
airstr <- airstr[!is.na(airstr$year), ]

