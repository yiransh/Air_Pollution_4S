
# 01 AIR POLLUTION ========================================================
#__________________________________________________________________________

# INSTALL REQUIRED PACKAGE
library(readxl)
library(tidyverse)
library(dplyr)
library(reshape2)

## PM2.5 =================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
sheet <- readxl::excel_sheets("Data/PM2.5.xlsx")
PM25 = lapply(setNames(sheet, sheet),
               function(x) read_excel("Data/PM2.5.xlsx", sheet=x))

# attaching all dataframes together
PM25 = bind_rows(PM25, .id="year")

# Checking the dataframe
str(PM25)
summary(PM25)               # 2 NAs in the "date" column
PM25[is.na(PM25$date), ]    # confirm if all values are NAs within the rows

# Renaming Columns
colnames(PM25) = c("year", "date","shiwuchang", "hongkou", "xuhui_snu",
                    "yangpu_sp", "qingpu_dsh", "jingan_jcz", "pudongxq_cs", 
                    "pudongxq_jcz", "pudongxq_zj", "mean", "SD", "putuo", 
                    "qingpu_dsh_na")

# Merging "qingpu_dsh" and "qingpu_dsh_na"
PM25 <- mutate(PM25, 
                "qingpu_dsh_c" = coalesce(PM25$qingpu_dsh, PM25$qingpu_dsh_na))


### TIDYING ---------------------------------------------------------------
## deleting columns
PM25$qingpu_dsh_na <- NULL
PM25$qingpu_dsh <- NULL
## reordering data
PM25 <- PM25[, c(1:2, 4, 7:10, 13, 3, 5:6, 14, 11:12)]
## converting numeric into date format
PM25$date <- as.Date(as.character(PM25$date), "%Y%m%d")
## filling the blanks of missing date
which(is.na(PM25$date), arr.ind = TRUE)                    # [1] 1331 1371
PM25[1331, 2] <- as.Date("2019-08-23")
PM25[1371, 2] <- as.Date("2019-10-02")
## checking missing date
daterange <- range(PM25$date)
alldates <- as.Date(format(seq(from = daterange[1], to = daterange[2], 'days'), "%Y%m%d"), "%Y%m%d")
setdiff(alldates, PM25$date)                      ## 3 days unmatched while numeric diff is 1 day
PM25[duplicated(PM25$date),]                      ## "2018-9-2"(977) "2018-11-1"(1037)
which(PM25$date == as.Date("2018-9-2"))           ## getting the row number
### replacing duplicates
PM25[977, 2] <- as.Date("2018-9-3")
PM25[1037, 2] <- as.Date("2018-11-2")
### find missing date "18321"
as.Date(18321, origin = "1970-1-1")                 ## return the date: "2020-02-29"
difftime("2020-2-29", "2016-1-1", units = "days")   ## estimating row number: 789 + 1 (790)
newrow <- list("2020", as.Date("2020-02-29"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
PM25[nrow(PM25)+1, ] <- newrow

# Check whether the Mean value was calculated based on data from the 10 stations
tempmean <- rowMeans(PM25[, 3:12], na.rm = T)
setdiff(tempmean, PM25$mean)                       ## 113 different values, both with 10 NAs
sd(as.numeric(PM25[1, 3:12]), na.rm = TRUE)
PM25[1, 13:14]

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
PM25 <- melt(PM25, id.vars = c("year", "date"), 
             variable.name = "sites", 
             value.name = "pm25")



## PM10 ==================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
PM10 = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/PM10.xlsx", sheet=x))

# attaching all dataframes together
PM10 = bind_rows(PM10, .id="year")

# Checking the dataframe
str(PM10)
summary(PM10)               # no NAs in the "date" column

# Renaming Columns
colnames(PM10) = c("year", "date","shiwuchang", "hongkou", "xuhui_snu",
                   "yangpu_sp", "qingpu_dsh", "jingan_jcz", "pudongxq_cs", 
                   "pudongxq_jcz", "pudongxq_zj", "mean", "SD", "putuo", 
                   "qingpu_dsh_na")

# Merging "qingpu_dsh" and "qingpu_dsh_na"
PM10 <- mutate(PM10, 
               "qingpu_dsh_c" = coalesce(PM10$qingpu_dsh, PM10$qingpu_dsh_na))


### TIDYING ---------------------------------------------------------------
## deleting columns
PM10$qingpu_dsh_na <- NULL
PM10$qingpu_dsh <- NULL
## reordering data
PM10 <- PM10[, c(1:2, 4, 7:10, 13, 3, 5:6, 14, 11:12)]
## converting numeric into date format
PM10$date <- as.Date(as.character(PM10$date), "%Y%m%d")
## filling the blanks of missing date
which(is.na(PM10$date), arr.ind = TRUE)           ## no NAs in the "date" col
## checking missing date
setdiff(alldates, PM10$date)                      ## 2 days unmatched while there's no numeric diff
PM10[duplicated(PM10$date),]                      ## "2018-9-2"(977) "2018-11-1"(1037)
which(PM10$date == as.Date("2018-9-2"))           ## getting the row number
### replacing duplicates
PM10[977, 2] <- as.Date("2018-9-3")
PM10[1037, 2] <- as.Date("2018-11-2")

# Check whether the Mean value was calculated based on data from the 10 stations
tempmean <- rowMeans(PM10[, 3:12], na.rm = T)
setdiff(tempmean, PM10$mean)                       ## consistent
sd(as.numeric(PM10[1, 3:12]), na.rm = TRUE)
PM10[1, 13:14]

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
PM10 <- melt(PM10, id.vars = c("year", "date"), 
             variable.name = "sites", 
             value.name = "pm10")



## CO ====================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
CO = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/CO.xlsx", sheet=x))

# attaching all dataframes together
CO = bind_rows(CO, .id="year")

# Checking the dataframe
str(CO)
summary(CO)               # no NAs in the "date" column

# Renaming Columns
colnames(CO) = c("year", "date","shiwuchang", "hongkou", "xuhui_snu",
                   "yangpu_sp", "qingpu_dsh", "jingan_jcz", "pudongxq_cs", 
                   "pudongxq_jcz", "pudongxq_zj", "mean", "SD", "putuo", 
                   "qingpu_dsh_na")

# Merging "qingpu_dsh" and "qingpu_dsh_na"
CO <- mutate(CO, 
               "qingpu_dsh_c" = coalesce(CO$qingpu_dsh, CO$qingpu_dsh_na))


### TIDYING ---------------------------------------------------------------
## deleting columns
CO$qingpu_dsh_na <- NULL
CO$qingpu_dsh <- NULL
## reordering data
CO <- CO[, c(1:2, 4, 7:10, 13, 3, 5:6, 14, 11:12)]
## converting numeric into date format
CO$date <- as.Date(as.character(CO$date), "%Y%m%d")
## filling the blanks of missing date
which(is.na(CO$date), arr.ind = TRUE)             ## no NAs in the "date" col
## checking missing date
setdiff(alldates, CO$date)                        ## 4 days unmatched VS size diff is 2
CO[duplicated(CO$date),]                          ## "2018-9-2"(977) "2018-11-1"(1037)
which(CO$date == as.Date("2018-9-2"))             ## getting the row number
### replacing duplicates
CO[977, 2] <- as.Date("2018-9-3")
CO[1037, 2] <- as.Date("2018-11-2")
### find missing date "18131"
as.Date(18131, origin = "1970-1-1")                 ## return the date: "2019-08-23"
difftime("2019-8-23", "2016-1-1", units = "days")   ## estimating row number: ~1330
CO[c(1328:1332),]                                   ## double check
newrow <- list("2019", as.Date("2019-08-23"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
CO[nrow(CO)+1, ] <- newrow
### find missing date "18171"
as.Date(18171, origin = "1970-1-1")                 ## return the date: "2019-10-02"
difftime("2019-10-02", "2016-1-1", units = "days")  ## estimating row number: ~1370
CO[c(1368:1372),]                                   ## double check
newrow <- list("2019", as.Date("2019-10-02"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
CO[nrow(CO)+1, ] <- newrow

# Check whether the Mean value was calculated based on data from the 10 stations
tempmean <- rowMeans(CO[, 3:12], na.rm = T)
setdiff(tempmean, CO$mean)                       ## 198 different values, may due to rounding
sd(as.numeric(CO[1, 3:12]), na.rm = TRUE)
CO[1, 13:14]

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
CO <- melt(CO, id.vars = c("year", "date"), 
             variable.name = "sites", 
             value.name = "co")



## NO2 ===================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
NO2 = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/NO2.xlsx", sheet=x))

# attaching all dataframes together
NO2 = bind_rows(NO2, .id="year")

# Checking the dataframe
str(NO2)
summary(NO2)

# Renaming Columns
colnames(NO2) = c("year", "date","shiwuchang", "hongkou", "xuhui_snu",
                   "yangpu_sp", "qingpu_dsh", "jingan_jcz", "pudongxq_cs", 
                   "pudongxq_jcz", "pudongxq_zj", "mean", "SD", "putuo", 
                   "qingpu_dsh_na")

# Merging "qingpu_dsh" and "qingpu_dsh_na"
NO2 <- mutate(NO2, 
               "qingpu_dsh_c" = coalesce(NO2$qingpu_dsh, 
                                         NO2$qingpu_dsh_na))


### TIDYING ---------------------------------------------------------------
## deleting columns
NO2$qingpu_dsh_na <- NULL
NO2$qingpu_dsh <- NULL
## reordering data
NO2 <- NO2[, c(1:2, 4, 7:10, 13, 3, 5:6, 14, 11:12)]
## converting numeric into date format
NO2$date <- as.Date(as.character(NO2$date), "%Y%m%d")
## filling the blanks of missing date
which(is.na(NO2$date), arr.ind = TRUE)           ## [1] no NAs in the "date" col
## checking missing date
setdiff(alldates, NO2$date)                      ## 4 days unmatched VS size diff=2
NO2[duplicated(NO2$date),]                       ## "2018-9-2"(977) "2018-11-1"(1037)
which(NO2$date == as.Date("2018-9-2"))           ## getting the row number
### replacing duplicates
NO2[977, 2] <- as.Date("2018-9-3")
NO2[1037, 2] <- as.Date("2018-11-2")
### find missing date "18131"
as.Date(18131, origin = "1970-1-1")                 ## return the date: "2019-08-23"
difftime("2019-8-23", "2016-1-1", units = "days")   ## estimating row number: ~1330
NO2[c(1328:1332),]                                  ## double check
newrow <- list("2019", as.Date("2019-08-23"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
NO2[nrow(NO2)+1, ] <- newrow
### find missing date 18171"
as.Date(18171, origin = "1970-1-1")                 ## return the date: "2019-10-02"
difftime("2019-10-02", "2016-1-1", units = "days")  ## estimating row number: ~1370
NO2[c(1368:1372),]                                  ## double check
newrow <- list("2019", as.Date("2019-10-02"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
NO2[nrow(NO2)+1, ] <- newrow

# Check whether the Mean value was calculated based on data from the 10 stations
tempmean <- rowMeans(NO2[, 3:12], na.rm = T)
setdiff(tempmean, NO2$mean)                         ## consistent
sd(as.numeric(NO2[1, 3:12]), na.rm = TRUE)
NO2[1, 13:14]

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
NO2 <- melt(NO2, id.vars = c("year", "date"), 
             variable.name = "sites", 
             value.name = "NO2")



## SO2 ===================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
SO2 = lapply(setNames(sheet, sheet),
             function(x) read_excel("Data/SO2.xlsx", sheet=x))

# attaching all dataframes together
SO2 = bind_rows(SO2, .id="year")

# Checking the dataframe
str(SO2)
summary(SO2)

# Renaming Columns
colnames(SO2) = c("year", "date","shiwuchang", "hongkou", "xuhui_snu",
                  "yangpu_sp", "qingpu_dsh", "jingan_jcz", "pudongxq_cs", 
                  "pudongxq_jcz", "pudongxq_zj", "mean", "SD", "putuo", 
                  "qingpu_dsh_na")

# Merging "qingpu_dsh" and "qingpu_dsh_na"
SO2 <- mutate(SO2, 
              "qingpu_dsh_c" = coalesce(SO2$qingpu_dsh, 
                                        SO2$qingpu_dsh_na))


### TIDYING ---------------------------------------------------------------
## deleting columns
SO2$qingpu_dsh_na <- NULL
SO2$qingpu_dsh <- NULL
## reordering data
SO2 <- SO2[, c(1:2, 4, 7:10, 13, 3, 5:6, 14, 11:12)]
## converting numeric into date format
SO2$date <- as.Date(as.character(SO2$date), "%Y%m%d")
## filling the blanks of missing date
which(is.na(SO2$date), arr.ind = TRUE)           ## [1] no NAs in the "date" col
## checking missing date
setdiff(alldates, SO2$date)                      ## 4 days unmatched VS size diff=2
SO2[duplicated(SO2$date),]                       ## "2018-9-2"(977) "2018-11-1"(1037)
which(SO2$date == as.Date("2018-9-2"))           ## getting the row number
### replacing duplicates
SO2[977, 2] <- as.Date("2018-9-3")
SO2[1037, 2] <- as.Date("2018-11-2")
### find missing date "18131"
as.Date(18131, origin = "1970-1-1")                 ## return the date: "2019-08-23"
difftime("2019-8-23", "2016-1-1", units = "days")   ## estimating row number: ~1330
SO2[c(1328:1332),]                                  ## double check
newrow <- list("2019", as.Date("2019-08-23"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
SO2[nrow(SO2)+1, ] <- newrow
### find missing date 18171"
as.Date(18171, origin = "1970-1-1")                 ## return the date: "2019-10-02"
difftime("2019-10-02", "2016-1-1", units = "days")  ## estimating row number: ~1370
SO2[c(1368:1372),]                                  ## double check
newrow <- list("2019", as.Date("2019-10-02"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
SO2[nrow(SO2)+1, ] <- newrow

# Check whether the Mean value was calculated based on data from the 10 stations
tempmean <- rowMeans(SO2[, 3:12], na.rm = T)
setdiff(tempmean, SO2$mean)                         ## consistent
sd(as.numeric(SO2[1, 3:12]), na.rm = TRUE)
SO2[1, 13:14]

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
SO2 <- melt(SO2, id.vars = c("year", "date"), 
            variable.name = "sites", 
            value.name = "SO2")



## OZONE =================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
O3 = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/O3_24h_8h.xlsx", sheet=x))

# attaching all dataframes together
O3 = bind_rows(O3, .id="year")

# Checking the dataframe
str(O3)
summary(O3)             

# Renaming Columns
colnames(O3) = c("year", "date","shiwuchang", "hongkou", "xuhui_snu",
                   "yangpu_sp", "qingpu_dsh", "jingan_jcz", "pudongxq_cs", 
                   "pudongxq_jcz", "pudongxq_zj", "mean", "SD", "putuo", 
                   "qingpu_dsh_na")

# Merging "qingpu_dsh" and "qingpu_dsh_na"
O3 <- mutate(O3, 
               "qingpu_dsh_c" = coalesce(O3$qingpu_dsh, 
                                         O3$qingpu_dsh_na))


### TIDYING ---------------------------------------------------------------
## deleting columns
O3$qingpu_dsh_na <- NULL
O3$qingpu_dsh <- NULL
## reordering data
O3 <- O3[, c(1:2, 4, 7:10, 13, 3, 5:6, 14, 11:12)]
## converting numeric into date format
O3$date <- as.Date(as.character(O3$date), "%Y%m%d")
## filling the blanks of missing date
which(is.na(O3$date), arr.ind = TRUE)           ## no NAs in the "date" col
## checking missing date
setdiff(alldates, O3$date)                      ## 4 days unmatched VS size diff=2
O3[duplicated(O3$date),]                        ## "2018-9-2"(977) "2018-11-1"(1037)
which(O3$date == as.Date("2018-9-2"))           ## getting the row number
### replacing duplicates
O3[977, 2] <- as.Date("2018-9-3")
O3[1037, 2] <- as.Date("2018-11-2")
### find missing date "18131"
as.Date(18131, origin = "1970-1-1")                 ## return the date: "2019-08-23"
difftime("2019-8-23", "2016-1-1", units = "days")   ## estimating row number: ~1330
O3[c(1328:1332),]                                   ## double check
newrow <- list("2019", as.Date("2019-08-23"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
O3[nrow(O3)+1, ] <- newrow
### find missing date "18171"
as.Date(18171, origin = "1970-1-1")                 ## return the date: "2019-10-02"
difftime("2019-10-02", "2016-1-1", units = "days")  ## estimating row number: ~1370
O3[c(1368:1372),]                                   ## double check
newrow <- list("2019", as.Date("2019-10-02"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
O3[nrow(O3)+1, ] <- newrow

# Check whether the Mean value was calculated based on data from the 10 stations
tempmean <- rowMeans(O3[, 3:12], na.rm = T)
setdiff(tempmean, O3$mean)                       ## consistent
sd(as.numeric(O3[1, 3:12]), na.rm = TRUE)
O3[1, 13:14]

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
O3 <- melt(O3, id.vars = c("year", "date"), 
             variable.name = "sites", 
             value.name = "o3")


## HONO =================================================================
### READING ---------------------------------------------------------------
# applying sheet names to dataframe names
sheet <- readxl::excel_sheets("Data/QP_HONO.xlsx")
HONO = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/QP_HONO.xlsx", sheet=x))

# attaching all dataframes together
HONO = bind_rows(HONO)

# Checking the dataframe
str(HONO)
HONO$Season <- as.factor(HONO$Season)
summary(HONO)               # 2017-2020; datetime stands for start time

### RESHAPING -------------------------------------------------------------
# setting monitor sites as a single variable
HONO$date <- date(HONO$datetime)
HONO <- HONO[, c(2:8)]
HONO$time <- as.character(HONO$time)
HONO$ap <- 'hono'
HONO <- HONO %>% rename(year = Year)
hono <- dcast(HONO, year + date ~ ap, value.var = 'HNO2', 
              fun.aggregate = mean, na.rm=T)   # 77 NAs
table(
  table(HONO$date[which(is.na(HONO$HNO2))])
  )                                            # 77 days with no value at all
                                               # 348 days with ≥1 missing values)




## DISTINGUISHING BETWEEN URBAN/SUBURBAN DISTRICTS =======================
urban <- c("hongkou", "jingan_jcz", "putuo", "shiwuchang", "xuhui_snu",
           "yangpu")
statval <- c("mean", "sd")

### PM2.5 ----------------------------------------------------------------
PM25$urban <- ifelse(PM25$sites %in% urban, "urban", 
                     ifelse(PM25$sites %in% statval, "na", "suburban"))
PM25$urban <- as.factor(PM25$urban)
PM25$urban3 <- ifelse(PM25$sites %in% urban, "urban", 
                     ifelse(PM25$sites %in% statval, "na", 
                            ifelse(PM25$site == "qingpu_dsh_c", "control",
                            "suburban")))
PM25$urban3 <- as.factor(PM25$urban3)

### PM10 -----------------------------------------------------------------
PM10$urban <- ifelse(PM10$sites %in% urban, "urban", 
                     ifelse(PM10$sites %in% statval, "na", "suburban"))
PM10$urban <- as.factor(PM10$urban)
PM10$urban3 <- ifelse(PM10$sites %in% urban, "urban", 
                      ifelse(PM10$sites %in% statval, "na", 
                             ifelse(PM10$site == "qingpu_dsh_c", "control",
                                    "suburban")))
PM10$urban3 <- as.factor(PM10$urban3)

### CO -----------------------------------------------------------------
CO$urban <- ifelse(CO$sites %in% urban, "urban", 
                     ifelse(CO$sites %in% statval, "na", "suburban"))
CO$urban <- as.factor(CO$urban)
CO$urban3 <- ifelse(CO$sites %in% urban, "urban", 
                      ifelse(CO$sites %in% statval, "na", 
                             ifelse(CO$site == "qingpu_dsh_c", "control",
                                    "suburban")))
CO$urban3 <- as.factor(CO$urban3)
### NO2 -----------------------------------------------------------------
NO2$urban <- ifelse(NO2$sites %in% urban, "urban", 
                     ifelse(NO2$sites %in% statval, "na", "suburban"))
NO2$urban <- as.factor(NO2$urban)
NO2$urban3 <- ifelse(NO2$sites %in% urban, "urban", 
                      ifelse(NO2$sites %in% statval, "na", 
                             ifelse(NO2$site == "qingpu_dsh_c", "control",
                                    "suburban")))
NO2$urban3 <- as.factor(NO2$urban3)
### SO2 -----------------------------------------------------------------
SO2$urban <- ifelse(SO2$sites %in% urban, "urban", 
                    ifelse(SO2$sites %in% statval, "na", "suburban"))
SO2$urban <- as.factor(SO2$urban)
SO2$urban3 <- ifelse(SO2$sites %in% urban, "urban", 
                     ifelse(SO2$sites %in% statval, "na", 
                            ifelse(SO2$site == "qingpu_dsh_c", "control",
                                   "suburban")))
SO2$urban3 <- as.factor(O2$urban3)
### O3 -----------------------------------------------------------------
O3$urban <- ifelse(O3$sites %in% urban, "urban", 
                    ifelse(O3$sites %in% statval, "na", "suburban"))
O3$urban <- as.factor(O3$urban)
O3$urban3 <- ifelse(O3$sites %in% urban, "urban", 
                     ifelse(O3$sites %in% statval, "na", 
                            ifelse(O3$site == "qingpu_dsh_c", "control",
                                   "suburban")))
O3$urban3 <- as.factor(O3$urban3)


# 02 METEROLOGICAL DATA ==================================================
#_________________________________________________________________________

### BAOSHAN ==============================================================
#### READING -------------------------------------------------------------
# applying sheet names to dataframe names
Metb = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/0-Met_2016_2021_宝山.xls", sheet=x))

# attaching all dataframes together
Metb = bind_rows(Metb, .id="year")

# Checking the dataframe
str(Metb)
summary(Metb)               # no NAs in the "date" column

# Renaming Columns
colnames(Metb) = c("year", "date","temperature", "rh")

#### TIDYING ---------------------------------------------------------------
## converting numeric into date format
library(lubridate)
Metb$date <- as_date(as.character(Metb$date))
## checking missing date
setdiff(alldates, Metb$date)                      ## consistent


### XUJIAHUI =============================================================
#### READING -------------------------------------------------------------
# applying sheet names to dataframe names
Metx = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/0-Met_2016_2021_徐家汇.xls", sheet=x))

# attaching all dataframes together
Metx = bind_rows(Metx, .id="year")

# Checking the dataframe
str(Metx)
summary(Metx)               # no NAs in the "date" column

# Renaming Columns
colnames(Metx) = c("year", "date","temperature", "rh")

#### TIDYING ---------------------------------------------------------------
## converting numeric into date format
Metx$date <- as_date(as.character(Metx$date))
## checking missing date
setdiff(alldates, Metb$date)                      ## consistent



### MERGING INTO ONE -----------------------------------------------------
Metb$sites <- "baoshan"
Metx$sites <- "xujiahui"
met <- merge(Metb, Metx, by.x = "date", by.y = "date", all = T)
met$year.y <- NULL


### TEMPERATURE ----------------------------------------------------------
temp <- met[, c(1:3, 6)] 
colnames(temp) <- c("date", "year", "baoshan", "xujiahui")
temp$mean <- rowMeans(temp[, 3:4], na.rm = T)
str(temp)
summary(temp)
# setting monitor sites as a single variable
temp <- melt(temp, id.vars = c("year", "date"), 
           variable.name = "sites", 
           value.name = "temp")


### RELATIVE HUMIDITY ----------------------------------------------------
rh <- met[, c(1:2, 4, 7)] 
colnames(rh) <- c("date", "year", "baoshan", "xujiahui")
rh$mean <- rowMeans(rh[, 3:4], na.rm = T)
str(temp)
summary(temp)
# setting monitor sites as a single variable
rh <- melt(rh, id.vars = c("year", "date"), 
           variable.name = "sites", 
           value.name = "rh")




# 03 MASTERSHEET OF MEAN ================================================
#________________________________________________________________________

# REORDER THE DATAFRAMES
PM25 <- PM25[order(PM25$date),]
PM10 <- PM10[order(PM10$date),]
NO2 <- NO2[order(NO2$date),]
SO2 <- SO2[order(SO2$date),]
CO <- CO[order(CO$date),]
O3 <- O3[order(O3$date),]
temp <- temp[order(temp$date),]
rh <- rh[order(rh$date),]
# DOUBLE CHECK
identical(PM25$date, PM10$date)

# MERGING INTO ONE (mean values)
env <- cbind(
  split(PM25, PM25$sites)$mean[, c(1:2,4)],
  split(PM10, PM10$sites)$mean[, 4],
  split(CO, CO$sites)$mean[, 4],
  split(NO2, NO2$sites)$mean[, 4],
  split(SO2, SO2$sites)$mean[, 4],
  split(O3, O3$sites)$mean[, 4],
  split(temp, temp$sites)$mean[, 4],
  split(rh, rh$sites)$mean[, 4]
)
colnames(env) <- c("year", "date", "pm25", "pm10","co", "no2", "so2", "o3", 
                   "temp", "rh")

## COMPUTING NEW VARIABLES ===========================================
# CREATING A COLUMN FOR COMMONDATE 
## for plotting vertical facet plots by year
env$CommonDate <- as.Date(paste0("2000-",format(env$date, "%j")), "%Y-%j")
env <- env[, c(2, 1, 11, 3:10)]
env$year <- as.factor(env$year)

## SUPPLEMENT: MISSING VALUES ========================================
sheet <- readxl::excel_sheets("Data/Air_Pollutant-Missing_Values.xlsx")
missval_ap = lapply(setNames(sheet, tolower(sheet)),
              function(x) read_excel("Data/Air_Pollutant-Missing_Values.xlsx", 
                                     col_names = c("date", "values"),sheet=x))
missval_ap = bind_rows(missval_ap, .id="ap")
missval_ap <- dcast(missval_ap, date ~ ap, value.var = "values")
env <- env %>% 
  rows_update(missval_ap[, c("date", "pm25")] %>% filter(!is.na(pm25)), by = "date")
for (i in 1:(ncol(missval_ap) -1)) {
  env <- env %>% 
    rows_update(missval_ap[, c(1, i+1)] %>% 
                  filter(!is.na(missval_ap[, i+1])), by = "date")
}

