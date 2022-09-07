#__________________________________________________________________________

# INSTALL REQUIRED PACKAGE
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)

## 4S_v2 =================================================================
### READING ---------------------------------------------------------------
# create decrypted excel document, sort variable names (Stroke_4S_unlocked)

#### CORRECT DATA ERRORS -------------------------------------------------
#### FORMATTING ERROR
## highlighted in both Stroke_4S_v2 & Stroke_4S_v2_unlocked)

## > stroke$ID[is.na(stroke$lkw_dt)]
## [1] "D20E7B887D3EAF92E050007F01008D68" "CDE5FCCC04FB6FE9E050007F01006F14"
## [3] "CD0B77953EBD5BACE050007F010006FE" "96F8BD7358834E6F846F707D5D89A414"

## [2] "CDE5FCCC04FB6FE9E050007F01006F14" [3] "CD0B77953EBD5BACE050007F010006FE"
##      having NA values: inhopistal_stroke
## [2] "D20E7B887D3EAF92E050007F01008D68" [3] "96F8BD7358834E6F846F707D5D89A414"
##      residential address occupying 2 columns and all data rightly shifted

#### INHOSPITAL STROKE
#### as well as checking other eligibility criteria

## > stroke$lkw_dt <- ymd_hm(stroke$lkw_dt)
## > stroke$admission_dt <- ymd_hm(stroke$admission_dt)
## > stroke$dur<-(date(stroke$lkw_dt) %--% date(stroke$admission_dt)/ddays(1))
## > summary(stroke$dur < 0 | stroke$dur >7)
##      Mode   FALSE    TRUE    NA's 
##    logical  113283    155      11
## > id_q <- (stroke$ID[which(stroke$dur < 0 | stroke$dur >7 | is.na(stroke$dur))])
## > write.csv(id_q, "ID_out_of_range")

## getting inhospital_stroke IDs: Stroke_4S_v2_inhospital_stroke
## merging datasets after reading


#### READING CORRECTED DATA ------------------------------------------------
# corrected data named as (Stroke_4S_v2_unlocked)
stroke <- read_excel("Data/Stroke_4S_v2_unlocked.xlsx")

# overview of stroke
str(stroke)


### TIDYING ---------------------------------------------------------------
#### VARIABLE NAMES (113,449 X 38 var) ------------------------------------
colnames(stroke) <- c("ID", "gender", "birth_dt", "age_ori", "residence", "lkw_dt", 
                      "date_onset", "onset_dt", "arrival_dt", "admission_dt", 
                      "discharge_dt", "hospt_name", "hospt_class","htn", "dm", 
                      "mi", "ci", "tia", "ich", "af_vhd", "dyslipid", 
                      "smoking", "other_heartdis", "dementia", "COPD", "bleeding", 
                      "anti-htn", "anti-dm", "lip-reduc", "anti-coag", "premrs",
                      "bnihss", "IVT", "EVT", "toast", "m6", "disch_mrs", "dx")

#### ADDING "inhospital_stroke" -------------------------------------------
inhospital_stroke <- read_excel("Data/Stroke_4S_v2_inhospital_stroke.xlsx")
inhospital_stroke$inhospital_stroke <- as.numeric(inhospital_stroke$inhospital_stroke)
stroke <- merge(stroke, inhospital_stroke, by = "ID", all.x = TRUE)
stroke$inhospital_stroke[is.na(stroke$inhospital_stroke)] <- 0

#### DATETIME AND AGE -----------------------------------------------------
##### VARIABLE TYPE -------------------------------------------------------
stroke$birth_dt <- ymd_hm(stroke$birth_dt)
stroke$lkw_dt <- ymd_hm(stroke$lkw_dt)
stroke$date_onset <- ymd_hm(stroke$date_onset)
stroke$onset_dt <- ymd_hm(stroke$onset_dt)
stroke$arrival_dt <- ymd_hm(stroke$arrival_dt)
stroke$admission_dt <- ymd_hm(stroke$admission_dt)
stroke$discharge_dt <- ymd_hm(stroke$discharge_dt)

###### DATA CORRECTION -----
## ONSET_DT FAILED TO PARSE
## depicted by EXCEL FILTER: 10·7-04-09 20:00
stroke$onset_dt[which(stroke$ID == "F4CC61A91EEC41E1B95FB0F94CF0BEE7")]
stroke$onset_dt[which(stroke$ID == "F4CC61A91EEC41E1B95FB0F94CF0BEE7")] <- ymd_hm("2017-04-09 20:00")

## INELIGIBLE CASES
## > stroke$dur<-(date(stroke$lkw_dt) %--% date(stroke$admission_dt)/ddays(1))
## > table(stroke$inhospital_stroke[which(stroke$dur < 0 | stroke$dur >7 | is.na(stroke$dur))])
## > View(stroke[which((stroke$dur < 0 | stroke$dur >7 | is.na(stroke$dur)) 
##                      & stroke$inhospital_stroke == 0), 
##               c("ID", "lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt")])
eligible_time <- read_excel("Data/Stroke_4S_v2_eligible_time.xlsx")
colnames(eligible_time) <- c("ID", "lkw_dt", "date_onset", "onset_dt", "arrival_dt",
                             "admission_dt", "discharge_dt", "hospt_name")
eligible_time$lkw_dt <- ymd_hm(eligible_time$lkw_dt)
eligible_time$date_onset <- ymd_hm(eligible_time$date_onset)
eligible_time$onset_dt <- ymd_hm(eligible_time$onset_dt)
eligible_time$arrival_dt <- ymd_hm(eligible_time$arrival_dt)
eligible_time$admission_dt <- ymd_hm(eligible_time$admission_dt)
eligible_time$discharge_dt <- ymd_hm(eligible_time$discharge_dt)
stroke$lkw_dt[which(stroke$ID == "CDE5FCCC04FB6FE9E050007F01006F14")] <- eligible_time$lkw_dt[which(eligible_time$ID == "CDE5FCCC04FB6FE9E050007F01006F14")]
stroke$admission_dt[which(stroke$ID == "2E5E0CEFF34E48A19EC719FDD779DE4C")] <- eligible_time$admission_dt[which(eligible_time$ID == "2E5E0CEFF34E48A19EC719FDD779DE4C")]
stroke$admission_dt[which(stroke$ID == eligible_time$ID[2])] <- eligible_time$admission_dt[2]
stroke$admission_dt[which(stroke$ID == eligible_time$ID[3])] <- eligible_time$admission_dt[3]
stroke$admission_dt[which(stroke$ID == eligible_time$ID[1])] <- eligible_time$admission_dt[1]
stroke$admission_dt[which(stroke$ID == eligible_time$ID[4])] <- eligible_time$admission_dt[4]
stroke$admission_dt[which(stroke$ID == eligible_time$ID[6])] <- eligible_time$admission_dt[6]
stroke$admission_dt[which(stroke$ID == eligible_time$ID[7])] <- eligible_time$admission_dt[7]

## VISIBLE ERROR (DATA CORRECTION_v2)
## 1. onset date range
##    2017/12/17 - 2021/12/31
## 2. onset date for inhospital_stroke
## 3. onset date for stroke outside the hospitals
##    1) onset_dt < admission_dt
##    2) onset_dt > admission_dt - 7 days

##### COMPUTING AGE -------------------------------------------------------
stroke$age_comp <- trunc((date(stroke$birth_dt) %--% date(stroke$admission_dt)) / years(1))
stroke$age_comp[is.na(stroke$birth_dt)] <- NA
stroke$age_comp[is.na(stroke$admission_dt)] <- NA

## comparing age_ori & age_comp
stroke$age_ori <- as.numeric(stroke$age_ori)
stroke$temp <- ifelse(stroke$age_ori != stroke$age_comp, 
                      stroke$age_comp-stroke$age_ori, 0)

## invalid birth_dt
stroke$age_comp[which(stroke$age_comp == 0)] <- NA

## creating corrected var: age
stroke$age <- stroke$age_ori
stroke$age[is.na(stroke$age)] <- stroke$age_comp[is.na(stroke$age)]


#### (ORDERED) FACTOR VARIABLEs & NUMERIC VARIABLEs -----------------------
stroke$gender <- factor(stroke$gender, order = T)
levels(stroke$gender) <- c("male", "female")
stroke$hospt_class <- factor(stroke$hospt_class)
stroke$hospt_class <- factor(stroke$hospt_class, levels = c("三级甲等",  "三级乙等", 
                                                            "三级中医院", "二级综合", 
                                                            "社会办医院"))
table(stroke$hospt_name[is.na(stroke$hospt_class)]) ## 411 医院
stroke$hospt_class[is.na(stroke$hospt_class)] <- "三级甲等"
stroke$htn <- factor(stroke$htn)
stroke[, 15:30] <- lapply(stroke[, 15:30], factor)
stroke$premrs <- as.numeric(stroke$premrs) - 1
stroke$premrs <- factor(stroke$premrs)

stroke$bnihss <- as.numeric(stroke$bnihss)      # all integers
stroke$IVT <- as.factor(stroke$IVT)
stroke$EVT <- as.factor(stroke$EVT)

stroke$toast <- factor(stroke$toast)
levels(stroke$toast) <- c("LAA", "CE", "SVD", "Others", "UnKn")
stroke$m6 <- as.factor(stroke$m6)

stroke$disch_mrs <- as.numeric(stroke$disch_mrs)  # with decimals
## disch_mrs: invalid value

stroke$dx <- as.numeric(stroke$dx)
stroke$dx <- factor(stroke$dx)
levels(stroke$dx) <- c("AIS", "TIA")




## TARGET POPULATION ==================================================
# 00 TOTAL: 113449
# 01 ADMISSION DATE WITHIN 2017-2021
#    admission_dt within [2017/1/1, 2021/12/31]
#    (possibility to omit patients with inhospital stroke admitted before 2017)
# 02 ADMISSION WITHIN 7D AFTER LKW
#    Non-inhospital Stroke: ADMISSION_DT -7 ≤ LKW_DT ≤ ADMISSION_DT
#    inhospital Stroke: ADMISSION_DT < LKW_DT
# 03 DATA COMPLETENESS & CONSISTENCY
#    - COMPLETE: lkw_dt, admission_dt, onset_dt
#    - CONSISTENCY: lkw_dt ≤ onset_dt
#                   Non-inhospital Stroke: ADMISSION_DT -7 ≤ ONSET_DT ≤ ADMISSION_DT
stroke_t <- stroke[which(year(stroke$admission_dt) %in% (2017:2021)), ]
stroke_t <- stroke_t[which(
  ((stroke_t$inhospital_stroke_cr == 0) & (stroke_t$dur <= 7) & (stroke_t$admission_dt >= stroke_t$lkw_dt))
  | ((stroke_t$inhospital_stroke_cr ==1) & (stroke_t$admission_dt <= stroke_t$lkw_dt))
  ), ]
stroke_t <- stroke_t[!(is.na(stroke_t$lkw_dt)
                     | is.na(stroke_t$onset_dt)
                     | is.na(stroke_t$admission_dt)), ]
stroke_t <- stroke_t[which(stroke_t$lkw_dt <= stroke_t$onset_dt), ]
stroke_t <- stroke_t[which(
  ((stroke_t$inhospital_stroke_cr == 0) & (stroke_t$dur2 <= 7) & (stroke_t$admission_dt >= stroke_t$onset_dt))
  | (stroke_t$inhospital_stroke_cr == 1)
), ]

