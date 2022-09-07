#__________________________________________________________________________

# INSTALL REQUIRED PACKAGE
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)

## 4S_Test Sample =========================================================
### READING ---------------------------------------------------------------
# create decrypted excel document, sort variable names (Stroke_4S_unlocked)

#### CORRECT DATA ERRORS -------------------------------------------------
## ERROR DEPICTION (highlighted in both Stroke_4S & Stroke_4S_final)

## > stroke_v1$ID[is.na(stroke_v1$lkw_dt)]
## [1] "D8D56C7A10264473920D2AAF7F38F551" "D20E7B887D3EAF92E050007F01008D68"
## [3] "96F8BD7358834E6F846F707D5D89A414"

## [1] "D8D56C7A10264473920D2AAF7F38F551" & its following row
##      containing 151 cases: all cases saved as Stroke_4S_supplement
## [2] "D20E7B887D3EAF92E050007F01008D68" [3] "96F8BD7358834E6F846F707D5D89A414"
##      residential address occupying 2 columns and all data rightly shifted

### READING CORRECTED DATA ------------------------------------------------
# corrected data named as (Stroke_4S_unlocked_final)
stroke_v1 <- read_excel("Data/Stroke_4S_unlocked_final.xlsx")

# overview of stroke_v1
str(stroke_v1)

### TIDYING ---------------------------------------------------------------
#### VARIABLE NAMES (120,909 X 36 var) ------------------------------------
colnames(stroke_v1) <- c("ID", "gender", "birth_dt", "age_ori", "residence", "lkw_dt", 
                   "onset_dt", "admission_dt", "discharge_dt", "hospt_name", "hospt_class", 
                   "htn", "dm", "mi", "ci", "tia", "ich", "af_vhd", "dyslipid", 
                   "smoking", "other_heartdis", "dementia", "COPD", "bleeding", 
                   "anti-htn", "anti-dm", "lip-reduc", "anti-coag", "premrs",
                   "bnihss", "IVT", "EVT", "toast", "m6", "disch_mrs", "dx")

#### DATETIME AND AGE -----------------------------------------------------
##### VARIABLE TYPE -------------------------------------------------------
stroke_v1$birth_dt <- ymd_hm(stroke_v1$birth_dt)
stroke_v1$lkw_dt <- ymd_hm(stroke_v1$lkw_dt)
stroke_v1$onset_dt <- ymd_hm(stroke_v1$onset_dt)
stroke_v1$admission_dt <- ymd_hm(stroke_v1$admission_dt)
stroke_v1$discharge_dt <- ymd_hm(stroke_v1$discharge_dt)

###### DATA CORRECTION --------------------------------------------------
## DATA ERROR DEPICTION: EXCEL FILTER
## onset_dt failed to parse: 10·7-04-09 20:00
stroke_v1$onset_dt[which(stroke_v1$ID == "F4CC61A91EEC41E1B95FB0F94CF0BEE7")]
stroke_v1$onset_dt[which(stroke_v1$ID == "F4CC61A91EEC41E1B95FB0F94CF0BEE7")] <- ymd_hm("2017-04-09 20:00")

##### COMPUTING AGE 
stroke_v1$age_comp <- trunc((date(stroke_v1$birth_dt) %--% date(stroke_v1$admission_dt)) / years(1))
stroke_v1$age_comp[is.na(stroke_v1$birth_dt)] <- NA
stroke_v1$age_comp[is.na(stroke_v1$admission_dt)] <- NA

## comparing age_ori & age_comp
stroke_v1$age_ori <- as.numeric(stroke_v1$age_ori)
stroke_v1$temp <- ifelse(stroke_v1$age_ori != stroke_v1$age_comp, 
                      stroke_v1$age_comp-stroke_v1$age_ori, 0)

setdiff(stroke_v1$age_ori[!is.na(stroke_v1$age_comp)], 
        stroke_v1$age_comp[!is.na(stroke_v1$age_comp)])   # NA 112
### checking age when age_ori==112
stroke_v1$age_comp[which(stroke_v1$age_ori == 112)]       # 74
stroke_v1$birth_dt[which(stroke_v1$age_ori == 112)]
stroke_v1$admission_dt[which(stroke_v1$age_ori == 112)]
stroke_v1$lkw_dt[which(stroke_v1$age_ori == 112)]
stroke_v1$onset_dt[which(stroke_v1$age_ori == 112)]
stroke_v1$discharge_dt[which(stroke_v1$age_ori == 112)]
### checking age when age_ori==NA
summary(stroke_v1$age_comp[is.na(stroke_v1$age_ori)])    # 28-95; 8 NAs
table(stroke_v1$age_comp[is.na(stroke_v1$age_ori)])
### CONCLUSION: In the presence of age_comp, age_comp is to believed.

## creating new corrected var: age
stroke_v1$age <- stroke_v1$age_comp
stroke_v1$age[is.na(stroke_v1$age_comp)] <- stroke_v1$age_ori[is.na(stroke_v1$age_comp)]

## stroke_v1$admission_dt[which(stroke_v1$age < 0)] <- ymd_hm("2021-01-01 10:00")
## repeat previous codes
## stroke_v1$age[which(stroke_v1$age == 0)] <- NA

#### (ORDERED) FACTOR VARIABLEs & NUMERIC VARIABLEs
stroke_v1$gender <- factor(stroke_v1$gender, order = T)
levels(stroke_v1$gender) <- c("male", "female")
stroke_v1$hospt_class <- factor(stroke_v1$hospt_class)
stroke_v1$hospt_class <- factor(stroke_v1$hospt_class, levels = c("三级甲等",  "三级乙等", 
                                                      "三级中医院", "二级综合", 
                                                      "社会办医院"))
stroke_v1$htn <- factor(stroke_v1$htn)
stroke_v1[, 12:27] <- lapply(stroke_v1[, 12:27], factor)
stroke_v1$premrs <- as.numeric(stroke_v1$premrs) - 1
stroke_v1$premrs <- factor(stroke_v1$premrs)

stroke_v1$bnihss <- as.numeric(stroke_v1$bnihss)      # all integers
stroke_v1$IVT <- as.factor(stroke_v1$IVT)
stroke_v1$EVT <- as.factor(stroke_v1$EVT)

stroke_v1$toast <- factor(stroke_v1$toast)
levels(stroke_v1$toast) <- c("LAA", "CE", "SVD", "Others", "UnKn")
stroke_v1$m6 <- as.factor(stroke_v1$m6)

stroke_v1$disch_mrs <- as.numeric(stroke_v1$disch_mrs)  # with decimals

stroke_v1$dx <- as.numeric(stroke_v1$dx)
stroke_v1$dx <- factor(stroke_v1$dx)
levels(stroke_v1$dx) <- c("AIS", "TIA", "ICH", "SAH", "Unspe")

# TARGET POPULATION: AGE≥18, AIS/TIA, admission-LKW<7, adm-onset>=0
stroke_v1_t <- stroke_v1[which(stroke_v1$dx == "AIS"), ]

