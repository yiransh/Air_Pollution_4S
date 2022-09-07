#__________________________________________________________________________

# INSTALL REQUIRED PACKAGE
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)

## 4S_Test Sample =========================================================
### READING ---------------------------------------------------------------
# create decrypted excel document named Test_4S_Dc
# applying sheet names to dataframe names
sheet <- readxl::excel_sheets("Data/Stroke_4S_Test_unlocked.xlsx")
t4s = lapply(setNames(sheet, sheet),
              function(x) read_excel("Data/Stroke_4S_Test_unlocked.xlsx", sheet=x))

# attaching all dataframes together
t4s = bind_rows(t4s, .id="year")

### TIDYING ---------------------------------------------------------------
# Standardizing Variables Names (35 var, 1 invalid)
t4s[, 36] <- NULL
colnames(t4s) <- c("year", "ID", "gender", "birth_dt", "residence", "lkw_dt", 
                   "onset_dt", "admission_dt", "hospt_name", "hospt_class", 
                   "htn", "dm", "mi", "ci", "tia", "ich", "af_vhd", "dyslipid", 
                   "smoking", "other_heartdis", "dementia", "COPD", "bleeding", 
                   "anti-htn", "anti-dm", "lip-reduc", "anti-coag", "premrs",
                   "bnihss", "IVT", "EVT", "toast", "m6", "disch_mrs", "dx")
# datetime
t4s$birth_dt <- ymd_hm(t4s$birth_dt)
t4s$lkw_dt <- ymd_hm(t4s$lkw_dt)
t4s$onset_dt <- ymd_hm(t4s$onset_dt)
t4s$admission_dt <- ymd_hm(t4s$admission_dt)
# new var
t4s$age <- trunc((date(t4s$birth_dt) %--% date(t4s$admission_dt)) / years(1))
t4s$age[is.na(t4s$birth_dt)] <- NA
t4s$age[is.na(t4s$admission_dt)] <- NA
## t4s$admission_dt[which(t4s$age < 0)] <- ymd_hm("2021-01-01 10:00")
## repeat previous codes
## t4s$age[which(t4s$age == 0)] <- NA

# (ordered) factor variable
t4s$gender <- factor(t4s$gender, order = T)
levels(t4s$gender) <- c("male", "female")
## t4s$hospt_class[which(t4s$hospt_class == "上海市普陀区中心医院")] <- "三级乙等"
t4s$hospt_class <- factor(t4s$hospt_class)
## t4s$hospt_class[which(t4s$hospt_name == "中国人民解放军第四一一医院")] <- "三级甲等"
t4s$hospt_class <- factor(t4s$hospt_class, levels = c("三级甲等",  "三级乙等", 
                                                      "三级中医院", "二级综合", 
                                                      "社会办医院"))
## t4s$htn[which(t4s$htn == "三级乙等")] <-1
t4s$htn <- factor(t4s$htn)
levels(t4s$htn) <- c("no", "yes", "NG", "NA")
t4s[, 12:27] <- lapply(t4s[, 12:27], factor)
t4s[, 12:27] <- lapply(t4s[, 12:27], recode_factor, "0" = "no", "1" = "yes", 
                       "2" = "NG", "NA" = "NA")
## t4s$premrs[which(t4s[,28]==0)] <- t4s$bnihss[which(t4s[,28]==0)]
t4s$premrs <- factor(t4s$premrs)
levels(t4s$premrs) <- c("0", "1", "2", "3", "4", "5")
t4s$bnihss <- as.numeric(t4s$bnihss)
t4s$IVT <- as.factor(t4s$IVT)
t4s$EVT <- as.factor(t4s$EVT)
## t4s$toast[which(t4s$toast==0)] <- t4s$m6[which(t4s$toast==0)]
t4s$toast <- factor(t4s$toast)
levels(t4s$toast) <- c("LAA", "CE", "SVD", "Others", "UnKn")
t4s$m6 <- as.factor(t4s$m6)
## t4s$disch_mrs[which(t4s$disch_mrs == "3.84")] <- "3"
t4s$disch_mrs <- as.numeric(t4s$disch_mrs)
t4s$disch_mrs <- as.factor(t4s$disch_mrs)
## t4s$dx[which(t4s$dx == "0")] <- "1"
t4s$dx <- as.numeric(t4s$dx)
t4s$dx <- factor(t4s$dx)
levels(t4s$dx) <- c("AIS", "TIA", "ICH", "SAH", "Unspe")

# TARGET POPULATION: AIS
t4s_t <- t4s[which(t4s$dx == "AIS"), ]

