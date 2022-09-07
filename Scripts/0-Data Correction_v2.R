# ====================
# ONSET_DT CORRECTION 
# ====================

################################################################################
# BASIC ASSUMPTION #
# - ADMISSION_DT & LKW_DT are reliable
# - LKW_DT ≤ ONSET_DT
# - Non-inhospital Stroke: ADMISSION_DT -7 ≤ LKW_DT ≤ ADMISSION_DT    [SOLID CRITERIA]
# - Non-inhospital Stroke: ADMISSION_DT -7 ≤ ONSET_DT ≤ ADMISSION_DT
# - Inhospital Stroke: ADMISSION_DT ≥ LKW_DT
################################################################################

# ONSET_DT OUT OF RANGE ========================================================
View(stroke[which((date(stroke$onset_dt) < as.Date("2016-12-17"))), c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
year(stroke$onset_dt[which(stroke$ID=="290EB335008346C383E5A0B6226D7429")]) <- 2017
year(stroke$onset_dt[which(stroke$ID=="73CE282959A647B99A555DF78F47CEE9")]) <- 2017
day(stroke$onset_dt[which(stroke$ID=="73CE282959A647B99A555DF78F47CEE9")]) <- 15 ## referring to onset_date
year(stroke$onset_dt[which(stroke$ID=="7F78CFD115538236E050010A050263F0")]) <- 2018
year(stroke$onset_dt[which(stroke$ID=="D3C3330354E446018D5CCB86FC0A67B6")]) <- 2019

View(stroke[which((year(stroke$onset_dt) > 2021)), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke")])
year(stroke$onset_dt[which(stroke$ID=="1613D01254254FCF9785828D5502B8E8")]) <- 2017
year(stroke$onset_dt[which(stroke$ID=="8F8136605F2445EBB3F637C7989A518E")]) <- 2017



## PATIENTS WITH INHOSPITAL_STROKE ==============================================
# ONSET_DT almost within range, except for 1 NA
## > summary(stroke$onset_dt[which(stroke$inhospital_stroke==1)])
##      Min.               1st Qu.                Median 
##   "2016-12-23 23:30:00" "2018-08-19 20:30:30" "2019-11-17 05:00:00" 
##      Mean               3rd Qu.                  Max. 
##    "2019-09-28 08:49:05" "2020-10-14 07:05:00" "2021-12-29 07:20:00" 
##      NA's 
##      "1" 
# ERROR: ADMISSION_DT > LKW_DT or NA
## NAs, nonthing can be done
View(stroke[(which(stroke$inhospital_stroke ==1) & is.na(stroke$dur)), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke")])

## ADMISSION_DT > LKW_DT
View(stroke[which((stroke$inhospital_stroke ==1) & (stroke$dur > 0)), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke")])
summary((stroke$dur[which(stroke$inhospital_stroke ==1)] > 0))
summary((stroke$dur[which(stroke$inhospital_stroke ==1)] > 0) & (stroke$dur2[which(stroke$inhospital_stroke ==1)] > 0))
### mutate new var: corrected inhopistal_stroke
stroke$inhospital_stroke_cr <- stroke$inhospital_stroke
stroke$inhospital_stroke_cr[which(stroke$inhospital_stroke ==1) & (stroke$dur >0) & (stroke$dur2 >0)] <- 0

## PATIENTS HAD STROKE OUTSIDE THE HOSPITAL =====================================
stroke$dur2<-(date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1))
## > summary(((stroke$onset_dt > stroke$admission_dt) | (stroke$dur2 >7)) & stroke$inhospital_stroke_cr ==0)
##    Mode   FALSE    TRUE    NA's 
## logical  112568      97     784  

### ONSET_DT > ADMISSION_DT -----------------------------------------------------
## 65 cases, 7 cases with different date(Y/M/D, onset and admission)
View(stroke[which((stroke$onset_dt > stroke$admission_dt) & stroke$inhospital_stroke_cr ==0), 
     c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
summary(date(stroke$onset_dt[which((stroke$onset_dt > stroke$admission_dt) & (stroke$inhospital_stroke_cr ==0) & (stroke$inhospital_stroke_cr ==0))]) == 
          date(stroke$admission_dt[which((stroke$onset_dt > stroke$admission_dt) & (stroke$inhospital_stroke_cr ==0) & (stroke$inhospital_stroke_cr ==0))]))
##    Mode   FALSE    TRUE 
## logical       7      58 

View(stroke[which((stroke$onset_dt > stroke$admission_dt) & (stroke$inhospital_stroke_cr ==0)
                  & (date(stroke$onset_dt) != date(stroke$admission_dt))), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
year(stroke$onset_dt[which(stroke$ID == "1F975A26515148CEAB28C33DD3C6AD29")]) <- 2019
year(stroke$onset_dt[which(stroke$ID == "7F78CFD114228236E050010A050263F0")]) <- 2018
month(stroke$onset_dt[which(stroke$ID == "89A946BA06574F30B174E6845E459838")]) <- 2
month(stroke$onset_dt[which(stroke$ID == "CDE5FFD03D65AD2AE050007F01006F0A")]) <- 6
day(stroke$onset_dt[which(stroke$ID == "DC0D7160940A42289A0CAD83414D416B")]) <- 21
day(stroke$onset_dt[which(stroke$ID == "DE3397887A374DA098E70E7C33F6B64C")]) <- 15
date(stroke$onset_dt[which(stroke$ID == "7F78CFD115C68236E050010A050263F0")]) <- ymd(20180107)
date(stroke$onset_dt[which(stroke$ID == "7F78CFD1162E8236E050010A050263F0")]) <- ymd(20180305)
date(stroke$onset_dt[which(stroke$ID == "CCB8EF73C800F0C8E050007F010006F8")]) <- ymd(20210414)


### ONSET_DT < ADMISSION_DT -7 --------------------------------------------------
## 24 cases
## comparing with lkw_dt
View(stroke[which((stroke$dur2 >7) & stroke$inhospital_stroke_cr ==0), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
## correct year
year(stroke$onset_dt[which((year(stroke$lkw_dt) == year(stroke$admission_dt)) 
                           & (year(stroke$onset_dt) != year(stroke$lkw_dt))
                           & (stroke$dur2 >7) & (stroke$inhospital_stroke_cr ==0))]) <- 
  year(stroke$lkw_dt[which((year(stroke$lkw_dt) == year(stroke$admission_dt)) 
                           & (year(stroke$onset_dt) != year(stroke$lkw_dt))
                           & (stroke$dur2 >7) & (stroke$inhospital_stroke_cr ==0))])
## correct month
month(stroke$onset_dt[which((year(stroke$lkw_dt) == year(stroke$admission_dt)) 
                           & (year(stroke$onset_dt) == year(stroke$lkw_dt))
                           & (month(stroke$lkw_dt) == month(stroke$admission_dt))
                           & (month(stroke$lkw_dt) != month(stroke$onset_dt))
                           & (stroke$dur2 >7) & (stroke$inhospital_stroke_cr ==0)
                           )]) <- 
  month(stroke$lkw_dt[which((year(stroke$lkw_dt) == year(stroke$admission_dt)) 
                           & (year(stroke$onset_dt) == year(stroke$lkw_dt))
                           & (month(stroke$lkw_dt) == month(stroke$admission_dt))
                           & (month(stroke$lkw_dt) != month(stroke$onset_dt))
                           & (stroke$dur2 >7) & (stroke$inhospital_stroke_cr ==0))])
## correct date
### comparing lkw_dt, onset_dt, admission_dt
day(stroke$onset_dt[which((year(stroke$lkw_dt) == year(stroke$admission_dt)) 
                            & (year(stroke$onset_dt) == year(stroke$lkw_dt))
                            & (month(stroke$lkw_dt) == month(stroke$admission_dt))
                            & (month(stroke$lkw_dt) == month(stroke$onset_dt))
                            & (day(stroke$onset_dt) != day(stroke$admission_dt))
                            & (day(stroke$admission_dt) == day (stroke$lkw_dt))
                            & (stroke$dur2 >7) & (stroke$inhospital_stroke_cr ==0)
                            )]) <- 
  day(stroke$lkw_dt[which((year(stroke$lkw_dt) == year(stroke$admission_dt)) 
                            & (year(stroke$onset_dt) == year(stroke$lkw_dt))
                            & (month(stroke$lkw_dt) == month(stroke$admission_dt))
                            & (month(stroke$lkw_dt) == month(stroke$onset_dt))
                            & (day(stroke$admission_dt) == day (stroke$lkw_dt))
                            & (day(stroke$onset_dt) != day(stroke$admission_dt))
                            & (stroke$dur2 >7) & (stroke$inhospital_stroke_cr ==0))])
### manual comparison
day(stroke$onset_dt[which(stroke$ID == "02247C16530740698946CC11BBEB329F")]) <- 29
day(stroke$onset_dt[which(stroke$ID == "8AB0C74B0BAA4AD293A7554A3EF529C4")]) <- 23
day(stroke$onset_dt[which(stroke$ID == "CCB8EF73C7C6F0C8E050007F010006F8")]) <- 28
day(stroke$onset_dt[which(stroke$ID == "CCB8EF73C80AF0C8E050007F010006F8")]) <- 16
  



# LKW_DT > ONSET_DT ------------------------------------------------------------
## 314 cases (not including 785 with NA values)
## > summary(stroke$lkw_dt <= stroke$onset_dt)
##      Mode   FALSE    TRUE    NA's 
##   logical     314  112350     785 
View(stroke[which(stroke$lkw_dt > stroke$onset_dt), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])

## HMS: LKW_DT > ONSET_DT
View(stroke[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt) == day(stroke$onset_dt))
                  & (date(stroke$lkw_dt) < date(stroke$date_onset))
                  & (stroke$inhospital_stroke_cr == 0)
                  & (date(stroke$date_onset) <= date(stroke$admission_dt))), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
### manual replacement: taking HMS of admission_dt into consideration
day(stroke$onset_dt[which(stroke$ID == "4F923372547F4F90AFCDBB6067031542")]) <- 13

View(stroke[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt) == day(stroke$onset_dt))
                  & (date(stroke$lkw_dt) < date(stroke$date_onset))
                  & (stroke$inhospital_stroke_cr == 1)), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])


## date: LKW_DT > ONSET_DT
### inhospital stroke: correction based on DATE_ONSET
day(stroke$onset_dt[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                          & (stroke$inhospital_stroke_cr == 1)
                          & (date(stroke$date_onset) > date(stroke$onset_dt)))]) <- 9
### non-inhospital stroke: correction based on ADMISSION_DT
### (ignoring ARRIVAL_DT)
#### date: (ONSET_DT) < LKW ≤ DATE_ONSET < ADMISSION
View(stroke[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                  & (stroke$inhospital_stroke_cr == 0)
                  & (date(stroke$date_onset) > date(stroke$onset_dt))
                  & (date(stroke$date_onset) < date(stroke$admission_dt))), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
day(stroke$onset_dt[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                          & (stroke$inhospital_stroke_cr == 0)
                          & (date(stroke$date_onset) > date(stroke$onset_dt))
                          & (date(stroke$date_onset) < date(stroke$admission_dt)))
                    ]) <- day(stroke$date_onset[which((stroke$lkw_dt > stroke$onset_dt) 
                                                    & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                                                    & (stroke$inhospital_stroke_cr == 0) 
                                                    & (date(stroke$date_onset) > date(stroke$onset_dt))
                                                    & (date(stroke$date_onset) < date(stroke$admission_dt)))])
#### date: (ONSET_DT) < LKW ≤  DATE_ONSET = ADMISSION
library(hms)
View(stroke[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                  & (stroke$inhospital_stroke_cr == 0)
                  & (date(stroke$date_onset) > date(stroke$onset_dt))
                  & (date(stroke$date_onset) == date(stroke$admission_dt))
                  & (as_hms(stroke$onset_dt) < as_hms(stroke$admission_dt))), 
            c("ID","lkw_dt", "date_onset", "onset_dt", "arrival_dt", "admission_dt", "discharge_dt", "inhospital_stroke_cr")])
day(stroke$onset_dt[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                          & (stroke$inhospital_stroke_cr == 0)
                          & (date(stroke$date_onset) > date(stroke$onset_dt))
                          & (date(stroke$date_onset) == date(stroke$admission_dt))
                          & (as_hms(stroke$onset_dt) < as_hms(stroke$admission_dt)))
                    ]) <- day(stroke$date_onset[which((stroke$lkw_dt > stroke$onset_dt) & (day(stroke$lkw_dt)!= day(stroke$onset_dt))
                                                    & (stroke$inhospital_stroke_cr == 0)
                                                    & (date(stroke$date_onset) > date(stroke$onset_dt))
                                                    & (date(stroke$date_onset) == date(stroke$admission_dt))
                                                    & (as_hms(stroke$onset_dt) < as_hms(stroke$admission_dt)))])



# ====================
# AGE CORRECTION 
# ====================

################################################################################
# BASIC ASSUMPTION #
# - AGE_ORI is reliable
# - In the absence of AGE_ORI, AGE_COMP is the alternative
#       AGE_COMP is checked to be within a proper range
##         > summary(stroke$age_comp[is.na(stroke$age_ori)])
##          Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##          28.00   62.00   70.00   69.14   79.00   95.00     321 
################################################################################

stroke$age <- stroke$age_ori
stroke$age[is.na(stroke$age)] <- stroke$age_comp[is.na(stroke$age)]

# FOR THOSE STILL OUT_OF_RANGE: INELIGIBLE ====================================
