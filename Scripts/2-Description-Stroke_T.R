###############################################################################
# 1 TABLEONE: STROKE VARIABLES  ----------------------------------------------
###############################################################################

# TARGET DATASET: stroke_t

library(tableone)
stroke_t$dur[which(stroke_t$inhospital_stroke_cr == 1)] <- NA
stroke_t$precovid <- ifelse(year(stroke_t$date) %in% c("2016", "2017", "2018", "2019"), 1, 0)
listvars_stroke <- c("age", "gender", "inhospital_stroke_cr", "hospt_class", "dur", "premrs", 
                     "htn", "dm", "ci", "mi", "ich", "af_vhd", 
                     "dyslipid", "bleeding", "dementia", "COPD", 
                     "anti-htn", "anti-dm", "lip-reduc", "anti-coag", "toast")
catvars_stroke <- c("gender", "inhospital_stroke_cr", "hospt_class",
                    "htn", "dm", "ci", "mi", "ich", "af_vhd", 
                    "dyslipid", "bleeding", "dementia", "COPD", 
                    "anti-htn", "anti-dm", "lip-reduc", "anti-coag", "toast")


# TEST FOR NORMALITY
stroke_t$premrs <- as.numeric(stroke_t$premrs)
shapiro.test(stroke_t$age)    # n>5000
shapiro.test(stroke_t$premrs) # n>5000
shapiro.test(stroke_t$dur)    # n>5000

###############################################################################
# 2 TABLEONE: EXPORT OUTPUT  -------------------------------------------------
###############################################################################

table1_stroke_total <- CreateTableOne(vars = listvars_stroke, data = stroke_t, 
                                      factorVars = catvars_stroke)
table1_stroke_by_dx <- CreateTableOne(vars = listvars_stroke, data = stroke_t, 
                                      factorVars = catvars_stroke, strata = c('dx'))
write.csv(print(table1_stroke_total), file = "Output/Description_Stroke1.csv")
write.csv(print(table1_stroke_by_dx), file = "Output/Description_Stroke2.csv")

table1_stroke_covid <- CreateTableOne(vars = listvars_stroke, data = stroke_t, 
                                      factorVars = catvars_stroke, strata = c('precovid'))
write.csv(print(table1_stroke_covid), file = "Output/Description_Stroke_covid.csv")
