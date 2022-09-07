# BY YEAR =================================================================

# By HOSPITAL NAME ========================================================
table(stroke$hospt_name,stroke$toast)
write.csv(table(stroke$hospt_name,stroke$toast), "toast_by_hospital_name.csv")
write.csv(tapply(is.na(stroke$toast), stroke$hospt_name, sum, simplify = T), "by_hospital_name_na.csv")
## 中山、瑞金: LAA > 70%, Unkn < 8%  
## 病例数多的医院往往LAA > 60%

# BY HOSPITAL CLASS ========================================================
table(stroke$hospt_class, stroke$toast)
tapply(is.na(stroke$toast), stroke$hospt_class, sum, simplify = T)
## 除了社会办医缺失多外，肉眼没有显著差异