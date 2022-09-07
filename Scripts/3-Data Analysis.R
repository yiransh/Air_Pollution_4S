################################################################################
# 00 DATA PREPRARTION ----------------------------------------------------------
################################################################################

# EXPAND DATA IN A CASE-CROSSOVER FORMAT
# SET ID AS STUDY STRATUM
airstr$id <- as.factor(airstr$id)

# SET THE DEFAULT ACTION FOR MISSING DATA TO na.exclude
# (MISSING EXCLUDED IN ESTIMATION BUT RE-INSERTED IN PREDICTION/RESIDUALS)
options(na.action="na.exclude")

# SCALE EXPOSURE AND LAG DAYS
# ALREADY IN SCALED UNITS

# SUBGROUP
airstr_ais <- airstr[which(airstr$dx == "AIS"),]
laa <- airstr_ais[which(airstr_ais$toast == "LAA"), ]
ce <- airstr_ais[which(airstr_ais$toast == "CE"), ]
svd <- airstr_ais[which(airstr_ais$toast == "SVD"), ]

################################################################################
# 01 CROSSBASIS FUNCTION -------------------------------------------------------
################################################################################

# LOAD THE PACKAGES
library(dlnm) ; library(splines) ; library(MASS) ; library(tsModel); library(survival)

# DEFINITION OF THE CROSS-BASIS
# - SPECIFICATION PARAMETERS OF THE EXPOSURE-RESPONSE DIMENSION OF THE CROSS-BASIS
argvar_pm25 <- list(fun="lin")
argvar_temp <- list(fun="ns", knots = quantile(airstr$temp,c(10,25,90)/100, na.rm=T),
                    Bound=range(airstr$temp,na.rm=T))
argvar_rh <- list(fun="ns", knots = equalknots(airstr$rh,nk=3),
                   Bound=range(airstr$rh,na.rm=T))

# - SPECIFICATION PARAMETERS OF THE LAG-ASSOCIATION DIMENSION OF THE CROSS-BASIS
# Definition of the maximum lag, that is, 14 days
maxlag <- 14
lag_t <- 6
lag_rh <- 6
# arglag: main model, it fits a B-cubic spline with four internal knots (empircal) 
#   equally-spaced in the log-scale. (nk denotes number of knots)
arglag <- list(fun="ns", df=3)
arglag_pm25 <- list(fun="ns", knots=logknots(maxlag,fun="ns", nk=3))
arglag_t <- list(fun="ns", knots=logknots(lag_t,fun="ns", nk=3))
arglag_rh <- list(fun="ns", knots=logknots(lag_rh,fun="ns", nk=3))

# - CREATE CROSSBASIS OBJECTS
cb_pm25 <- crossbasis(airstr$pm25, maxlag, argvar_pm25, arglag_pm25)
cb_temp <- crossbasis(airstr$temp, lag_t, argvar_temp, arglag_t)
cb_rh <- crossbasis(airstr$rh, lag_rh, argvar_rh, arglag_rh)

################################################################################
# 02 MODEL FIT AND PREDICTION --------------------------------------------------
################################################################################

# CREATE THE CONDITIONAL LOGISTIC MODEL INCORPORATING COVARIATES
# - PM25:       crossbasis   (ER linear; LAG df=3, maxlag = 21)
# - TEMP10:     crossbasis   (ER ns df=3;LAG df=3, maxlag = 6)
# - DPT10:      crossbaiss   (ER ns df=3;LAG df=3, maxlag = 6)
# - HOLIDAY:    dummy/linear
# - STRATUM:    linear
model <- clogit(status ~ cb_pm25 + cb_temp + cb_rh +
                  strata(id) + as.factor(holiday),
                data = airstr,
                na.action = na.exclude)

# SET UP RANGE AND REFERENT FOR EXPOSURE-RESPONSE RELATIONSHIP
# define extreme air quality (PM2.5 concentration) and remove it from analysis
p1 <-round(quantile(airstr$pm25,probs=c(0.01),na.rm=TRUE),0)
p25 <-round(quantile(airstr$pm25,probs=c(0.25),na.rm=TRUE),0)
p99 <-round(quantile(airstr$pm25,probs=c(0.99),na.rm=TRUE),0)
# extract model coefficient within the defined PM2.5 range
pred1 <- crosspred(cb_pm25, model, cen = p1, at=c(p1:p99),cumul=TRUE) 
# choose a referent PM2.5 level as the one corresponding to the lowest incidence
#   risk in the exposure-response curve
cen <- pred1$predvar[which.min(pred1$allRRfit)]  
pred1 <- crosspred(cb_pm25, model,cen = cen, at=c(p1:p99),cumul=TRUE)


################################################################################
# 03 RESULTS AND PLOTS ---------------------------------------------------------
################################################################################

library(RColorBrewer)

# 3-D PLOT
plot(pred1, zlab="RR",xlab="PM2.5")

# PLOT EXPOSURE-RESPONSE RELATIONSHIP
plot(pred1, lag = maxlag, cumul = TRUE,
     lwd=1.8,cex.axis=1.30,ylim = c(1,1.5),cex.main=1.8,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])),
     xlab="PM2.5(µg/m3)",ylab="Odds Ratio",
     main = "Exposure-Response Curve")
## OVERALL EFFECT (SIMPLE)
plot(pred1,"overall",xlab="PM2.5",
     lwd=1.8,cex.axis=1.30,cex.main=1.2,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])),
     main="Overall Effect of PM2.5 on Stroke Admission 2017-2021")


# SLICES
percentiles <- round(quantile(airstr$pm25,c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE),0)
plot(pred1, var=percentiles, lag=c(0,5,7,10,14),
     lwd=1.8,cex.axis=1.30,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])))   # modification is needed

## RR AT CHOSEN PERCENTILES VERSUS CEN (9µg/m3) (AND 95%CI)
pred1$allRRfit[as.character(percentiles)]
cbind(pred1$allRRlow,pred1$allRRhigh)[as.character(percentiles),]

## PLOT LAG-RESPONSE RELATIONSHIP AT P25
plot(pred1, "slices" ,ci="area",var = p25,
     lwd=1.8, ylim = c(0.9,1.2),cex.axis=1.30,cex.main=1.40,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])),
     xlab="Lag days",ylab="Odds Ratio",main = "Lag-Response Curve") 

round(with(pred1,cbind(cumRRfit,cumRRlow,cumRRhigh)["30",]),3)


