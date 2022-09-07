# LOAD THE PACKAGES
library(dlnm) ; library(splines) ; library(MASS) ; library(tsModel); library(survival)
library(RColorBrewer)

################################################################################
# 00 DATA PREPRARTION ----------------------------------------------------------
################################################################################

# POPULATION
# - TOTAL (AIS + TIA)  airstr
# - AIS                airstr_ais
# - LAA                laa
# - CE                 ce
# - SVD                svd
pop <- airstr
##[which(airstr$year %in% c("2017", "2018", "2019")), ]

# INTERVENTION/CONTROL
# - AIR POLLUTANTS: pm25, pm10, o3, no2, so3, co
ap <- "pm25"

# OUTCOME
# - STATUS

# PARAMATERS
# - POLLUTANTS: crossbasis   (ER linear; LAG df=3, maxlag = 14)
# - TEMP10:     crossbasis   (ER ns df=4;LAG df=3, maxlag = 6)
# - DPT10:      crossbaiss   (ER ns df=3;LAG df=3, maxlag = 6)
# - HOLIDAY:    dummy/linear
# - STRATUM:    linear
argvar_ap <- list(fun="lin")  ## test linearity: fun="ns", nk=2
argvar_temp <- list(fun="ns", knots = quantile(pop$temp,c(10,50,90)/100, na.rm=T),
                    Bound=range(pop$temp,na.rm=T))
argvar_rh <- list(fun="ns", knots = equalknots(pop$rh,nk=3),
                  Bound=range(pop$rh,na.rm=T))

lag_ap <- 14
lag_t <- 6
lag_rh <- 6
arglag_ap <- list(fun="ns", knots=logknots(lag_ap,fun="ns", nk=3))
arglag_t <- list(fun="ns", knots=logknots(lag_t,fun="ns", nk=2))
arglag_rh <- list(fun="ns", knots=logknots(lag_rh,fun="ns", nk=2))

################################################################################
# 01 MODEL FIT AND PREDICTION --------------------------------------------------
################################################################################

cb_ap <- crossbasis(pop[, which(names(pop)==ap)], lag_ap, argvar_ap, arglag_ap)
cb_temp <- crossbasis(pop$temp, lag_t, argvar_temp, arglag_t)
cb_rh <- crossbasis(pop$rh, lag_rh, argvar_rh, arglag_rh)

model <- clogit(status ~ cb_ap + cb_temp + cb_rh +
                  strata(id) + as.factor(holiday),
                data = pop,
                na.action = na.exclude)

p1 <-round(quantile(pop[, which(names(pop)==ap)],probs=c(0.01),na.rm=TRUE),0)
p99 <-round(quantile(pop[, which(names(pop)==ap)],probs=c(0.99),na.rm=TRUE),0)
pred1 <- crosspred(cb_ap, model, cen = p1, at=c(p1:p99),cumul=TRUE) 
cen <- pred1$predvar[which.min(pred1$allRRfit)]  
pred1 <- crosspred(cb_ap, model,cen = cen, at=c(p1:p99),cumul=TRUE)

################################################################################
# 02 PLOTS AND OUTPUT ----------------------------------------------------------
################################################################################

# 3-D PLOT
plot(pred1, zlab="OR",xlab="NO2")

# PLOT EXPOSURE-RESPONSE RELATIONSHIP
plot(pred1, lag = lag_ap, cumul = TRUE,
     lwd=1.8,cex.axis=1.30,ylim = c(0.99,1.2),cex.main=1.8,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])),
     xlab="PM2.5(µg/m3)",ylab="Odds Ratio",
     main = "Exposure-Response Curve")

# SLICES
percentiles <- round(quantile(pop[, which(names(pop)==ap)],c(0.10,0.25,0.50,0.75,0.90), na.rm = TRUE),0)
plot(pred1, var=percentiles, lag=c(0,1,2,3,4),
     lwd=1.8,cex.axis=1.30,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])))   # modification is needed

AIC(model)

## RR AT CHOSEN PERCENTILES VERSUS CEN (8µg/m3) (AND 95%CI)
pred1$allRRfit[as.character(percentiles)]
cbind(pred1$allRRlow,pred1$allRRhigh)[as.character(percentiles),]

## PLOT LAG-RESPONSE RELATIONSHIP AT P25
plot(pred1, "slices" ,ci="area",var = p25,
     lwd=1.8, ylim = c(0.9,1.2),cex.axis=1.30,cex.main=1.40,
     col=(brewer.pal(9,"Blues")[c(8)]),
     ci.arg=list(col=(brewer.pal(9,"Blues")[c(3)])),
     xlab="Lag days",ylab="Odds Ratio",main = "Lag-Response Curve") 

round(with(pred1,cbind(cumRRfit,cumRRlow,cumRRhigh)["30",]),3)
