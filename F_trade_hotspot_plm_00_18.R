rm(list=ls())


F.trade <- read.csv("F_trade_hotspot_00_18_input.csv", header=T, stringsAsFactors=FALSE)

###!!!!! Option 1: all years !!!!!###
F.trade.sub <- F.trade[,c(-3,-4)]

F.trade.sub$F.prod.dom <- F.trade.sub$F.prod.tonne - F.trade.sub$F.exp.tonne

F.trade.sub2 <- F.trade.sub[,c(1,2,11,5,6,3,7,8,9,10)]


###!!!!! Option2 !!!!!###
### Select Year 2000-2008 & 2009-2018 ###
F.trade.sub <- F.trade[which(F.trade$Year > 2008),] # select >2008 or < 2009

### New variables ###
F.trade.sub <- F.trade.sub[,c(-3,-4)]

F.trade.sub$F.prod.dom <- F.trade.sub$F.prod.tonne - F.trade.sub$F.exp.tonne

F.trade.sub2 <- F.trade.sub[,c(1,2,11,5,6,3,7,8,9,10)]
###########!!!!!!!!!!!#############


######################
### Panel analysis ###
######################
library(Formula)
library(plm)
library(car)
library(stats)


########################
##### Fixed Effect #####
########################

# Food production for domestic supply
plm.fixed <- plm(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc), data=F.trade.sub2, index=c("ISO3", "Year"), model="within") 
summary(plm.fixed)
#fixef(plm.fixed) # Display the fixed effects (constants for each city)

# Food export
plm.fixed.E <- plm(log1p(F.exp.tonne) ~ log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc), data=F.trade.sub2, index=c("ISO3", "Year"), model="within")
summary(plm.fixed.E)

# Food import
plm.fixed.I <- plm(log1p(F.imp.tonne) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc), data=F.trade.sub2, index=c("ISO3", "Year"), model="within")
summary(plm.fixed.I)


#########################
##### Random Effect #####
#########################
# Food production for domestic supply
plm.random <- plm(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) , data=F.trade.sub2, index=c("ISO3", "Year"),  model="random")
summary(plm.random)
vif(plm.random)

# Food export
plm.random.E <- plm(log1p(F.exp.tonne) ~ log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc), data=F.trade.sub2, index=c("ISO3", "Year"),  model="random")
summary(plm.random.E)
vif(plm.random.E)

# Food import
plm.random.I <- plm(log1p(F.imp.tonne) ~ log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc), data=F.trade.sub2, index=c("ISO3", "Year"),  model="random")
summary(plm.random.I)
vif(plm.random.I)

##########################################
##### Testing for time-fixed effects #####
##########################################

# Food production for domestic supply
plm.fixed.time <- plm(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) +  factor(Year), data=F.trade.sub2, index=c("ISO3", "Year"), model="within")
summary(plm.fixed.time)

# If these p-value is <0.05 then use time-fixed effects.
# In this example, no need to use time-fixed effects.
pFtest(plm.fixed.time, plm.fixed)
plmtest(plm.fixed, c("time"), type=("bp"))

# Food export
plm.fixed.time.E <- plm(log1p(F.exp.tonne) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(Year), data=F.trade.sub2, index=c("ISO3", "Year"), model="within")
summary(plm.fixed.time.E)

# If these p-value is <0.05 then use time-fixed effects.
# In this example, no need to use time-fixed effects.
pFtest(plm.fixed.time.E, plm.fixed.E)
plmtest(plm.fixed.E, c("time"), type=("bp"))

# Food import
plm.fixed.time.I <- plm(log1p(F.imp.tonne) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(Year), data=F.trade.sub2, index=c("ISO3", "Year"), model="within")
summary(plm.fixed.time.I)

# If these p-value is <0.05 then use time-fixed effects.
# In this example, no need to use time-fixed effects.
pFtest(plm.fixed.time.I, plm.fixed.I)
plmtest(plm.fixed.I, c("time"), type=("bp"))

#############
#### OLS ####
#############
ols <- lm(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc), data = F.trade.sub2)
summary(ols)


###################
### OLS - dummy ###
###################
ols.dum <- lm(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(ISO3) -1, F.trade.sub2)
summary(ols.dum)


##########################
## fixed effect or ols? ##
##########################
pFtest(plm.fixed, ols)

# test for individual fixed effects present in the pooled model
#https://thetarzan.wordpress.com/2011/05/27/surviving-graduate-econometrics-with-r-advanced-panel-data-methods-4-of-8/
# Failure to reject the null hypothesis implies that you will have more efficient estimates using OLS
plmtest(plm.fixed, effect = "time")

####################################
## fixed effect or random effect? ##
####################################

# if the p-value is significant (for example<0.05) then use fixed effects, if not use random effects 
phtest(plm.fixed, plm.random)
phtest(plm.fixed.E, plm.random.E)
phtest(plm.fixed.I, plm.random.I)

# A possible reason might be that your dummies do not vary over time. In this case, the fixed effects estimator and first differencing will remove such variables from the model. The reason is that these estimators cannot identify variables that do not vary over time because those variables will be eliminated together with the unobserved fixed effects.
# http://stats.stackexchange.com/questions/24680/panel-data-model-estimation-with-dummy-variables


########################################################################
## Testing for random effects: Breusch-Pagan Lagrange multiplier (LM) ##
########################################################################
# Regular OLS (pooling model) using plm
plm.random.pool <- plm(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(Year), data=F.trade.sub2, index=c("ISO3", "Year"), model="pooling")
summary(plm.random.pool)

plm.random.pool.E <- plm(log1p(F.exp.tonne) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(Year), data=F.trade.sub2, index=c("ISO3", "Year"), model="pooling")
summary(plm.random.pool.E)

plm.random.pool.I <- plm(log1p(F.imp.tonne) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(Year), data=F.trade.sub2, index=c("ISO3", "Year"), model="pooling")
summary(plm.random.pool.I)

# Breusch-Pagan Lagrange Multipler for random effect. Null is no panel effect (i.e. OLS better)
# If the p-value is not significant (p>0.05), we failed to reject the null and conclude that random effects is not appropriate.
# This is, no evidence of significant differences across countries, therefore you can run a simple OLS regression. (p>0.05)
plmtest(plm.random.pool, type=c("bp"))
plmtest(plm.random.pool.E, type=c("bp"))
plmtest(plm.random.pool.I, type=c("bp"))

#################################
## Testing for cross-sectional dependence/contemporaneous correlation: using Breusch-Pagan LM test for independence and Pasaran CD test ##
#################################
# According to Baltagi, cross-sectional dependence is a problem in macro panels with long time series. This is not much of a problem in micro panels (few years and large number of cases)
# The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals across entities are not correlated. B-P/LM and Pasaran CD (cross-sectional dependence) tests are used to test whether the residuals are correlated across entities*. Cross-sectional dependence can lead to bias in tests results (also called contemporaneous correlation).
# If the p-value is not significant (p>0.05), then there is no cross-sectional dependence.
pcdtest(plm.fixed, test=c("lm"))
pcdtest(plm.fixed.E, test=c("lm"))
pcdtest(plm.fixed.I, test=c("lm"))

pcdtest(plm.fixed, test=c("cd"))
pcdtest(plm.fixed.E, test=c("cd"))
pcdtest(plm.fixed.I, test=c("cd"))

####################################
## Testing for serial correlation ##
####################################
# Serial correlation tests apply to macro panels with long time series. Not a problem in micro panels (with very few years). The null is that there is not serial correlation.
# if the p-value is not significant (p>0.05), there is no serial correlation.
pbgtest(plm.fixed)
pbgtest(plm.fixed.E)
pbgtest(plm.fixed.I)

pbgtest(plm.random)
pbgtest(plm.random.E)
pbgtest(plm.random.I)


#########################################
## Testing for unit roots/stationary ####
#########################################
# The Dickey-Fuller test to check for stochastic trends. The null hypothesis is that the series has a unit root (i.e. non-stationary). If unit root is present you can take the first difference of the variable
#qing.subset <- qing[,c(2, 3, 4, 6, 18, 38, 10, 31, 34)]
Panel.set <- plm.data(F.trade.sub2, index=c("ISO3", "Year"))
library(tseries)
#if the p-value is <0.05, then no unit roots present.
adf.test(Panel.set$Year, k=2)


###################################
## Testing for heteroskeasticity ##
###################################
# The null hypothesis for the Breusch-Pagan test is homoskedasticity
library(lmtest)

# if the p-value is significant (p<0.05), there is presence of heteroskedasiticity.
bptest(log1p(F.prod.dom) ~  log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(ISO3), data = F.trade.sub2, studentize=F)

bptest(log1p(F.exp.tonne) ~ log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(ISO3), data = F.trade.sub2, studentize=F)

bptest(log1p(F.imp.tonne) ~ log1p(hotspot_pct) + log1p(Diet.supply.percent) + log1p(Ag.area.km2) + log1p(Pop.1000) + log1p(GDP.pc) + factor(ISO3), data = F.trade.sub2, studentize=F)

## If hetersokedaticity is detected you can use robust covariance matrix to account for it. See the following sector.

########################################################
## Controlling for heteroskedasticity: Random effects ##
########################################################
# The number of tourists shows a presence of heteroskedasitcity. We need to control that.
coeftest(plm.random)
coeftest(plm.random, vcovHC) # used in this research #plm.random; plm.random.E; plm.random.I
coeftest(plm.random, vcovHC(plm.random, type = "HC3")) #HC0-3, 
coeftest(plm.random, vcovHC(plm.random, type = "HC3", method="white1")) #white1

# The vcovHC function estimates three heteroskedasticity-consistent covariance estimators:
# [method]
## "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects
## "white2' - is "white1" restricted to a common variance within groups. Recommond for random effects
## "arellano" - both heteroskedasticity and serial correction. Recommended for fixed effects.
# [type]
## HC0 - heteroskedasticity consistent. The default.
## HC1, HC2, HC3 - Recommended for small samples. HC3 gives less weight to influential observations.
## HC4 - small samples with influential observations
## HAC - heteroskedasticity and autocorrelation consistent (type ?vcovHAC for more details)

# do not need this for our models (if heteroskeasticity test, p>0.05)
