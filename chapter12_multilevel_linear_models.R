library(R2OpenBUGS)
library(arm)
library(dplyr)
library(lme4)

######################################

# radon in the United States
dat_cty <- read.delim("radon/cty.dat", header = TRUE, sep = ",")
dat_srrs <- read.delim("radon/srrs2.dat", header = TRUE, sep = ",")
head(dat_cty)
str(dat_cty)
head(dat_srrs)
str(dat_srrs)

dat_srrs_mn <- filter(dat_srrs, state == "MN")

log.radon <- log (ifelse (dat_srrs_mn$activity==0, .1, dat_srrs_mn$activity))
floor <- dat_srrs_mn$floor
county <- dat_srrs_mn$cntyfips

# complete-pooling
fit.1 <- lm(log.radon ~ floor)
summary(fit.1)

# no pooling. The "-1" in the formula removes the constant term (intercept), so that all 85 counties are included rather than one county being used as a baseline
fit.2 <- lm(log.radon ~ floor + factor(county) - 1)
summary(fit.2)

# lmer varying-intercept, no predictors
M0 <- lmer (log.radon ~ 1 + (1 | county))
display(M0)

# lmer varying-intercept with floor as predictor
M1 <- lmer (log.radon ~ floor + (1 | county))
display(M1)
coef(M1)
