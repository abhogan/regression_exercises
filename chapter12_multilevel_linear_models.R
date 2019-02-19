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
dat_cty_mn <- filter(dat_cty, st == "MN")
dat_cty_mn <- unique(dat_cty_mn)
dat_cty_mn <- dat_cty_mn %>%
  group_by(ctfips) %>%
  slice(1L)

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

### Group-level predictors
## Get the county-level predictor
#srrs_fips <- dat_srrs_mn$stfips*1000 + dat_srrs_mn$cntyfips
#cty_fips <- 1000*dat_cty_mn$stfips + dat_cty_mn$ctfips
dat_cty_mn$log.uranium <- log (dat_cty_mn$Uppm)
dat_cty_mn <- dat_cty_mn[which(dat_cty_mn$ctfips %in% county), ]

sub <- select(dat_cty_mn, c(ctfips, log.uranium))
sub <- unique(sub) # remove duplicates


dat_srrs_mn <- left_join(dat_srrs_mn, sub, by = c("cntyfips" = "ctfips"))
log.uranium.full <- dat_srrs_mn$log.uranium


## Varying-intercept model w/ group-level predictors
M2 <- lmer (log.radon ~ floor + log.uranium.full + (1 | county))
display (M2)
coef(M2)
fixef(M2)
ranef(M2)

plot(dat_cty_mn$log.uranium, x$county$`(Intercept)`)
dat_cty_mn[1:89,]

y <- fixef(M2)[1] + fixef(M2)[3]*dat_cty_mn$log.uranium + ranef(M2)$county
plot(dat_cty_mn$log.uranium, y$`(Intercept)`)
