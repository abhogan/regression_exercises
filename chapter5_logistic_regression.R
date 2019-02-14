
# wells in Bangladesh
dat <- read.delim("wells.dat", header = TRUE, sep = " ")
head(dat)
str(dat)

dat$dist100 <- dat$dist/100
xax <- seq(0,3.5,0.01)

fit.1 <- glm (dat$switch ~ dat$dist100, family=binomial(link="logit"))
display(fit.1)


plot(dat$dist100, jitter.binary(dat$switch), col = "red")
lines(xax, invlogit(fit.1$coefficients[1] + fit.1$coefficients[2] * xax))

# add a second input variable
fit.2 <- glm (dat$switch ~ dat$dist100 + dat$arsenic, family=binomial(link="logit"))
display(fit.2)
plot(dat$dist100, jitter.binary(dat$switch), col = "red" )
lines(xax, invlogit(fit.2$coefficients[1] + fit.2$coefficients[2] * xax + fit.2$coefficients[3]*0.5), col = "blue")
lines(xax, invlogit(fit.2$coefficients[1] + fit.2$coefficients[2] * xax + fit.2$coefficients[3]*1), col = "green")

xax = seq(0,10,0.1)
plot(dat$arsenic, jitter.binary(dat$switch), col = "red" )
lines(xax, invlogit(fit.2$coefficients[1] + fit.2$coefficients[2] * 0.5 + fit.2$coefficients[3]*xax), col = "blue")
lines(xax, invlogit(fit.2$coefficients[1] + fit.2$coefficients[2] * 1 + fit.2$coefficients[3]*xax), col = "green")

# add an interaction term
fit.4 <- glm (dat$switch ~ dat$dist100 + dat$arsenic + dat$dist100:dat$arsenic, family=binomial(link="logit"))
display(fit.4)

mean_dist100 <- mean(dat$dist100)
mean_arsenic <- mean(dat$arsenic)

# probability of switching for average distance and arsenic level
invlogit(fit.4$coefficients[1] + mean_dist100 * fit.4$coefficients[2] + mean_arsenic * fit.4$coefficients[3] + mean_dist100 * mean_arsenic * fit.4$coefficients[4])

# centering the inputs
dat$cent_dist100 <- dat$dist100 - mean_dist100
dat$cent_arsenic <- dat$arsenic - mean_arsenic

fit.5 <- glm(dat$switch ~ dat$cent_dist100 + dat$cent_arsenic + dat$cent_dist100:dat$cent_arsenic, family=binomial(link="logit"))
display(fit.5)
# probability of switching for average distance and arsenic level
invlogit(fit.5$coefficients[1])

