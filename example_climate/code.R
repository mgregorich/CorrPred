# -----------------------------
# Teaching example:  climate change data
# -----------------------------

rm(list=ls())

library(dslabs)
library(tidyverse)
library(vegan)
library(sandwich)
library(Hmisc)
library(kableExtra)
library(splines)
library(effects) 
library(lmtest)
library(rms)
library(mctest)

# Load data from R-package dslabs
data(temp_carbon)
temp_carbon <- temp_carbon[order(temp_carbon$year),]
data_carbon <- temp_carbon[temp_carbon$year > 1879 & temp_carbon$year <2015, c("temp_anomaly", "carbon_emissions", "year")]


# Set colorblind-friendly color palette
colorblind_palette <- c("black", "#E69F00", "#56B4E9", "#009E73",
                        "#CC79A7", "#F0E442", "#0072B2", "#D55E00")


# Line plot of anthropogenic carbon emissions over 250+ years
temp_carbon %>%
  select(Year = year, Global = temp_anomaly, Land = land_anomaly, Ocean = ocean_anomaly) %>%
  gather(Region, Temp_anomaly, Global:Ocean) %>%
  ggplot(aes(Year, Temp_anomaly, col = Region)) +
  geom_line(size = 1) +
  theme_bw() +
  geom_hline(aes(yintercept = 0), col = colorblind_palette[8], lty = 2) +
  geom_label(aes(x = 2002, y = -.08), col = colorblind_palette[8], 
             label = "20th century mean", size = 3) +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  scale_color_manual(values = colorblind_palette) 


# Redundancy analysis
redun(~data_carbon$year+data_carbon$carbon_emissions)

# Correlation matrix
cor(data_carbon)
plot(data_carbon$carbon_emissions,data_carbon$temp_anomaly)

# Test for autocorrelation function of temperature nomalies
acf(data_carbon$temp_anomaly,main='')


#----------------------------- Model building ------------------------------------
# Simple models with HAC estimator
# define a covariance function
# the following estimator is defined according to section 3.2 in 
# https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf
# (for the function parameters, see the paragraph on p 7)
vcov_function <- function(x,...) {
  kernHAC(x, order.by = ~ year, 
          data = data_carbon,
          bw = bwAndrews, 
          kernel = "Quadratic Spectral", 
          approx = "AR(1)")
}

# Build the univariable and the multivariable linear models
lm.res1 <- (lm(temp_anomaly ~ year, data=data_carbon))
coeftest(lm.res1, vcov. = kernHAC)
confint(coeftest(lm.res1, vcov. = kernHAC))

lm.res2 <- (lm(temp_anomaly ~ carbon_emissions, data=data_carbon))
coeftest(lm.res2, vcov. = kernHAC)
confint(coeftest(lm.res2, vcov. = kernHAC))

lm.res3 <- (lm(temp_anomaly ~ year + carbon_emissions, data=data_carbon))
coeftest(lm.res3, vcov. = kernHAC)
confint(coeftest(lm.res3, vcov. = kernHAC))
vif(lm.res3)

# Condition indices and variance-decomposition proportions
eigprop(lm.res3)


# Model selection including non-linear relationships
fit.lm <- list()
var_year<- c('year', paste0('ns(year,df=',3:5,')'))
var_carbon <-c('carbon_emissions', paste0('ns(carbon_emissions,df=',3:5,')'))
var_combs <- expand.grid(var_year, var_carbon)
lm_formula <- c(var_year,var_carbon, apply(var_combs, 1, paste, collapse="+"))

fit.lm <- lapply(lm_formula, function(x) lm(as.formula(paste0("temp_anomaly~", x)),  data=data_carbon))
eff.time <- c(var_year,rep('-',4), paste0(var_combs[,1]))
eff.carbon <- c(rep('-',4), var_carbon, paste0(var_combs[,2]))

fit.lm.stats <- data.frame(t(sapply(1:length(fit.lm), function(x) c(paste0('Model ',x),
                                                                    summary(fit.lm[[x]])$df[1], 
                                                                    round(AIC(fit.lm[[x]]),2),
                                                                    round(extractAIC(fit.lm[[x]], k=2)[2],2),
                                                                    round(BIC(fit.lm[[x]]),2), 
                                                                    round(summary(fit.lm[[x]])$adj.r.squared,2), 
                                                                    round(sum((data_carbon$temp_anomaly-fit.lm[[x]]$fitted.values)^2),2),
                                                                    round(sum(fit.lm[[x]]$residuals^2),2)))))
fit.lm.stats <- cbind(fit.lm.stats[,1], eff.time,eff.carbon, fit.lm.stats[,2:ncol(fit.lm.stats)])
colnames(fit.lm.stats) <- c('Model','Time effect','Carbon effect','df', 'AIC','AICc', 'BIC', 'adj R2', 'MSE', 'SSE')
fit.lm.stats


# Final model according to AIC
fit.final <- lm(temp_anomaly ~ year + ns(carbon_emissions,df=5), data=data_carbon)
coeftest(fit.final, vcov. = kernHAC)
confint(coeftest(fit.final, vcov. = kernHAC))

# Effect plots
eff.temp <- allEffects(fit.final)
as.data.frame(eff.temp$year)
as.data.frame(eff.temp$`ns(carbon_emissions,df=5)`)

plot(allEffects(fit.final)[1], main="", xlab=c("Year"), ylab="Temperature anomaly [C]")
plot(allEffects(fit.final)[2], main="", xlab=c("Carbon emissions [mil. of t]"), ylab="Temperature anomaly [C]")


