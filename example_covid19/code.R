# -----------------------------
# Teaching example:  Covid19 data
# -----------------------------

rm(list=ls())

library(tableone)
library(rms)
library(MASS)
library(RColorBrewer)


# Load data 
load("data_covid19.RData")


# Data visualization
ggplot(data_covid, aes(y=sqrt(GGO), x=sqrt(consolidation), group=severe)) +
  geom_point(aes(shape=severe, color=severe), size=3) +
  scale_x_continuous(expression(sqrt("Consolidation")), limits=c(-0,5.5), breaks = seq(0,5.5,1)) +
  scale_y_continuous(expression(sqrt("GGO")), limits=c(-0,5.5), breaks = seq(0,5.5,1)) +
  scale_color_brewer(palette="Dark2", "Disease progression", label=c("Moderate","Severe")) +
  scale_shape_manual(values=c(16,17),"Disease progression", label=c("Moderate","Severe")) +
  theme_bw() +
  theme(legend.position = c(0.8,0.2), panel.border = element_blank(), axis.line = element_line(), 
        text = element_text(size=16),legend.text=element_text(size=14), legend.title = element_text(size=14),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"), axis.text = element_text(size=14))

# Redundancy analysis
redun(~GGO+consolidation, type="adjusted", data=data_covid)

# Correlation
cor(data_covid$GGO, data_covid$consolidation)


#----------------------------- Model building ------------------------------------

fit.res1 <- glm(severe~GGO, family="binomial", data=data_covid)
summary(fit.res1)
exp(cbind(coef(fit.res1), confint(fit.res1)))
rcorr.cens(fit.res1$fitted.values,data_covid$severe)[1]
confint(fit.res1)

fit.res2 <- glm(severe~consolidation, family="binomial", data=data_covid)
summary(fit.res2)
exp(cbind(coef(fit.res2), confint(fit.res2)))
rcorr.cens(fit.res2$fitted.values, data_covid$severe)[1]
confint(fit.res2)

fit.res3 <- glm(severe~GGO+consolidation, family="binomial", data=data_covid)
summary(fit.res3)
vif(fit.res3)
exp(cbind(coef(fit.res3), confint(fit.res3)))
rcorr.cens(fit.res3$fitted.values,data_covidg$severe)[1]

