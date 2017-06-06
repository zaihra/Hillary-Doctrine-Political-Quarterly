require(ggplot2)
require(sandwich)
require(msm)
require(foreign)
require(MASS)
require(gdata)
library(dplyr)
#getwd()
HD<-read.csv("HD.csv")
#head(HD)
#attributes(HD)
dim(HD)

#subsetting for our time frame 1981-2006

HD1<- subset(HD, YEAR >= 1981 & YEAR <2006)
#head(HD1)
dim(HD1)
#MODEL3
summary(m3 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOSOC, data = HD1))
cov.m3 <- vcovHC(m3, type="HC0")
std.err3 <- sqrt(diag(cov.m3))
r.est3 <- cbind(Estimate= coef(m3), "Robust SE" = std.err3,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(m3)/std.err3), lower.tail=FALSE),
                LL = coef(m3) - 1.96 * std.err3,
                UL = coef(m3) + 1.96 * std.err3)


round(r.est3,3)
with(m3, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#To present the regression results as incident rate ratios and 
#their standard errors, together with the confidence interval. 
#To compute the standard error for the incident rate ratios, 
#use the Delta method. To this end, we make use the function deltamethod implemented in R package msm.

s3 <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4),~ exp(x5),~ exp(x6),~ exp(x7),~ exp(x8)), coef(m3), cov.m3)
## exponentiate old estimates dropping the p values
rexp.est3 <- exp(r.est3[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est3[, "Robust SE"] <- s3

rexp.est3
round(rexp.est3,3)

#########################
#Robustness tests
###########################

#R9: Model 3, plus a dummy for Muslim majority countries (Muslim, column, N)
summary(r9 <- glm.nb(incidentsusvictims_L1~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOSOC+Muslim, data = HD1))
cov.r9 <- vcovHC(r9, type="HC0")
std.err9r <- sqrt(diag(cov.r9))
r.est9r <- cbind(Estimate= coef(r9), "Robust SE" = std.err9r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r9)/std.err9r), lower.tail=FALSE),
                 LL = coef(r9) - 1.96 * std.err9r,
                 UL = coef(r9) + 1.96 * std.err9r)


round(r.est9r,3)

#R10: Model 3, plus the measure for physical integrity (PhysicalIntregity, column O)
summary(r10 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOSOC+PhysicalIntegrity, data = HD1))
cov.r10 <- vcovHC(r10, type="HC0")
std.err10r <- sqrt(diag(cov.r10))
r.est10r <- cbind(Estimate= coef(r10), "Robust SE" = std.err10r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r10)/std.err10r), lower.tail=FALSE),
                 LL = coef(r10) - 1.96 * std.err10r,
                 UL = coef(r10) + 1.96 * std.err10r)


round(r.est10r,3)

#R11: Model 3 plus the measure of American alliances (Alliances, column P)


summary(r11 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOSOC+Alliances, data = HD1))
cov.r11 <- vcovHC(r11, type="HC0")
std.err11r <- sqrt(diag(cov.r11))
r.est11r <- cbind(Estimate= coef(r11), "Robust SE" = std.err11r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r11)/std.err11r), lower.tail=FALSE),
                 LL = coef(r11) - 1.96 * std.err11r,
                 UL = coef(r11) + 1.96 * std.err11r)


round(r.est11r,3)
# R12: Model 1, plus the count of terrorist incidents in the preceding year (incidentsusvictims, column Q).
oldw <- getOption("warn")
options(warn = -1)
summary(r12 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOSOC+incidentsusvictims, data = HD1))
cov.r12 <- vcovHC(r12, type="HC0")
std.err12r <- sqrt(diag(cov.r12))
r.est12r <- cbind(Estimate= coef(r12), "Robust SE" = std.err12r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r12)/std.err12r), lower.tail=FALSE),
                 LL = coef(r12) - 1.96 * std.err12r,
                 UL = coef(r12) + 1.96 * std.err12r)


round(r.est12r,3)
options(warn = oldw)
