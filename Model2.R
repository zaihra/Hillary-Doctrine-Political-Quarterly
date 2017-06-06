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

#####  Model 2 #########
summary(m2 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOPOL, data = HD1))

cov.m2 <- vcovHC(m2, type="HC0")
std.err2 <- sqrt(diag(cov.m2))
r.est2 <- cbind(Estimate= coef(m2), "Robust SE" = std.err2,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m2)/std.err2), lower.tail=FALSE),
               LL = coef(m2) - 1.96 * std.err,
               UL = coef(m2) + 1.96 * std.err)


round(r.est2,3)
with(m2, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#To present the regression results as incident rate ratios and 
#their standard errors, together with the confidence interval. 
#To compute the standard error for the incident rate ratios, 
#use the Delta method. To this end, we make use the function deltamethod implemented in R package msm.

s2 <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4),~ exp(x5),~ exp(x6),~ exp(x7),~ exp(x8)), coef(m2), cov.m2)
## exponentiate old estimates dropping the p values
rexp.est2 <- exp(r.est2[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est2[, "Robust SE"] <- s2

rexp.est2
round(rexp.est2,3)

#########################
#Robustness tests
###########################

#R5: Model 2, plus a dummy for Muslim majority countries (Muslim, column, N)
summary(r5 <- glm.nb(incidentsusvictims_L1~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOPOL+Muslim, data = HD1))
cov.r5 <- vcovHC(r5, type="HC0")
std.err5r <- sqrt(diag(cov.r5))
r.est5r <- cbind(Estimate= coef(r5), "Robust SE" = std.err5r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r5)/std.err5r), lower.tail=FALSE),
                 LL = coef(r5) - 1.96 * std.err5r,
                 UL = coef(r5) + 1.96 * std.err5r)


round(r.est5r,3)

#R6: Model 2, plus the measure for physical integrity (PhysicalIntregity, column O)
summary(r6 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOPOL+PhysicalIntegrity, data = HD1))
cov.r6 <- vcovHC(r6, type="HC0")
std.err6r <- sqrt(diag(cov.r6))
r.est6r <- cbind(Estimate= coef(r6), "Robust SE" = std.err6r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r6)/std.err6r), lower.tail=FALSE),
                 LL = coef(r6) - 1.96 * std.err6r,
                 UL = coef(r6) + 1.96 * std.err6r)


round(r.est6r,3)

#R7: Model 2, plus the measure of American alliances (Alliances, column P)


summary(r7 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOPOL+Alliances, data = HD1))
cov.r7 <- vcovHC(r7, type="HC0")
std.err7r <- sqrt(diag(cov.r7))
r.est7r <- cbind(Estimate= coef(r7), "Robust SE" = std.err7r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r7)/std.err7r), lower.tail=FALSE),
                 LL = coef(r7) - 1.96 * std.err7r,
                 UL = coef(r7) + 1.96 * std.err7r)


round(r.est7r,3)

# R4: Model 2, plus the count of terrorist incidents in the preceding year (incidentsusvictims, column Q).
oldw <- getOption("warn")
options(warn = -1)
summary(r8 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WOPOL+incidentsusvictims, data = HD1))
cov.r8 <- vcovHC(r8, type="HC0")
std.err8r <- sqrt(diag(cov.r8))
r.est8r <- cbind(Estimate= coef(r8), "Robust SE" = std.err8r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r8)/std.err8r), lower.tail=FALSE),
                 LL = coef(r8) - 1.96 * std.err8r,
                 UL = coef(r8) + 1.96 * std.err8r)


round(r.est8r,3)
options(warn = oldw)
