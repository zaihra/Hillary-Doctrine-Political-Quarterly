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

################Model1#####

summary(m1 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON, data = HD1))

cov.m1 <- vcovHC(m1, type="HC0")
std.err1 <- sqrt(diag(cov.m1))
r.est1 <- cbind(Estimate= coef(m1), "Robust SE" = std.err1,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err1), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err1,
               UL = coef(m1) + 1.96 * std.err1)

r.est1
round(r.est1,3)
with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#To present the regression results as incident rate ratios and 
#their standard errors, together with the confidence interval. 
#To compute the standard error for the incident rate ratios, 
#use the Delta method. To this end, we make use the function deltamethod implemented in R package msm.

s1 <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4),~ exp(x5),~ exp(x6),~ exp(x7),~ exp(x8)), coef(m1), cov.m1)
## exponentiate old estimates dropping the p values
rexp.est1 <- exp(r.est1[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est1[, "Robust SE"] <- s1


round(rexp.est1,3)
#########################
#Robustness tests
###########################

#R1: Model 1, plus a dummy for Muslim majority countries (Muslim, column, N)
summary(r1 <- glm.nb(incidentsusvictims_L1~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON+Muslim, data = HD1))
cov.r1 <- vcovHC(r1, type="HC0")
std.err1r <- sqrt(diag(cov.r1))
r.est1r <- cbind(Estimate= coef(r1), "Robust SE" = std.err1r,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(r1)/std.err1r), lower.tail=FALSE),
                LL = coef(r1) - 1.96 * std.err1r,
                UL = coef(r1) + 1.96 * std.err1r)


round(r.est1r,3)

#R2: Model 1, plus the measure for physical integrity (PhysicalIntregity, column O)
summary(r2 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON+PhysicalIntegrity, data = HD1))
cov.r2 <- vcovHC(r2, type="HC0")
std.err2r <- sqrt(diag(cov.r2))
r.est2r <- cbind(Estimate= coef(r2), "Robust SE" = std.err2r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r2)/std.err2r), lower.tail=FALSE),
                 LL = coef(r2) - 1.96 * std.err2r,
                 UL = coef(r2) + 1.96 * std.err2r)


round(r.est2r,3)

#R3: Model 1, plus the measure of American alliances (Alliances, column P)


summary(r3 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON+Alliances, data = HD1))
cov.r3 <- vcovHC(r3, type="HC0")
std.err3r <- sqrt(diag(cov.r3))
r.est3r <- cbind(Estimate= coef(r3), "Robust SE" = std.err3r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r3)/std.err3r), lower.tail=FALSE),
                 LL = coef(r3) - 1.96 * std.err3r,
                 UL = coef(r3) + 1.96 * std.err3r)


round(r.est3r,3)

# R4: Model 1, plus the count of terrorist incidents in the preceding year (incidentsusvictims, column Q).


oldw <- getOption("warn")
options(warn = -1)

summary(r4 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON+incidentsusvictims, data = HD1))
cov.r4 <- vcovHC(r4, type="HC0")
std.err4r <- sqrt(diag(cov.r4))
r.est4r <- cbind(Estimate= coef(r4), "Robust SE" = std.err4r,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(r4)/std.err4r), lower.tail=FALSE),
                 LL = coef(r4) - 1.96 * std.err4r,
                 UL = coef(r4) + 1.96 * std.err4r)


round(r.est4r,3)

options(warn = oldw)