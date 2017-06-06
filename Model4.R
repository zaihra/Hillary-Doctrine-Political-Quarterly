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
#summary(HD1)
length(unique(HD1$CTRY))
summary(HD1$incidentsusvictims_L1)
mean(HD1$incidentsusvictims_L1,na.rm=TRUE)
var(HD1$incidentsusvictims_L1,na.rm=TRUE)
#MODEL4
summary(m4 <- glm.nb(incidentsusvictims_L1 ~ lnpopterr + logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON+WOPOL+WOSOC, data = HD1))
cov.m4 <- vcovHC(m4, type="HC0")
std.err4 <- sqrt(diag(cov.m4))
r.est4 <- cbind(Estimate= coef(m4), "Robust SE" = std.err4,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(m4)/std.err4), lower.tail=FALSE),
                LL = coef(m4) - 1.96 * std.err4,
                UL = coef(m4) + 1.96 * std.err4)


round(r.est4,3)
with(m4, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#To present the regression results as incident rate ratios and 
#their standard errors, together with the confidence interval. 
#To compute the standard error for the incident rate ratios, 
#use the Delta method. To this end, we make use the function deltamethod implemented in R package msm.

s4 <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4),~ exp(x5),~ exp(x6),~ exp(x7),~ exp(x8), ~exp(x9), ~exp(x10)), coef(m4), cov.m4)
## exponentiate old estimates dropping the p values
rexp.est4 <- exp(r.est4[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est4[, "Robust SE"] <- s4

rexp.est4
round(rexp.est4,3)


########################################
summary(HD1$WECON)
summary(HD1$WOPOL)
summary(HD1$WOSOC)
library(psych)
#describe(HD1)

# diagnostics for multicolinarity and relative importance of predictors
x<-as.matrix(cbind(HD1$WECON,HD1$WOPOL,HD1$WOSOC))
head(x)

library(Hmisc)
rcorr(x,type="pearson") # type can be pearson or spearman

library(Hmisc)
cor(x,use = "complete.obs",method = "spearman")
#plot(HD1$WECON,HD1$WOPOL)
oldw <- getOption("warn")
options(warn = -1)
cor.test(HD1$WECON,HD1$WOPOL,method = "spearman", alternative = "greater")
options(warn = oldw)
# ideally we should be using spearman or gamma, spearman requires no ties which 
#calls for continuous ordinal
#install.packages("rococo")
#library(rococo)
#a<-rococo(HD1$WECON,HD1$WOPOL, na.rm=TRUE)

#rococo.test(x=HD1$WECON,y=HD1$WOPOL,alternative = "greater",na.rm=TRUE)

# Assume that we are fitting a multiple linear regression

fit <- lm(incidentsusvictims_L1 ~ lnpopterr+logGDPCAP+logUSMilitaryAid+logArea+Polity2+CINC+WECON+WOPOL+WOSOC, data = HD1)
# Evaluate Collinearity
# note the model coefficients are not right as our response is count 
#but VIF or tolerance is right as it's calculation doesnt involve outcome variable
# it doesnt effect how predictors are related to each other
library(MASS)
library(car)
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?
