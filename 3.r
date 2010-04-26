require(foreign)
adults <- read.xport("Datasets/adults.xpt")
# summary(adults)

require(survey)

adults$ASTHMA <- abs(adults$ASTHMA-2)
adults$CIG100 <- abs(adults$CIG100-2) #smoked
adults$HAYFEVER <- abs(adults$HAYFEVER-2)
# drinks = TOT_ALQ
# age = AGE
# head(adults$PSU)
# race = RACETHN

adults.dsgn <- svydesign(ids = ~PSU, strata = ~STRATUM, weights = ~EXAMWGT,
                         data = adults, 
                         nest=T)
adults.JKn <- as.svrepdesign(adults.dsgn, type="JKn")

rmodel2 <- svyglm(CIG100 ~ ASTHMA + as.factor(RACETHN) + HAYFEVER +TOT_ALQ + AGE,family = binomial, design=adults.JKn)
# [part a] 
# plot(rmodel2$y,rmodel2$fitted.values)
summary(rmodel2)
# plot(rmodel2)


# [part b] 
betas <- summary(rmodel2)$coefficients[,1]
SE.betas <- summary(rmodel2)$coefficients[,2] 
adults.JKn$degf
CIs <- round(cbind(betas - qt(p = 0.975, df = adults.JKn$degf)*SE.betas, betas, betas + qt(p = 0.975, df = adults.JKn$degf)*SE.betas), 4)
# get odds ratio for asthmatics from this table
dimnames(CIs)[[2]] <- c("Lower", "Beta", "Upper") # Odds ratios and CIs
round(exp(CIs), 4)

#[part c]
#scaled covariance matrix
rmodel2.cov <- summary(rmodel2)$cov.scaled
# Vector that specifies an asthmatic, mexian american, without hayfever, has 30 drinks a month and is 40 years old.
char <- c(0,1,0,1,0,0,30,40)
odds.char <- round(exp(sum(betas*char)),4)

SE.char <- sqrt(char%*%rmodel2.cov%*%char)
round(cbind(odds.char - qt(p = 0.975, df = adults.JKn$degf)*SE.char , odds.char, odds.char + qt(p = 0.975, df = adults.JKn$degf)*SE.char), 4)