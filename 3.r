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

adults.dsgn <- svydesign(ids = ~PSU, strata = ~STRATUM, 
                         data = adults, 
                         nest=T)
summary(adults.dsgn) 

rmodel1 <- svyglm(CIG100 ~ ASTHMA + as.factor(RACETHN) + HAYFEVER +TOT_ALQ + AGE, design=adults.dsgn) 
summary(rmodel1)

adults.JKn <- as.svrepdesign(adults.dsgn, type="JKn")
# summary(adults.JKn)
rmodel2 <- svyglm(CIG100 ~ ASTHMA + as.factor(RACETHN) + HAYFEVER +TOT_ALQ + AGE, design=adults.JKn)
summary(rmodel2) 


betas <- summary(rmodel2)$coefficients[,1][-1]
SE.betas <- summary(rmodel2)$coefficients[,2][-1] 
CIs <- round(
	cbind(betas - qt(p = 0.975, df = adults.JKn$degf)*SE.betas, betas, betas + qt(p = 0.975, df = adults.JKn$degf)*SE.betas), 4)
dimnames(CIs)[[2]] <- c("Lower", "Beta", "Upper") # Odds ratios and CIs
round(exp(CIs), 4)

# adults.pred <- adults[adults$ASTHMA==1 & adults$CIG100==1 & adults$RACETHN==3 & adults$HAYFEVER==0 & adults$TOT_ALQ ==30 & adults$AGE==40, ]

adults.pred <- data.frame(AGE=40, ASTHMA=1, CIG100=1, RACETHN=3, HAYFEVER=0,TOT_ALQ =30 )

# adults.pred <- data.frame(ASTHMA=rep(1,8360), CIG100=rep(1,8360), RACETHN=rep(3,8360), HAYFEVER=rep(0,8360) ,TOT_ALQ =rep(30,8360) )
summary(adults.pred)
predict(rmodel2, newdata=adults.pred, type="response")

require(survey)
data(api)




dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
dclus2<-svydesign(id=~dnum+snum, weights=~pw, data=apiclus2)
rstrat<-as.svrepdesign(dstrat)
rclus2<-as.svrepdesign(dclus2)

summary(svyglm(api00~ell+meals+mobility, design=dstrat))
summary(svyglm(api00~ell+meals+mobility, design=dclus2))
summary(svyglm(api00~ell+meals+mobility, design=rstrat))
summary(svyglm(api00~ell+meals+mobility, design=rclus2))

## use quasibinomial, quasipoisson to avoid warning messages
summary(svyglm(sch.wide~ell+meals+mobility, design=dstrat,
      family=quasibinomial()))


## Compare regression and ratio estimation of totals
api.ratio <- svyratio(~api.stu,~enroll, design=dstrat)
pop<-data.frame(enroll=sum(apipop$enroll, na.rm=TRUE))
npop <- nrow(apipop)
predict(api.ratio, pop$enroll)

## regression estimator is less efficient
api.reg <- svyglm(api.stu~enroll, design=dstrat)
predict(api.reg, newdata=pop, total=npop)
## same as calibration estimator
svytotal(~api.stu, calibrate(dstrat, ~enroll, pop=c(npop, pop$enroll)))

## svyglm can also reproduce the ratio estimator
api.reg2 <- svyglm(api.stu~enroll-1, design=dstrat,
                  family=quasi(link="identity",var="mu"))
predict(api.reg2, newdata=pop, total=npop)

## higher efficiency by modelling variance better
api.reg3 <- svyglm(api.stu~enroll-1, design=dstrat,
                  family=quasi(link="identity",var="mu^3"))
predict(api.reg3, newdata=pop, total=npop)
## true value
sum(apipop$api.stu)
