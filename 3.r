require(foreign)
adults <- read.xport("Datasets/adults.xpt")
summary(adults)

adults$ASTHMA <- abs(adults$ASTHMA-2)
adults$CIG100 <- abs(adults$CIG100-2)

# head(adults$PSU)


adults.dsgn <- svydesign(ids = ~psu,
strata = ~stratum, nest = true, data = adults, weights = ~examwgt)

require(survey)


attach(adults)
asthma <- V23
asthma[asthma == 1] <- "yes"
asthma[asthma == 2] <- "no"
summary(as.factor(asthma))
asthma <- as.factor(asthma)
race <- as.factor(V13)
hayfever <- as.factor(V24)
drinks <- V41
age <- V27
smoked <- as.factor(V30)
drinks <- as.double(drinks)
hayfever[hayfever == "."] <- NA
hayfever <- as.factor(hayfever)
lmodel <- glm(smoked ~ asthma + race + hayfever + drinks + age, family = binomial(link = "logit"))
summary(lmodel)

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
