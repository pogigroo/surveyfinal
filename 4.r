require(foreign)
adults <- read.xport("Datasets/adults.xpt")
# summary(adults)

#--------------------------------------------------------------
adults$IncPovRatio <- adults$POVERTY
adults$BLACK <- adults$BLACK-1
adults$OVERWGHT <- abs(adults$OVERWGHT-2)
adults$ASTHMA <- abs(adults$ASTHMA-2)
adults$CIG100 <- abs(adults$CIG100-2) #smoked
adults$HAYFEVER <- abs(adults$HAYFEVER-2)

#--------------------------------------------------------------
require(survey)

adults.fay <- svydesign(ids = ~PSU, strata = ~STRATUM, 
                         data = adults, weights = ~EXAMWGT, 
                         nest=T,type="Fay", fay.rho=0.5)

# summary(adults.dsgn) # optional, confirm design
# nonblack.dsgn <- subset(adults.dsgn,BLACKr==0)
# black.dsgn <- subset(adults.dsgn,BLACKr==1)

rmodel1.lin <- svyglm(LEAD~IncPovRatio+ BLACK + as.factor(AGECODE) + CIG100 + BLACK:as.factor(AGECODE) + BLACK:CIG100, design=adults.fay)
summary(rmodel1.lin)
rmodel2.lin <- svyglm(HIGHLEAD~IncPovRatio+ BLACK + as.factor(AGECODE) + CIG100 + BLACK:as.factor(AGECODE) + BLACK:CIG100, design=adults.fay)
summary(rmodel2.lin)
rmodel2.log <- svyglm(HIGHLEAD~IncPovRatio+ BLACK + as.factor(AGECODE) + CIG100 + BLACK:as.factor(AGECODE) + BLACK:CIG100, design=adults.fay, family = binomial)
summary(rmodel2.log)

#-----------------------------------------------------------------
rmodel1.blackage.wald <- regTermTest(rmodel1.lin,"BLACK:as.factor(AGECODE)")
rmodel1.blackage.age.wald <- regTermTest(rmodel1.lin,~as.factor(AGECODE) + BLACK:as.factor(AGECODE))

regTermTest(rmodel1.lin,"BLACK:as.factor(AGECODE)", method="LRT")
regTermTest(rmodel1.lin,~as.factor(AGECODE) + BLACK:as.factor(AGECODE), method="LRT")

#Wald F for model1.lin
# Assuming that Variance estimate has 30 degrees of freedom, corresponding to number of strata in design
f <- 30
K <- 6
# Walf f for b1
(f-K+2)/(f*(K-1)) * rmodel1.blackage.wald$chisq
#crit value of test for b1
qf(.95, K-1,f-K+2)

K <- 12
# Walf f for b2
(f-K+2)/(f*(K-1)) * rmodel1.blackage.age.wald$chisq
#crit value of test for b2
qf(.95, K-1,f-K+2)

#-----------------------------------------------------------------
rmodel2.blackage.wald <- regTermTest(rmodel2.lin,"BLACK:as.factor(AGECODE)")
rmodel2.blackage.age.wald <- regTermTest(rmodel2.lin,~as.factor(AGECODE) + BLACK:as.factor(AGECODE))

regTermTest(rmodel2.lin,"BLACK:as.factor(AGECODE)", method="LRT")
regTermTest(rmodel2.lin,~as.factor(AGECODE) + BLACK:as.factor(AGECODE), method="LRT")

#Wald F for model1.lin
# Assuming that Variance estimate has 30 degrees of freedom, corresponding to number of strata in design
f <- 30
K <- 6
# Walf f for b1
(f-K+2)/(f*(K-1)) * rmodel2.blackage.wald$chisq
#crit value of test for b1
qf(.95, K-1,f-K+2)

K <- 12
# Walf f for b2
(f-K+2)/(f*(K-1)) * rmodel2.blackage.age.wald$chisq
#crit value of test for b2
qf(.95, K-1,f-K+2)
#-----------------------------------------------------------------
rmodel2.blackage.wald <- regTermTest(rmodel2.log,"BLACK:as.factor(AGECODE)")
rmodel2.blackage.age.wald <- regTermTest(rmodel2.log,~as.factor(AGECODE) + BLACK:as.factor(AGECODE))

regTermTest(rmodel2.log,"BLACK:as.factor(AGECODE)", method="LRT")
regTermTest(rmodel2.log,~as.factor(AGECODE) + BLACK:as.factor(AGECODE), method="LRT")

#Wald F for model1.lin
# Assuming that Variance estimate has 30 degrees of freedom, corresponding to number of strata in design
f <- 30
K <- 6
# Walf f for b1
(f-K+2)/(f*(K-1)) * rmodel2.blackage.wald$chisq
#crit value of test for b1
qf(.95, K-1,f-K+2)

K <- 12
# Walf f for b2
(f-K+2)/(f*(K-1)) * rmodel2.blackage.age.wald$chisq
#crit value of test for b2
qf(.95, K-1,f-K+2)