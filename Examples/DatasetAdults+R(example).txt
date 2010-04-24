###############################################################
###  FILE ~\JPSM\example13.R.txt   (in Libraries\Documents)
###  PROJECT: SURV 742 Inference
###  DATE: 11 Apr 2010
###  PGMR: R Fay
###  PURPOSE: Illustrate factorial log-linear models as an
###        addition to chapter 13
###############################################################


require(foreign)
adults <- read.xport("c:\\JPSM\\data sets\\adults.xpt")


###############################################################
#  First, analyze the data as many sociologists might, ignoring
#  the weights and design.  R has routines to analyze multiway
#  tables with log-linear models by assuming simple random 
#  sampling
###############################################################

table1 <- table(adults$RACETHN,adults$HYPER,adults$SEX,useNA="no",
          dnn=c("Race/Ethnicity","Hyper", "Sex"))          

table1
summary(table1)
require(MASS)

# model1 is the 3-way independence model
# model2 represents conditional independence given sex
#        (related to problems 11.5 and 11.6)
# model3 is the "no-three-way-interaction model"

model1 <- loglm(~1+2+3,table1)     # independence

model2 <- loglm(~1*3+2*3,table1)   # conditional independence
                                   # given sex
                                   
model3 <- loglm(~1*2+1*3+2*3,table1,param=T) # no-3-factor

model1
model2
model3
anova(model3,model1)
anova(model3,model2)
anova(model2,model1)

#  model3 is effectively a logistic regression model for HYPER
#  as can be seen (with some difficulty) from the parameter
#  estimates
#  for example, 4*.1169773 = .46791 (off in last place)
#

model3$param

with(adults, {
    glm(as.factor(HYPER)~as.factor(SEX)+as.factor(RACETHN),
    family="binomial") } )

# The survey package currently supports some of this program of 
# analysis, but parts appear under development

require(survey)
adults.dsgn <- svydesign(ids = ~PSU, strata = ~STRATUM, 
                         data = adults, weights = ~EXAMWGT, 
                         nest=T)


# svytable will tabulate a 3-way table and display a cross-classification
# similar to table1
table1d <- svytable(~RACETHN+HYPER+SEX,design=adults.dsgn,na.rm=T)
table1d

# but > summary table1d produces an error messages saying that chi-squares
# are only available for 2-way tables at the moment

# svytotal also will create the table, but summary(total1d) is
# disappointing

total1d <- svytotal(~interaction(as.factor(RACETHN),as.factor(HYPER),
              as.factor(SEX)),design=adults.dsgn,na.rm=T)

total1d
summary(total1d)

# svyglm can fit a logistic model that can be compared to 
# the SRS version

svyglm3 <- svyglm(as.factor(HYPER)~as.factor(SEX)+as.factor(RACETHN), 
                  design=adults.dsgn,family=quasibinomial())
                  
summary(svyglm3)

# to fit the log-linear model, I followed the example in 
# Lumley's book pretty closely
# m4 is the saturated model that fits exactly, for 
# purposes of comparing to m3

adults.dsgn <-update(adults.dsgn, hyper=as.factor(HYPER))
adults.dsgn <-update(adults.dsgn, sex=as.factor(SEX))
adults.dsgn <-update(adults.dsgn, racethn=as.factor(RACETHN))

m1 <- svyloglin(~hyper+sex+racethn, adults.dsgn)

m2 <- update(m1, ~.+(hyper:sex)+racethn:sex)

m3 <- update(m2, ~.+(hyper:racethn))

m4 <- update(m3, ~.+(hyper:racethn:sex))

 
summary(m1)
summary(m2)
summary(m3)
summary(m4)

anova(m1,m2)
anova(m1,m3)
anova(m2,m3)
anova(m3,m4)




 




