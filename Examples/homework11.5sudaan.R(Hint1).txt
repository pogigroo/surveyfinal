###############################################################
###  FILE ~\JPSM\homework11.5.R.txt   (in Libraries\Documents)
###  PROJECT: SURV 742 Inference
###  DATE: 01 Apr 2010
###  PGMR: R Fay
###  PURPOSE: Provide a start on homework 11.5 and 11.6
###############################################################


require(foreign)
adults <- read.xport("c:\\JPSM\\data sets\\adults.xpt")

#--------------------------------------------------------------
# According to the file variables.lst, the dichotomies are
# BLACK (1=No, 2=Yes)
# OVERWGHT (1=Yes, 2=No)
# CIG100 (1=Yes, 2=No)
# OVERWT (1=Yes, 0=No)
# For ease of interpretation, recode these to 0/1 variables.
# Note: we will create OVERWGHTr to be identical to OVERWT.
# Also, create a more meaningful name for POVERTY.
#
# At this point, we are adding variables to a data frame.
# After a survey design object is created, you can add 
# variables to it with update()
#--------------------------------------------------------------

adults$OVERWGHTr <- adults$OVERWGHT
adults$OVERWGHTr[adults$OVERWGHT==2] <- 0

adults$CIG100r  <- adults$CIG100
adults$CIG100r[adults$CIG100==2] <- 0

adults$BLACKr <- adults$BLACK-1

adults$IncPovRatio <- adults$POVERTY

adults$agecodef <- as.factor(adults$AGECODE)

adults$agecodefs <- relevel(adults$agecodef,ref="7")

adults$blackfs <- relevel(as.factor(adults$BLACK),ref="2")

adults$cig100fs <- relevel(as.factor(adults$CIG100),ref="2")

adults$overwtfs <- relevel(as.factor(adults$OVERWT),ref="0")

#--------------------------------------------------------------
# Run the specific model requested by 11.5
#--------------------------------------------------------------


require(survey)

adults.dsgn <- svydesign(ids = ~PSU, strata = ~STRATUM, 
                         data = adults, weights = ~EXAMWGT, 
                         nest=T)

#--------------------------------------------------------------
# Rerun the model requested by 11.5 separately by Black and 
# non-Black
#--------------------------------------------------------------


nonblack.dsgn <- subset(adults.dsgn,BLACKr==0)
black.dsgn <- subset(adults.dsgn,BLACKr==1)

rmodel1nb <- svyglm(LEAD~WEIGHT+IncPovRatio+as.factor(AGECODE)
                   +OVERWGHTr+CIG100r,
                   design=nonblack.dsgn)
summary(rmodel1nb)

rmodel1b  <- svyglm(LEAD~WEIGHT+IncPovRatio+as.factor(AGECODE)
                   +OVERWGHTr+CIG100r,
                   design=black.dsgn)

#--------------------------------------------------------------
# Run the specific model requested by 11.5
#--------------------------------------------------------------


summary(rmodel1b)

rmodel1 <- svyglm(LEAD~WEIGHT+IncPovRatio+BLACKr
            +as.factor(AGECODE)+OVERWGHTr
            +CIG100r+BLACKr:OVERWGHTr
            +BLACKr:as.factor(AGECODE)+BLACKr:CIG100r,
            design=adults.dsgn)

summary(rmodel1)
rmodel1$aic

rmodel1sudaan <- svyglm(LEAD~WEIGHT+IncPovRatio+blackfs
                 +agecodefs+overwtfs
                 +cig100fs+blackfs:overwtfs
                 +blackfs:agecodefs+blackfs:cig100fs,
                 design=adults.dsgn)

summary(rmodel1sudaan)
rmodel1sudaan$aic

#--------------------------------------------------------------
# For 11.5b, investigate the significance of various subsets
# of variables
#
# First, show that correctly interpreted, the contribution of the
# BLACK:AGECODE interaction is the same regardless of the 
# parameterization used by R or SUDAAN
#--------------------------------------------------------------


regTermTest(rmodel1,~BLACKr:as.factor(AGECODE))

regTermTest(rmodel1,~BLACKr:as.factor(AGECODE),method="LRT")

regTermTest(rmodel1sudaan,~blackfs:agecodefs)

regTermTest(rmodel1sudaan,~blackfs:agecodefs,method="LRT")

#--------------------------------------------------------------
# How about (a) WEIGHT, OVERWGHT, and the interaction with 
# BLACK, or (b) OVERWGHT and its interaction with BLACK?
#--------------------------------------------------------------

regTermTest(rmodel1,~WEIGHT+OVERWGHTr+BLACKr:OVERWGHTr)

regTermTest(rmodel1,~WEIGHT+OVERWGHTr+BLACKr:OVERWGHTr,method="LRT")

regTermTest(rmodel1,~OVERWGHTr+BLACKr:OVERWGHTr)

regTermTest(rmodel1,~OVERWGHTr+BLACKr:OVERWGHTr,method="LRT")

#--------------------------------------------------------------
# The most conservative approach to reduce the model is to
# drop only WEIGHT
#--------------------------------------------------------------


rmodel2 <- svyglm(LEAD~IncPovRatio+BLACKr
            +as.factor(AGECODE)+OVERWGHTr
            +CIG100r+BLACKr:OVERWGHTr
            +BLACKr:as.factor(AGECODE)+BLACKr:CIG100r,
            design=adults.dsgn)

summary(rmodel2)
rmodel2$aic

regTermTest(rmodel2,~OVERWGHTr+BLACKr:OVERWGHTr)

regTermTest(rmodel2,~OVERWGHTr+BLACKr:OVERWGHTr,method="LRT")

#--------------------------------------------------------------
# Several students were attracted to a far more aggressive
# simplification
#--------------------------------------------------------------

rmodel3 <- svyglm(LEAD~IncPovRatio+BLACKr
            +as.factor(AGECODE)
            +CIG100r
            +BLACKr:CIG100r,
            design=adults.dsgn)

summary(rmodel3)
rmodel3$aic


