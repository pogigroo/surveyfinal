###############################################################
###  FILE ~\JPSM\homework11.5.R.txt   (in Libraries\Documents)
###  PROJECT: SURV 742 Inference
###  DATE: 20 Mar 2010
###  PGMR: R Fay
###  PURPOSE: Provide a start on homework 11.5 and 11.6
###############################################################

#--------------------------------------------------------------
# Load foreign package for importing files and read adults file
# Use summary() to take a first look at the variables.
#
# Because R uses "\" as an escape character, "\\" is needed
# to reference the data file in read.xport.
#
# Notice summary() shows that all variable names are upper case.
# LEAD has 240 NA. POVERTY has 660 NA, as well as being highly
# skewed.
#--------------------------------------------------------------

require(foreign)
adults <- read.xport("~\\JPSM\\data sets\\adults.xpt")
summary(adults)

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

# summary(adults) # optional: check that variables are added

#--------------------------------------------------------------
# Bring in the survey package and set up the design
#
# Because the initial model in 11.5 includes interactions with
# BLACK, first look at the models separately for non-blacks 
# and blacks
#
# Note the use of as.factor() with AGECODE, otherwise R would
# treat AGECODE as a continuous variable
#--------------------------------------------------------------


require(survey)

adults.dsgn <- svydesign(ids = ~PSU, strata = ~STRATUM, 
                         data = adults, weights = ~EXAMWGT, 
                         nest=T)

# summary(adults.dsgn) # optional, confirm design


nonblack.dsgn <- subset(adults.dsgn,BLACKr==0)
black.dsgn <- subset(adults.dsgn,BLACKr==1)

rmodel1nb <- svyglm(LEAD~WEIGHT+IncPovRatio+as.factor(AGECODE)
                   +OVERWGHTr+CIG100r,
                   design=nonblack.dsgn)
summary(rmodel1nb)

rmodel1b  <- svyglm(LEAD~WEIGHT+IncPovRatio+as.factor(AGECODE)
                   +OVERWGHTr+CIG100r,
                   design=black.dsgn)

summary(rmodel1b)

#--------------------------------------------------------------
# Run the specific model requested by 11.5
#--------------------------------------------------------------


rmodel1 <- svyglm(LEAD~WEIGHT+IncPovRatio+BLACKr
            +as.factor(AGECODE)+OVERWGHTr
            +CIG100r+BLACKr*OVERWGHTr
            +BLACKr*as.factor(AGECODE)+BLACKr*CIG100r,
            design=adults.dsgn)

summary(rmodel1)

#--------------------------------------------------------------
# This much is a start. You must run at least one more model to
# complete the assignment.
#
# For 11.5.c, consider
# adults.dsgn <- svydesign(ids=~1, data = adults)
#
#
# To save the output begin with
# sink("filename")
# and end with
# sink()
# to return to the console
#
# For 11.6 use
# adults.JKn <- as.svrepdesign(adults.dsgn)
# and take it from there!
#--------------------------------------------------------------
