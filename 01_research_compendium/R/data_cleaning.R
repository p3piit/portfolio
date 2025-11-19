# ====================================================================
# R CODE for data cleaning 
# ====================================================================
# ====================================================================
# libraries:
renv::activate()
library(Hmisc)
library(mice)
library(tidyverse)
library(here)
# ====================================================================
# The data can be dowloaded in xpt form from https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015
# read data:
d1 <- sasxport.get(here("data","raw_data","DEMO_I.XPT"))
d2 <- sasxport.get(here("data","raw_data","BPX_I.XPT"))
d3 <- sasxport.get(here("data","raw_data","BMX_I.XPT"))
d4 <- sasxport.get(here("data","raw_data","GHB_I.XPT"))
d5 <- sasxport.get(here("data","raw_data","TCHOL_I.XPT"))
d1.t <- subset(d1,select=c("seqn","riagendr","ridageyr"))
d2.t <- subset(d2,select=c("seqn","bpxsy1"))
d3.t <- subset(d3,select=c("seqn","bmxbmi"))
d4.t <- subset(d4,select=c("seqn","lbxgh"))
d5.t <- subset(d5,select=c("seqn","lbdtcsi"))
d <- merge(d1.t,d2.t)
d <- merge(d,d3.t)
d <- merge(d,d4.t)
d <- merge(d,d5.t)
# ====================================================================
# rename variables:
# RIAGENDR - Gender
# RIDAGEYR - Age in years at screening
# BPXSY1 - Systolic: Blood pres (1st rdg) mm Hg
# BMXBMI - Body Mass Index (kg/m**2)
# LBDTCSI - Total Cholesterol (mmol/L)
# LBXGH - Glycohemoglobin (%)
d$age <- d$ridageyr
d$sex <- d$riagendr
d$bp <- d$bpxsy1
d$bmi <- d$bmxbmi
d$HbA1C <- d$lbxgh
d$chol <- d$lbdtcsi
d$age[d$age<18] <- NA
# select complete cases:
dc <- cc(subset(d,select=c("age","sex","bmi","HbA1C","bp")))

# Save the cleaned data 
saveRDS(dc, file=here("data","clean_data.rds"))
