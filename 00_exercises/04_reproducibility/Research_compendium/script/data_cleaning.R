# ====================================================================
# R CODE
# small scale simulation study to investigate impact of measurement error
# measurement error on (continuous) exposure and/or (continuous) confounding variable
# ====================================================================
# DATA CLEANING
# ====================================================================
# libraries:
library(here)
library(Hmisc)
library(tidyverse)
library(mice)
# ====================================================================
# set working directory:
here::here()
# ====================================================================
# The data can be dowloaded in xpt form from https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015
# read data:
d1 <- sasxport.get(here("data","raw_data","DEMO_I.XPT")) %>% 
  select(seqn, riagendr, ridageyr)
d2 <- sasxport.get(here("data","raw_data","BPX_I.XPT")) %>% 
  select(seqn, bpxsy1)
d3 <- sasxport.get(here("data","raw_data","BMX_I.XPT")) %>% 
  select(seqn, bmxbmi)
d4 <- sasxport.get(here("data","raw_data","GHB_I.XPT")) %>% 
  select(seqn, lbxgh)
d5 <- sasxport.get(here("data","raw_data","TCHOL_I.XPT")) %>% 
  select(seqn, lbdtcsi)

d <- d1 %>%
  inner_join(d2, by="seqn") %>%
  inner_join(d3, by="seqn") %>%
  inner_join(d4, by="seqn") %>%
  inner_join(d5, by="seqn")


# ====================================================================
# rename variables:
# RIAGENDR - Gender
# RIDAGEYR - Age in years at screening
# BPXSY1 - Systolic: Blood pres (1st rdg) mm Hg
# BMXBMI - Body Mass Index (kg/m**2)
# LBDTCSI - Total Cholesterol (mmol/L)
# LBXGH - Glycohemoglobin (%)

d <- d %>%
  rename(age = ridageyr,
         sex = riagendr,
         bp = bpxsy1,
         bmi = bmxbmi,
         HbA1C = lbxgh,
         chol = lbdtcsi) %>%
  mutate(age = ifelse(age < 18, NA, age))

# ====================================================================
# select complete cases:
dc <- cc(subset(d,select=c("age","sex","bmi","HbA1C","bp")))

# ====================================================================
# Write the cleaned data to file:
write.csv(dc, here("data","cleaned_data","cleaned_data.csv"), row.names=FALSE)


# ====================================================================
# END OF R CODE
# ===================================================================