# ====================================================================
# R CODE
# small scale simulation study to investigate impact of measurement error
# measurement error on (continuous) exposure and/or (continuous) confounding variable
# ====================================================================
# ANALYSIS
# ===================================================================
# libraries:
library(here)
# ====================================================================
# download data
dc <- read.csv(here("data","cleaned_data","cleaned_data.csv"))

# analysis:
summary(lm(bp ~ HbA1C + age + as.factor(sex), data=dc))
confint(lm(bp ~ HbA1C + age + as.factor(sex), data=dc))
summary(lm(bp ~ HbA1C + bmi + age + as.factor(sex), data=dc))
confint(lm(bp ~ HbA1C + bmi + age + as.factor(sex), data=dc))
# ====================================================================
# simulation of measurement error:
ref <- lm(bp ~ HbA1C + bmi + age + as.factor(sex), data=dc)$coef[2]
n.sim <- 1e3
perc.me.exp <- seq(0,.5,.1)
perc.me.conf<- seq(0,.5,.1)
scenarios <- expand.grid(perc.me.exp,perc.me.conf)
var.exp <- var(dc$HbA1C)
var.conf <- var(dc$bmi)
n <- dim(dc)[1]
beta.hat <- matrix(ncol=dim(scenarios)[1], nrow=n.sim)
for (k in 1:n.sim){
  print(k)
  set.seed(k)
  for (i in 1:dim(scenarios)[1]){
    var.me.exp <- var.exp*scenarios[i,1]/(1-scenarios[i,1])
    var.me.conf <- var.conf*scenarios[i,2]/(1-scenarios[i,2])
    dc$HbA1C.me <- dc$HbA1C + rnorm(dim(dc)[1], 0, sqrt(var.me.exp) )
    dc$bmi.me <- dc$bmi + rnorm(dim(dc)[1], 0, sqrt(var.me.conf) )
    beta.hat[k,i] <- lm(bp ~ HbA1C.me + age + bmi.me + as.factor(sex), data=dc)$coef[2]
  }}

# ====================================================================
# END OF R CODE
# ===================================================================