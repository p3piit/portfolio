# ===============================================================
# In this script we generate the table needed for the research report
# ===============================================================
# Run the analysis from the analysis script
source("R/analysis.R")

f1_tab <- as.data.frame(cbind(results$s1$cv_summary$f1_maj_mean, 
                              results$s2$cv_summary$f1_maj_mean,
                              results$s3$cv_summary$f1_maj_mean,
                              results$s4$cv_summary$f1_maj_mean))

row.names(f1_tab) <- c("GMERT","GLMM", "Logit", "CART", "RF", "GMERF") 

# put the sixth observation (GMERF) in the third place
f1_tab <-   dplyr::slice(round(f1_tab,3), c(3,4,5,1,2,6))
colnames(f1_tab) <- c("Baseline","Nonlinear","Interaction","Multicollinearity")
print(f1_tab)
# save as latex table
library(xtable)
xtable(f1_tab, 
       caption = "Mean F1-score of the majority class across simulation scenarios",
       label = "tab:f1_scores")
# make an image of the table
library(gridExtra)
png("tables/f1_table.png", width = 600, height = 400)
grid.table(round(f1_tab,3))
dev.off()



