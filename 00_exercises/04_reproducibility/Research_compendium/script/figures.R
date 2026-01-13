# ====================================================================
# R CODE
# small scale simulation study to investigate impact of measurement error
# measurement error on (continuous) exposure and/or (continuous) confounding variable
# ====================================================================
# FIGURE GENERATION
# ====================================================================
# libraries:
library(here)
library(tidyverse)
# ====================================================================
# Run the analysis script
source(here("script","analysis.R"))

# ====================================================================
# create figure:
tot.mat <- cbind(100*scenarios,apply(beta.hat,2,mean))
colnames(tot.mat) <- c("me.exp","me.conf","estimate")
FIGURE <- ggplot(tot.mat, aes(me.exp, me.conf)) +
  geom_tile(color="white",aes(fill = estimate)) +
  geom_text(aes(label = round(estimate, 2))) +
  scale_fill_gradient2(low="#D55E00",mid="white",high = "#56B4E9", midpoint=ref) +
  labs(x=paste("% of total variance of HbA1c due to measurement error"),
       y=paste("% of total variance of BMI due to measurement error")) +
  coord_equal()+
  scale_y_continuous(breaks=unique(tot.mat[,1]))+
  scale_x_continuous(breaks=unique(tot.mat[,1]))+
  theme(panel.background = element_rect(fill='white', colour='grey'),
        plot.title=element_text(hjust=0),
        axis.ticks=element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10))
FIGURE
# save figure:
ggsave(here("figures","figure_measurement_error.png"),FIGURE, width=6, height=5, dpi=300)
# ====================================================================
# END OF R CODE
# ===================================================================