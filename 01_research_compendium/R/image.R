# ===============================================================
# Generate summary table and visualization for the research report
# ===============================================================


# ==================================================
# Run analysis script 
source("R/analysis.R")
library(tidyverse)

# ==================================================
# Build unified dataset across scenarios
plot_data <- results$s1$cv_summary %>%
  rbind(results$s2$cv_summary) %>%
  rbind(results$s3$cv_summary) %>%
  rbind(results$s4$cv_summary) %>%
  
  # Add scenario labels and recorde model / nesting
  mutate(
    scenario = c(rep("Baseline",6),
                 rep("Nonlinear",6),
                 rep("Interaction",6),
                 rep("Multicollinearity",6)),
    
    nesting = ifelse(model %in% c("GMERT","GMERF", "GLMM"), 
                     "With Nesting", "Without Nesting"),
    
    model = ifelse(model %in% c("GLMM", "Logit"), "GLM",
                   ifelse(model %in% c("GMERT", "CART"), "CART", "RF")),
    
    nesting = factor(
      nesting,
      levels = c("Without Nesting", "With Nesting"),
      labels = c("Without Nesting", "With Nesting")
    )
  ) %>%
  
  # Reshape metrics to long format
  pivot_longer(
    cols = c(acc_mean, f1_maj_mean, f1_min_mean, bias_mean),
    names_to = "metric",
    values_to = "mean"
  ) %>%
  
  # Attach SDs and clean metric labels
  mutate(
    sd = case_when(
      metric == "acc_mean" ~ acc_sd,
      metric == "f1_mag_mean" ~ f1_maj_sd,
      metric == "f1_min_mean" ~ f1_min_sd,
      metric == "bias_mean" ~ bias_sd
    ),
    
    metric = recode(metric,
                    acc_mean    = "Accuracy",
                    f1_maj_mean = "F1 (Majority)",
                    f1_min_mean = "F1 (Minority)",
                    bias_mean   = "Bias"
    )
  )


# ==================================================================
# Plot: metrics by model, scenario, and nesting

ggplot(
  plot_data,
  aes(x = model, y = mean, color = metric, shape = nesting)
) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.6),
    width = 0.15,
    linewidth = 0.6
  ) +
  facet_wrap(~ scenario, nrow = 2) +
  labs(
    x = NULL,
    y = "Mean ± SD",
    color = "Metric",
    shape = "Model specification"
  ) +
  theme_minimal(base_size = 13) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = "left",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text  = element_text(size = 10),
    legend.spacing.y = unit(2, "pt"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey50", fill = NA, linewidth = 0.8)
  )


# ==========================================================================
# Save figure
ggsave(
  filename = "images/graph.png",
  width = 8,
  height = 6,
  dpi = 300
)

