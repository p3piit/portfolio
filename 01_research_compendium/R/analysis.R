# ===============================================================
# In this script we run the parallelized cross-validation analysis
# ===============================================================
# clean the environment
rm(list = ls())

# Load necessary libraries
library(mlml) 
# This is my package with all the costum functions that I use and it
# also imports all the other necessary libraries.

# ================================================================
# Read data
df_list <- list(
  s1 = read.csv("data/simulated_data1.csv"),
  s2 = read.csv("data/simulated_data2.csv"),
  s3 = read.csv("data/simulated_data3.csv"),
  s4 = read.csv("data/simulated_data4.csv")
)

# Make sure id is a factor
df_list <- lapply(df_list, function(d) {
  d$id <- as.factor(d$id)
  d
})

# ================================================================
# Helpers

# cluster-aware folds 
make_cluster_folds <- function(df, K = 10, seed = 42) {
  set.seed(seed)
  folds <- vector("list", K)
  for (k in seq_len(K)) folds[[k]] <- integer(0)
  
  for (g in unique(df$id)) {
    idx_g <- which(df$id == g)
    idx_g <- sample(idx_g)
    split_g <- split(idx_g, cut(seq_along(idx_g), K, labels = FALSE))
    for (k in seq_len(K)) folds[[k]] <- c(folds[[k]], split_g[[k]])
  }
  folds
}

# fold worker: runs the same models/metrics you already run
# NOTE: scenario-specific bits passed as args:
# - glmm_formula
# - logit_formula
# - gmert_args / gmerf_args (tol/max_iter_out)
cv_one_fold <- function(k, df, folds,
                        glmm_formula, logit_formula,
                        gmert_args = list(), gmerf_args = list(),
                        rf_seed = 42) {
  
  test_idx  <- folds[[k]]
  train_idx <- setdiff(seq_len(nrow(df)), test_idx)
  df_train  <- df[train_idx, ]
  df_test   <- df[test_idx, ]
  
  # --- GMERT
  fit_gmert_k <- do.call(fit_gmert_small, c(list(df_train), gmert_args))
  yhat_gmert  <- predict_gmert(fit_gmert_k, df_test)
  
  # --- GLMM
  fit_glmm_k <- lme4::glmer(
    glmm_formula,
    data = df_train,
    family = binomial(link = "logit"),
    control = lme4::glmerControl(optimizer = "bobyqa")
  )
  p_glmm    <- as.numeric(predict(fit_glmm_k, newdata = df_test, type = "response"))
  yhat_glmm <- as.integer(p_glmm >= 0.5)
  
  # --- GMERF
  fit_gmerf_k <- do.call(fit_gmerf_small, c(list(df_train), gmerf_args))
  yhat_gmerf  <- predict_gmerf(fit_gmerf_k, df_test)
  
  # --- Logit
  logit_fit_k <- glm(logit_formula, data = df_train, family = binomial())
  p_logit     <- as.numeric(predict(logit_fit_k, newdata = df_test, type = "response"))
  pred_logit  <- as.integer(p_logit >= 0.5)
  
  # --- CART
  df_train_ct <- df_train; df_train_ct$y <- factor(df_train_ct$y, levels = c(0, 1))
  df_test_ct  <- df_test;  df_test_ct$y  <- factor(df_test_ct$y,  levels = c(0, 1))
  ctree_fit_k <- rpart::rpart(
    y ~ x1 + x2 + x3,
    data = df_train_ct,
    method = "class",
    control = rpart::rpart.control(cp = 0.0, minsplit = 20, minbucket = 7, maxdepth = 5)
  )
  pred_ct_class_k <- predict(ctree_fit_k, newdata = df_test_ct, type = "class")
  pred_ctree      <- as.integer(as.character(pred_ct_class_k))
  
  # --- RF
  rf_fit_k <- ranger::ranger(
    formula = y ~ x1 + x2 + x3,
    data = df_train,
    num.trees = 50,
    mtry = 2,
    min.node.size = 20,
    classification = TRUE,
    probability = FALSE,
    seed = rf_seed
  )
  pred_rf <- as.integer(predict(rf_fit_k, data = df_test)$predictions)
  
  # --- Confusion matrices
  cm_g     <- table(predicted = yhat_gmert,  actual = df_test$y)
  cm_l     <- table(predicted = yhat_glmm,   actual = df_test$y)
  cm_logit <- table(predicted = pred_logit,  actual = df_test$y)
  cm_ctree <- table(predicted = pred_ctree,  actual = df_test$y)
  cm_rf    <- table(predicted = pred_rf,     actual = df_test$y)
  cm_gmerf <- table(predicted = yhat_gmerf,  actual = df_test$y)
  
  # --- Metrics
  acc_g     <- mean(yhat_gmert == df_test$y)
  acc_l     <- mean(yhat_glmm  == df_test$y)
  acc_logit <- mean(pred_logit == df_test$y)
  acc_ctree <- mean(pred_ctree == df_test$y)
  acc_rf    <- mean(pred_rf == df_test$y)
  acc_gmerf <- mean(yhat_gmerf == df_test$y)
  
  f1_maj_g     <- f1_fun(cm_g,     majority = TRUE)
  f1_maj_l     <- f1_fun(cm_l,     majority = TRUE)
  f1_maj_logit <- f1_fun(cm_logit, majority = TRUE)
  f1_maj_ctree <- f1_fun(cm_ctree, majority = TRUE)
  f1_maj_rf    <- f1_fun(cm_rf,    majority = TRUE)
  f1_maj_gmerf <- f1_fun(cm_gmerf, majority = TRUE)
  
  f1_min_g     <- f1_fun(cm_g,     majority = FALSE)
  f1_min_l     <- f1_fun(cm_l,     majority = FALSE)
  f1_min_logit <- f1_fun(cm_logit, majority = FALSE)
  f1_min_ctree <- f1_fun(cm_ctree, majority = FALSE)
  f1_min_rf    <- f1_fun(cm_rf,    majority = FALSE)
  f1_min_gmerf <- f1_fun(cm_gmerf, majority = FALSE)
  
  bias_g <- sum(yhat_gmert)/sum(df_test$y) - 1
  bias_l <- sum(yhat_glmm)/sum(df_test$y) - 1
  bias_logit <- sum(pred_logit)/sum(df_test$y) - 1
  bias_ctree <- sum(pred_ctree)/sum(df_test$y) - 1
  bias_rf <- sum(pred_rf)/sum(df_test$y) - 1
  bias_gmerf <- sum(yhat_gmerf)/sum(df_test$y) - 1
  
  list(
    fold_id   = k,
    train_idx = train_idx,
    test_idx  = test_idx,
    metrics = list(
      acc_gmert = acc_g, f1_maj_gmert = f1_maj_g, f1_min_gmert = f1_min_g, bias_gmert = bias_g,
      acc_glmm  = acc_l, f1_maj_glmm  = f1_maj_l, f1_min_glmm  = f1_min_l, bias_glmm  = bias_l,
      acc_logit = acc_logit, f1_maj_logit = f1_maj_logit, f1_min_logit = f1_min_logit, bias_logit = bias_logit,
      acc_ctree = acc_ctree, f1_maj_ctree = f1_maj_ctree, f1_min_ctree = f1_min_ctree, bias_ctree = bias_ctree,
      acc_rf    = acc_rf,    f1_maj_rf    = f1_maj_rf,    f1_min_rf    = f1_min_rf,    bias_rf    = bias_rf,
      acc_gmerf = acc_gmerf, f1_maj_gmerf = f1_maj_gmerf, f1_min_gmerf = f1_min_gmerf, bias_gmerf = bias_gmerf
    ),
    confusion = list(
      gmert = cm_g, gmerf = cm_gmerf, glmm = cm_l, logit = cm_logit, ctree = cm_ctree, rf = cm_rf
    ),
    df_train = df_train,
    df_test  = df_test
  )
}

summarise_cv <- function(cv_results) {
  metrics_df <- do.call(rbind, lapply(cv_results, function(x) {
    data.frame(
      fold = as.double(x$fold_id),
      acc_gmert = x$metrics$acc_gmert,
      f1_maj_gmert = x$metrics$f1_maj_gmert,
      f1_min_gmert = x$metrics$f1_min_gmert,
      bias_gmert = x$metrics$bias_gmert,
      acc_glmm = x$metrics$acc_glmm,
      f1_maj_glmm = x$metrics$f1_maj_glmm,
      f1_min_glmm = x$metrics$f1_min_glmm,
      bias_glmm = x$metrics$bias_glmm,
      acc_logit = x$metrics$acc_logit,
      f1_maj_logit = x$metrics$f1_maj_logit,
      f1_min_logit = x$metrics$f1_min_logit,
      bias_logit = x$metrics$bias_logit,
      acc_ctree = x$metrics$acc_ctree,
      f1_maj_ctree = x$metrics$f1_maj_ctree,
      f1_min_ctree = x$metrics$f1_min_ctree,
      bias_ctree = x$metrics$bias_ctree,
      acc_rf = x$metrics$acc_rf,
      f1_maj_rf = x$metrics$f1_maj_rf,
      f1_min_rf = x$metrics$f1_min_rf,
      bias_rf = x$metrics$bias_rf,
      acc_gmerf = x$metrics$acc_gmerf,
      f1_maj_gmerf = x$metrics$f1_maj_gmerf,
      f1_min_gmerf = x$metrics$f1_min_gmerf,
      bias_gmerf = x$metrics$bias_gmerf
    )
  }))
  
  cv_summary <- data.frame(
    model = c("GMERT", "GLMM", "Logit", "CART", "RF", "GMERF"),
    acc_mean = c(mean(metrics_df$acc_gmert), mean(metrics_df$acc_glmm),
                 mean(metrics_df$acc_logit), mean(metrics_df$acc_ctree),
                 mean(metrics_df$acc_rf),    mean(metrics_df$acc_gmerf)),
    acc_sd   = c(sd(metrics_df$acc_gmert), sd(metrics_df$acc_glmm),
                 sd(metrics_df$acc_logit), sd(metrics_df$acc_ctree),
                 sd(metrics_df$acc_rf),    sd(metrics_df$acc_gmerf)),
    f1_maj_mean = c(mean(metrics_df$f1_maj_gmert), mean(metrics_df$f1_maj_glmm),
                    mean(metrics_df$f1_maj_logit), mean(metrics_df$f1_maj_ctree),
                    mean(metrics_df$f1_maj_rf),    mean(metrics_df$f1_maj_gmerf)),
    f1_maj_sd   = c(sd(metrics_df$f1_maj_gmert), sd(metrics_df$f1_maj_glmm),
                    sd(metrics_df$f1_maj_logit), sd(metrics_df$f1_maj_ctree),
                    sd(metrics_df$f1_maj_rf),    sd(metrics_df$f1_maj_gmerf)),
    f1_min_mean = c(mean(metrics_df$f1_min_gmert), mean(metrics_df$f1_min_glmm),
                    mean(metrics_df$f1_min_logit), mean(metrics_df$f1_min_ctree),
                    mean(metrics_df$f1_min_rf),    mean(metrics_df$f1_min_gmerf)),
    f1_min_sd   = c(sd(metrics_df$f1_min_gmert), sd(metrics_df$f1_min_glmm),
                    sd(metrics_df$f1_min_logit), sd(metrics_df$f1_min_ctree),
                    sd(metrics_df$f1_min_rf),    sd(metrics_df$f1_min_gmerf)),
    bias_mean = c(mean(metrics_df$bias_gmert, na.rm = TRUE), mean(metrics_df$bias_glmm, na.rm = TRUE),
                  mean(metrics_df$bias_logit, na.rm = TRUE), mean(metrics_df$bias_ctree, na.rm = TRUE),
                  mean(metrics_df$bias_rf, na.rm = TRUE),    mean(metrics_df$bias_gmerf, na.rm = TRUE)),
    bias_sd   = c(sd(metrics_df$bias_gmert, na.rm = TRUE), sd(metrics_df$bias_glmm, na.rm = TRUE),
                  sd(metrics_df$bias_logit, na.rm = TRUE), sd(metrics_df$bias_ctree, na.rm = TRUE),
                  sd(metrics_df$bias_rf, na.rm = TRUE),    sd(metrics_df$bias_gmerf, na.rm = TRUE))
  )
  
  list(metrics_df = metrics_df, cv_summary = cv_summary)
}

# ================================================================
# Scenario configurations

# The maximum iterations for GMERT and GMERF are setted to 10 for both loops, 
# that is ridiculus, the algorithm is not expected to converge in so few iterations. 
# However, this is done to speed up the cross-validation runs for demonstration purposes.
K <- 10
seed_folds <- 30
seed_cluster <- 30

scenario_cfg <- list(
  s1 = list(
    glmm_formula  = y ~ x1 + x2 + x3 + (1 + x1 | id),
    logit_formula = y ~ x1 + x2 + x3,
    gmert_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4),
    gmerf_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4)
  ),
  s2 = list(
    glmm_formula  = y ~ x1 + x2 + x3 + (1 + x1 | id),
    logit_formula = y ~ x1 + x2 + x3,
    gmert_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4),
    gmerf_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4)
  ),
  s3 = list(
    glmm_formula  = y ~ x1 + x2 + x3 + x4 + (1 + x1 | id),
    logit_formula = y ~ x1 + x2 + x3 + x4,
    gmert_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4),
    gmerf_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4)
  ),
  s4 = list(
    glmm_formula  = y ~ x1 + x2 + x3 + (1 + x1 | id),
    logit_formula = y ~ x1 + x2 + x3,
    gmert_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4),
    gmerf_args = list(max_iter_out = 10, max_iter_in = 10, tol = 1e-4)
  )
)

# ================================================================
# Setup cluster and run CV
n_workers <- max(1L, min(K, parallel::detectCores() - 1L))
cl <- parallel::makeCluster(n_workers, type = "PSOCK")

# Load packages on workers once
parallel::clusterEvalQ(cl, {
  library(mlml)
  # optional: prevent hidden multithreading issues
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
             MKL_NUM_THREADS = "1", OMP_THREAD_LIMIT = "1")
})

# Export functions once (they come from mlml / global env)
parallel::clusterExport(
  cl,
  varlist = c("cv_one_fold", "f1_fun",
              "fit_gmert_small", "predict_gmert",
              "fit_gmerf_small", "predict_gmerf"),
  envir = environment()
)

# ================================================================
# Run CV for each scenario in parallel

results <- list()

for (nm in names(df_list)) {
  
  df <- df_list[[nm]]
  cfg <- scenario_cfg[[nm]]
  
  # folds once per dataset
  folds <- make_cluster_folds(df, K = K, seed = seed_folds)
  
  # export only the dataset-specific objects each time
  parallel::clusterExport(cl, varlist = c("df", "folds", "K"), envir = environment())
  
  # reset RNG streams so reruns are bit-identical (optional but nice)
  parallel::clusterSetRNGStream(cl, iseed = seed_cluster)
  
  cv_res <- parallel::parLapply(
    cl, X = seq_len(K),
    fun = function(k, glmm_formula, logit_formula, gmert_args, gmerf_args) {
      cv_one_fold(
        k = k, df = df, folds = folds,
        glmm_formula = glmm_formula,
        logit_formula = logit_formula,
        gmert_args = gmert_args,
        gmerf_args = gmerf_args,
        rf_seed = 42
      )
    },
    glmm_formula = cfg$glmm_formula,
    logit_formula = cfg$logit_formula,
    gmert_args = cfg$gmert_args,
    gmerf_args = cfg$gmerf_args
  )
  
  summ <- summarise_cv(cv_res)
  
  results[[nm]] <- list(
    metrics_df = summ$metrics_df,
    cv_summary = summ$cv_summary
  )
  
  cat("Done:", nm, "\n")
}
parallel::stopCluster(cl)


