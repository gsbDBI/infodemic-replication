library(cowplot) # binding plots
library(dataMaid) # write codebook
library(data.table) # rbindlist
library(dplyr) # data manipulation
library(estimatr) #estimation with robust SEs
library(foreign) # read spss
library(ggdist) # distributions
library(ggplot2) # figures
library(gtools) # for smartbind
library(grf) # model prediction
library(kableExtra) # tables
library(modelsummary) # models into tables
library(readr) # reading data in cleaning
library(stringr) # data cleaning
library(tidyr) # gather

# Formatting ####

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

unicode_minus <- function(x){sub('^-', '\U2212', format(x))}


# Estimation & Inference Functions ####

weighted_se <- function(x, w, na.rm=FALSE)
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = sqrt(n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2)))
  return(out)
}

aw_scores <- function(yobs, ws, balwts, K, muhat = FALSE){
  scores <- mumat <- matrix(0, length(yobs), K)
  idx <- matrix(c(1:length(yobs),as.numeric(as.factor(ws))), ncol= 2)
  scores[idx] <- balwts*yobs # Y[t]*W[t]/e[t] term
  if(!isFALSE(muhat) ){ # (1 - W[t]/e[t])*mu[t,w] term
    mumat[idx] <- balwts
    scores <- scores + (1-mumat) * muhat
  }
  return(scores)
}


aw_scores_learn <- function(xs_h, xs_r, 
                            ws_h, ws_r, ws, 
                            yobs, 
                            K_h, K_r, K, 
                            balwts, balwts_r, balwts_h, 
                            probs_r, probs_h, probsK,
                            chunks,...){
  
  muhatR_learn <- causal_forest_muhat_lfo(xs = xs_h, 
                                          ws = ws_r, 
                                          yobs = yobs, 
                                          K = K_r, 
                                          chunks = chunks,
                                          probsK = probsK,
                                          probs = probs_r,
                                          sw = balwts_r)
  
  # for realized treatment assignment in NxK matrix, replace prediction with oob version
  muhatR_learn[[2]][cbind(seq_along(ws), ws)] <- muhatR_learn[[1]][cbind(seq_along(ws), ws_r)]
  
  aipw_scoresR_learn <- aw_scores(yobs, ws, balwts, K, 
                                  muhat = muhatR_learn[[2]])
  
  aipw_scoresRmarg_learn <- sapply(WR_idx, function(x) rowMeans(aipw_scoresR_learn[,x[]]), simplify = 'matrix')
  
  
  muhatH_learn <- causal_forest_muhat_lfo(xs = xs_r, 
                                          ws = ws_h, 
                                          yobs = yobs, 
                                          K = K_h, 
                                          chunks = chunks,
                                          probsK = probsK,
                                          probs = probs_h,
                                          sw = balwts_h)
  
  # for realized treatment assignment in NxK matrix, replace prediction with oob version
  muhatH_learn[[2]][cbind(seq_along(ws), ws)] <- muhatH_learn[[1]][cbind(seq_along(ws), ws_h)]
  
  aipw_scoresH_learn <- aw_scores(yobs, ws, balwts, K, 
                                  muhat = muhatH_learn[[2]])
  
  aipw_scoresHmarg_learn <- sapply(WH_idx, function(x) rowMeans(aipw_scoresH_learn[,x[]]), simplify = 'matrix')
  
  aipw_scores_learn <- list(
    aipw_scoresR_learn,
    aipw_scoresRmarg_learn, 
    aipw_scoresH_learn,
    aipw_scoresHmarg_learn
  )
  return(aipw_scores_learn)
}

aw_scores_eval <- function(xs, yobs, ws, what = NULL, 
                           sample.weights = NULL,...){
  # run multi-causal forest
  mcf <- multi_arm_causal_forest(X = xs,
                                 Y = yobs,
                                 W = ws,
                                 W.hat = what,
                                 sample.weights = sample.weights)
  
  # get scores of treatment effects
  scores_mcf <- get_scores(mcf)
  
  # get score for control condition
  Y_forest <- multi_regression_forest(X = xs, Y = yobs,
                                      sample.weights = sample.weights)
  Y_hat <- predict(Y_forest)$predictions[,1]
  
  W_hat <- mcf$W.hat
  tau_hat <- predict(mcf)$predictions[,,]
  Y_hat_baseline <- Y_hat - rowSums(W_hat[, -1, drop = FALSE] * tau_hat)
  
  scores_control <- yobs*(ws == 1)/W_hat[,1] + # Y[t]*W[t]/e[t] term
    (1 - (ws == 1)/W_hat[,1] )* Y_hat_baseline # (1 - W[t]/e[t])*mu[t,w] term
  
  # save all scores together; first column is control, remaining are TE
  aipw_scores_eval <- cbind(scores_control, scores_mcf[,,1])
  
  return(aipw_scores_eval)
}

# compute mean response
find_est <- function(scores) {
  as.data.frame(t(apply(scores, 2, 
                        function(x){
                          # if not control, add control mean
                          if(!identical(x, scores[, 1])){ 
                            x <- scores[, 1] + x
                          }
                          estimate <- mean(x, na.rm = T)
                          std.error <- sd(x, na.rm = T)/sqrt(length(x))
                          return(c(estimate = estimate,
                                   std.error = std.error,
                                   statistic = estimate/std.error,
                                   # one-sided p-value (less than)
                                   p.value.lower = pt(estimate/std.error, df = length(x)-1, lower = TRUE),
                                   # one-sided p-value (greater than)
                                   p.value.upper = pt(estimate/std.error, df = length(x)-1, lower = FALSE),
                                   # two-sided p-value
                                   p.value.twosided = 2*(pt(abs(estimate/std.error), df = length(x)-1, lower = FALSE))))
                        })))
}


# compute treatment effect
find_te <- function(scores) {
  as.data.frame(t(apply(scores[, -1], 2, 
                        function(x){
                          estimate <- mean(x, na.rm = TRUE)
                          std.error <- sd(x, na.rm = TRUE)/sqrt(length(x))
                          return(c(estimate = estimate,
                                   std.error = std.error,
                                   statistic = estimate/std.error,
                                   # one-sided p-value (less than)
                                   p.value.lower = pt(estimate/std.error, df = length(x)-1, lower = TRUE),
                                   # one-sided p-value (greater than)
                                   p.value.upper = pt(estimate/std.error, df = length(x)-1, lower = FALSE),
                                   # two-sided p-value
                                   p.value.twosided = 2*(pt(abs(estimate/std.error), df = length(x)-1, lower = FALSE))))
                        })))
}

# compute mean response - adjusted
find_adj_est <- function(scores, weights) {
  as.data.frame(t(apply(scores, 2, 
                        function(x){
                          if(!identical(x, scores[, 1])){
                            x <- scores[, 1] + x
                          }
                          estimate <- weighted.mean(x, na.rm = TRUE, w = weights)
                          std.error <- weighted_se(x, na.rm = TRUE, w = weights)
                          return(c(estimate = estimate,
                                   std.error = std.error,
                                   statistic = estimate/std.error,
                                   # one-sided p-value (less than)
                                   p.value.lower = pt(estimate/std.error, df = length(x)-1, lower = TRUE),
                                   # one-sided p-value (greater than)
                                   p.value.upper = pt(estimate/std.error, df = length(x)-1, lower = FALSE),
                                   # two-sided p-value
                                   p.value.twosided = 2*(pt(abs(estimate/std.error), df = length(x)-1, lower = FALSE))))
                        })))
}

# compute treatment effect - adjusted
find_adj_te <- function(scores, weights) {
  as.data.frame(t(apply(scores[, -1], 2, 
                        function(x){
                          estimate <- weighted.mean(x, na.rm = TRUE, w = weights)
                          std.error <- weighted_se(x, na.rm = TRUE, w = weights)
                          return(c(estimate = estimate,
                                   std.error = std.error,
                                   statistic = estimate/std.error,
                                   # one-sided p-value (less than)
                                   p.value_lower = pt(estimate/std.error, df = length(x)-1, lower = FALSE),
                                   # one-sided p-value (greater than)
                                   p.value_upper = 1 - pt(estimate/std.error, df = length(x)-1, lower = FALSE),
                                   # two-sided p-value
                                   p.value_twosided = 2*(pt(abs(estimate/std.error), df = length(x)-1, lower = FALSE))))
                        })))
}

# p-value for two-sided hypotheses
pval <- function(x,y){
  x <- scoresl[[x]]
  y <- scoresl[[y]]
  2*(1-pnorm(abs(mean(x- y, na.rm = TRUE))/(sd(x - y, na.rm = TRUE)/sqrt(length(x)))))
}

# p-value for hypothesis that x is greater than y
pval_ge <- function(x,y){
  x <- scoresl[[x]]
  y <- scoresl[[y]]
  (1-pnorm( mean(x- y, na.rm = TRUE)/(sd(x - y, na.rm = TRUE)/sqrt(length(x))) ))
}


# Bandit Contextual Inference Functions ####

calculate_continuous_X_statistics <- function(h, gammahat, policy){
  # Return bias and variance of policy evaluation via contextual weighting.
  # 
  # INPUT
  # - h: adaptive weights h_t(X_s) of size (T, T)
  # - gammahat: AIPW score of shape [T, K]
  # - policy: policy matrix pi(X_t, w), shape [T, K]
  # - policy_value: ground truth policy value
  # 
  # OUTPUT:
  #   - np.array([bias, var])
  T <- dim(h)[1]
  Z <- colSums(h)  # size (T) \sum_{s=1}^T h_s(X_t)
  gamma_policy <- rowSums(gammahat * policy)
  ht_Xt_Z <- h[cbind(1:T, 1:T)]
  ht_Xt_Z[Z > 1e-6] <- ht_Xt_Z[Z > 1e-6] / Z[Z > 1e-6]  # size (T), h_t(X_t) / Z(X_t)
  B <- ht_Xt_Z * gamma_policy
  h_Z <- h
  h_Z[, Z > 1e-6] <- sweep(h_Z[, Z > 1e-6], 2, Z[Z > 1e-6], `/`)
  
  estimate <- sum(B)
  var <- sum((B - colSums(h_Z*B))^2)
  
  return(c(`estimate` = estimate, `var` = var))
}

output_estimates <- function(policy0 = NULL,
                             policy1,
                             gammahat,
                             contextual_probs){
  # policy0: A * K control policy matrix for contrast evaluation, with probabilities under control
  ## when policy0 = NULL, the function is estimating the value Q(w) of a single arm w
  ## when policy0 doesn't equal to NULL, the function is estimating treatment effects of policies as compared to control \delta(w_1, w_2), using the difference in AIPW scores as the unbiased scoring rule for \delta (w_1, w_2)
  # policy1:counterfactual treatment policy matrix for evaluation
  # gammahat: scores matrix
  # contextual_probs: A * A * K matrix for contextual probabilities, with dimensions representing, time, contexts, treatment arms
  # Use the difference in AIPW scores as the unbiased scoring rule for \delta (w_1, w_2)
  A <- nrow(gammahat)
  
  
  if(is.null(policy0)){
    policy0 <- matrix(0, nrow = A, ncol = ncol(gammahat))
  }
  
  policy <- policy1 - policy0
  
  # Reciprocal of interior of (10) in Zhan et al. 2021
  mask <- matrix(1, nrow = A, ncol = A)
  
  for(i in 2:A){
    mask[i, i:A] <- 0
  }
  
  all_condVars <- sapply(1:A,
                         function(x) rowSums(sweep(1/contextual_probs[,x,],
                                                   MARGIN = 2, policy[x,]^2, `*`)))
  all_condVars_inverse <- matrix(0,
                                 ncol = A,
                                 nrow = A)
  all_condVars_inverse[all_condVars > 1e-6] <- 1 / all_condVars[all_condVars > 1e-6]
  expected_condVars <- rowSums(all_condVars * mask)/rowSums(mask)
  
  expected_condVars_inverse <- expected_condVars
  expected_condVars_inverse[] <- 0
  expected_condVars_inverse[expected_condVars > 1e-6] <- 1 / expected_condVars[expected_condVars > 1e-6]
  
  # contextual stablevar (ldvl X)
  result <- calculate_continuous_X_statistics(sqrt(all_condVars_inverse),
                                              gammahat, policy)
  result['std.error'] <- sqrt(result['var'])
  return(result[c('estimate', 'std.error')])
}



# Forest Functions ####

causal_forest_muhat_lfo <- function(xs, ws, yobs, K, chunks, probs, probsK = NULL,
                                    compute_oob_predictions = TRUE, sw = NULL){
  
  # Fits a sequence of grf::multi_arm_causal_forests sequentially, ensuring that
  # each prediction is only using past information. To be used for constructing
  # doubly-robust scores in an adaptive experiment.
  # Fitting and prediction are made in "chunks", so that predictions for
  # the bth chunk are computed using the first b-1 chunks. Size of chunks
  # is fixed and governed by 'chunks' argument.
  # Chunks need not correspond to batches in an adaptive experiment.
  # Output is a list of length 2. [[1]] is prediction under a single factor, 
  # treating the other factor as observed context. [[2]] is prediction under 
  # unique R x H factorial combination. 
  
  N <- dim(xs)[1]
  if (length(chunks)==1 & is.integer(chunks)){
    timepoints <- seq(chunks, N, chunks)
  } else {
    timepoints <- chunks
  }
  
  t0 <- timepoints[1]
  muhat <- matrix(nrow = N, ncol = K)
  muhatK <- matrix(nrow = N, ncol = 40)
  
  Y_forest <- multi_regression_forest(X = xs[1:t0,], Y = yobs[1:t0], sample.weights = sw[1:t0])
  Y_hat <- predict(Y_forest)$predictions[,1]
  forest <- multi_arm_causal_forest(X = xs[1:t0,], 
                                    Y = yobs[1:t0], 
                                    W = as.factor(ws[1:t0]), 
                                    Y.hat = Y_hat, 
                                    sample.weights = sw[1:t0],
                                    compute.oob.predictions = compute_oob_predictions)
  
  W_hat <- forest$W.hat
  tau_hat <- predict(forest)$predictions[,,]
  Y_hat_baseline <- Y_hat - rowSums(W_hat[, -1, drop = FALSE] * tau_hat)
  
  muhat[1:t0,] <- cbind(Y_hat_baseline, Y_hat_baseline + tau_hat)
  
  
  # if provided probs is for respondent factor only, predict under 
  # counterfactuals for headline factor//
  # if provided probs is for headline factor only, predict under 
  # counterfactuals for respondent factor
  if(!is.null(probsK)){
    
    # prediction under counterfactual headline/respondent
    treat_cols <- grepl(pattern = 'treatment', colnames(xs))
    xs_new <- xs[1:t0,]
    
    probs_altwide <- probsK
    if(ncol(probs) == 8){
      Walt_idx <- WH_idx
      Walt_levels <- WH_levels
    }else{
      Walt_idx <- WR_idx
      Walt_levels <- WR_levels
    }
    
    for(x in Walt_idx){
      probs_altwide[,x] <- probsK[,x]/rowSums(probsK[,x])
    }
    
    W_forest <- probability_forest(xs[1:t0,], as.factor(ws[1:t0]), sample.weights = sw[1:t0])
    
    # predict response under treatment with secondary factor varied across 
    # counterfactual levels
    for(walt_id in 1:length(Walt_idx)){
      # create dummy matrix with just counterfactual factor level
      walt_dummy <- rep(0, sum(treat_cols))
      walt_dummy[walt_id] <- 1
      colidx <- Walt_idx[[walt_id]]
      
      # create new prediction matrix
      xs_new[, treat_cols] <- matrix(walt_dummy, 
                                     nrow = nrow(xs_new), 
                                     ncol = sum(treat_cols), byrow = TRUE)
      
      X_test <- xs_new
      # re-calculate probabilities, based on counterfactual factor
      W_hat_test <- predict(W_forest, X_test)$predictions
      
      Y_hat_test <- predict(Y_forest, X_test)$predictions[, 1]
      tau_hat_test <- predict(forest, X_test)$predictions[,,]
      Y_hat_baseline_test <- Y_hat_test - rowSums(W_hat_test[, -1, drop = FALSE] * tau_hat_test)
      muhatK[1:t0, colidx] <- cbind(Y_hat_baseline_test, Y_hat_baseline_test + tau_hat_test)
    }
  }
  
  for (i in 1:(length(timepoints)-1)){
    t0 <- timepoints[i]+1
    t1 <- timepoints[i+1]
    
    Y_forest <- multi_regression_forest(X = xs[1:t1,], Y = yobs[1:t1], sample.weights = sw[1:t1])
    Y_hat <- predict(Y_forest)$predictions[,1]
    forest <- multi_arm_causal_forest(X = xs[1:t1,], 
                                      W = as.factor(ws[1:t1]),
                                      Y = yobs[1:t1], 
                                      sample.weights = sw[1:t1],
                                      Y.hat = Y_hat,
                                      compute.oob.predictions = compute_oob_predictions)
    
    W_hat <- forest$W.hat
    tau_hat <- predict(forest)$predictions[,,]
    Y_hat_baseline <- Y_hat - rowSums(W_hat[, -1, drop = FALSE] * tau_hat)
    
    muhat[t0:t1,] <- cbind(Y_hat_baseline, Y_hat_baseline + tau_hat)[t0:t1,]
    
    # if provided probs is for respondent factor only, predict under 
    # counterfactuals for headline factor//
    # if provided probs is for headline factor only, predict under 
    # counterfactuals for respondent factor
    if(!is.null(probsK)){
      
      xs_new <- xs[t0:t1,]
      
      W_forest <- probability_forest(xs[1:t1,], as.factor(ws[1:t1]), sample.weights = sw[1:t1])
      
      # predict response under treatment with secondary factor varied across 
      # counterfactual levels
      for(walt_id in 1:length(Walt_idx)){
        
        # create dummy matrix with just counterfactual factor level
        walt_dummy <- rep(0, sum(treat_cols))
        walt_dummy[walt_id] <- 1
        colidx <- Walt_idx[[walt_id]]
        
        # create new prediction matrix
        xs_new[, treat_cols] <- matrix(walt_dummy, 
                                       nrow = nrow(xs_new), 
                                       ncol = sum(treat_cols), byrow = TRUE)
        
        X_test <- xs_new
        W_hat_test <- predict(W_forest, X_test)$predictions
        
        
        Y_hat_test <- predict(Y_forest, X_test)$predictions[, 1]
        tau_hat_test <- predict(forest, X_test)$predictions[,,]
        Y_hat_baseline_test <- Y_hat_test - rowSums(W_hat_test[, -1, drop = FALSE] * tau_hat_test)
        muhatK[t0:t1, colidx] <- cbind(Y_hat_baseline_test, Y_hat_baseline_test + tau_hat_test)
      }
    }
  }
  
  return(list(muhat, muhatK))
}


predict_multi_forest <- function(forests, xs, return_stderr = FALSE){
  N = dim(xs)[1]
  K = length(forests)
  preds <- sapply(forests, function(fr) predict(fr, xs, estimate_variance = return_stderr))
  muhat <- sapply(1:K, function(p) `[[`(preds, p))
  if (return_stderr){
    stderr <- sapply(preds, function(p) p$variance_estimates)
    return (list(muhat = muhat, stderr = stderr))
  } else {
    return (list(muhat = muhat))
  }
}



# Plotting Functions ####

plot_covariate_means_by_group <- function(.df = .df, n_top = 15, 
                                          .title = "Covariate averages within leaf",
                                          rowvars = covariate_names,
                                          ignore.order = FALSE) {
  
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  # Regress each covariate on =subgroup assignment to means p
  cov_means <- lapply(covariate_names, function(covariate) {
    lm_robust(as.formula(paste0(covariate, " ~ 0 + ", "leaf")), data = .df)
  })
  
  covariate_labels <- case_when(covariate_names == 'dli' ~ 
                                  'Digital\nliteracy index',
                                covariate_names == 'pol' ~ 
                                  'Supports\ngoverning party',
                                covariate_names == 'science' ~ 
                                  'Scientific knowledge\nindex',
                                covariate_names == 'age' ~ 
                                  'Age',
                                covariate_names == 'male' ~ 
                                  'Male',
                                TRUE ~ covariate_names)
  
  # Extract the mean and standard deviation of each covariate per subgroup
  cov_means <- lapply(covariate_names, function(covariate) {
    cm_df <- as.data.frame(cbind(
      t(coef(summary(lm_robust(as.formula(paste0(covariate, " ~ 0 + ", "leaf")), data = .df)))[,c("Estimate", "Std. Error")]),
      t(coef(summary(lm_robust(as.formula(paste0(covariate, " ~  + ", "leaf")), data = .df)))[,c("Estimate", "Std. Error")])[,-1]))
    colnames(cm_df)[(length(unique(.df$leaf)) + 1):ncol(cm_df)] <- 'leafDifference'
    cm_df
  })
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate <- gsub('`', '', covariate_names[j])
    label <- covariate_labels[j]
    .mean <- mean(.df[, covariate], na.rm = TRUE)
    .sd <- sd(.df[, covariate], na.rm = TRUE)
    m <- as.matrix(round(signif(cov_means[[j]], digits=4), 3))
    .standardized <- (m["Estimate",1:length(unique(.df$leaf))] - .mean) /.sd
    return(data.frame(covariate = label, 
                      group = substring(colnames(m), 5),
                      estimate = m["Estimate",], se = m["Std. Error",], 
                      standardized = c(.standardized, rep(NA,  length(unique(.df$leaf)) -1))))
  })
  table <- rbindlist(table)
  
  # Preparation to color the chart
  temp_standardized <- sapply(seq_along(covariate_names), function(j) {
    covariate_name <- gsub('`', '', covariate_names[j])
    .mean <- mean(.df[, covariate_name], na.rm = TRUE)
    .sd <- sd(.df[, covariate_name], na.rm = TRUE)
    m <- as.matrix(round(signif(cov_means[[j]], digits=4), 3))
    .standardized <- (m["Estimate",1:length(unique(.df$leaf))] - .mean) / .sd
    .standardized
  })
  
  colnames(temp_standardized) <- covariate_labels
  if(ignore.order){
    ordering <- 1:length(covariate_names)
  }else{
    ordering <- order(apply(temp_standardized, MARGIN = 2, function(x) {.range <- range(x); abs(.range[2] - .range[1])}), decreasing = TRUE) 
  }
  
  
  
  setnames(table, "group", "leaf")
  table[, covariate := factor(covariate, levels = rev(gsub('`', '', covariate_labels)[ordering]), ordered = TRUE)]
  
  if(is.factor(.df$leaf)){
    table$leaf <- factor(table$leaf, levels = c(levels(.df$leaf), 'Difference'))
  }
  
  table$p_val <- 2*(1-pnorm(abs(table$estimate)/table$se ))
  table$stars <- case_when(
    # table$leaf!= 'Difference' ~'',
    # table$p_val < 0.001 ~'***',
    # table$p_val < 0.01 ~'**',
    # table$p_val < 0.05 ~'*',
    # table$p_val < 0.1 ~'+',
    TRUE ~''
  )
  
  table_dat <- table[covariate %in% head(gsub('`', '', covariate_labels)[ordering], n_top)] %>%
    mutate(info = paste0(unicode_minus(estimate), stars, "\n(", se, ")"))

  if(length(unique(.df$leaf))>2){
    table_dat <- table_dat[which(table_dat$leaf!='Difference'),]
  }  
  
  ggplot(table_dat,
         aes(x = leaf, y = covariate)) +
    # Add coloring
    geom_tile(aes(fill = standardized)
              , alpha = 0.9
    )  +
    geom_tile(data = table_dat[which(table_dat$leaf == 'Difference')], 
              aes(x = leaf, y = covariate),
              color = cbPalette[1],
              lwd = 1.5,
              linetype = 1, 
              fill = NA) +
    scale_fill_gradientn(colours = c(cbPalette[3],
                                     '#66bbeb',
                                     '#77c3ed',
                                     # '#88caef',
                                     # '#99d2f1',
                                     # '#aad9f4',
                                     # '#bbe1f6',
                                     # '#cce8f8',
                                     # '#ddf0fa',
                                     '#FFFFFFFF',
                                     # '#faebcc',
                                     # '#f7e2b2',
                                     # '#f5d899',
                                     # '#f2cf7f',
                                     # '#f0c566',
                                     # '#edbb4c',
                                     '#ebb232',
                                     '#e8a819',
                                     cbPalette[2]), 
                         name = 'Standard deviation on\nnormalized distribution',
                         na.value = 'white',
                         limits = c(-0.3, 0.3), 
                         oob = scales::squish) +
    # add numerics
    geom_text(aes(label = info), size=2.5) +
    # reformat
    labs(title = .title,
         y = "Covariate") +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white', color = 'white'))
}





