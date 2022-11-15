library(data.table) #TODO: check if we actually use all of these
library(dplyr)
library(DT)
library(estimatr)
library(ggdist)
library(ggplot2)
library(grf)
library(kableExtra)
library(lmtest)
library(modelsummary)
library(stringr)
library(xtable)
library(dataMaid) # write codebook
library(dplyr) # data manipulation
library(grf) # model prediction
library(readr) # read in files
library(stringr) # data cleaning

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

# Data checks ####

variation_check <- function(df, colname){
  df[[colname]] <- as.factor(df[[colname]])
  ggplot(df, aes_string(x = colname)) + 
    geom_bar() + 
    theme_minimal() + 
    ggtitle(paste0("Distribution of ", colname)) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, vjust = .6),
          panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white', color = 'white'))+
    
    ggsave(paste0("../figures/", colname, ".png"))
}

find.seq.na <- function(r) {
  i <- is.na(r)
  j <- which(i)
  k <- which(!i)
  attr <- j[j > k[length(k)]][1]
  return (attr)
}

# Inference Functions ####

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

aw_estimate <- function(scores, evalwts = NULL){
  if(is.vector(scores)){
    scores <- as.matrix(scores)
  }
  
  if (is.null(evalwts)){
    evalwts <- scores
    evalwts[,] <- 1
  }
  estimates <- scores * evalwts
  estimates[is.na(estimates)] <- 0
  return (colCumsums(estimates) / colCumsums(evalwts))
}

aw_tstat <- function(estimate, stderr){
  out = abs(estimate)/stderr
  out[stderr == 0] = NA
  return (out)
}

aw_relerror <- function(bias, stderr){
  out <- bias/stderr
  out[stderr == 0] <- NA
  return (out)
}

aw_stderr <- function(scores, estimate, evalwts = NULL){
  if(is.vector(scores)){
    scores <- as.matrix(scores)
  }
  # estimate <- colMeans(scores)
  if (is.null(evalwts)){
    evalwts <- scores
    evalwts[,] <- 1
  }
  
  Q <- estimate
  Q2 <- estimate^2
  h2 <- evalwts^2
  
  H <- apply(evalwts, 2, cumsum)
  H2 <- apply(h2, 2, cumsum)
  H2G <- apply(h2 * scores, 2, cumsum)
  H2G2 <- apply(h2 * scores^2, 2, cumsum)
  
  num <- H2G2 - 2 * H2G * Q + Q2 * H2
  std <- sqrt(num) / H
  return(std)
}

se_table <- function(x1, x2, wopt){
  x1 <- t(round(x1, 2))[-1,]
  x2 <- t(sapply(x2, function(x) paste0('(', round(x,2), ')')))[-1,]
  idx <- rep(1:nrow(x1), each = 2)+rep(c(0, nrow(x1)), nrow(x1))
  
  xx <- rbind(x1, x2)[idx,]
  xx <- rbind(xx, table(wopt))
  return(xx)
}


tfunc <- function(scores1, scores2, N){
  return(mean(scores1-scores2)/(sd(scores1-scores2)/sqrt(N)))
}

pwr <- function(scores1, scores2, N){
  return(1 - pnorm(1.64 - mean(scores1-scores2)/( sd(scores1-scores2)/sqrt(N)) ))
}

pwr_multi <- function(scores1, scores2, N, num_compare){
  return(1 - pnorm(1.64 / num_compare - mean(scores1-scores2)/( sd(scores1-scores2)/sqrt(N)) ))
}

aw_stats <- function(scores, mus, evalwts = NULL){
  if(is.vector(scores)){
    scores <- as.matrix(scores)
  }
  
  if (is.null(evalwts)){
    evalwts <- scores
    evalwts[,] <- 1
  }
  
  estimate <- aw_estimate(scores, evalwts)
  bias <- estimate - mus
  stderr <- aw_stderr(scores, estimate, evalwts)
  rlerr <- aw_relerror(bias, stderr)
  cover <- as.numeric(abs(rlerr) < 1.96)
  tstat <- aw_tstat(estimate, stderr)
  power <- as.numeric(abs(tstat) > 1.96)
  sqerr <- bias ^ 2
  return (list(estimate = estimate, stderr = stderr, 
               rlerr = rlerr, cover = cover, 
               tstat = tstat, power = power, sqerr = sqerr))
}

aw_dstderr <- function(scores1, scores2, estimate1, estimate2){
  diff_scores = scores1 - scores2
  diff_estimates = estimate1 - estimate2
  evalwts <- scores1
  evalwts[,] <- 1
  return (aw_stderr(diff_scores, diff_estimates, evalwts))
}

aw_dstats <- function(scores1, scores2, mus1, mus2, evalwts1 = NULL, evalwts2 = NULL){
  if(is.vector(scores1)){
    scores1 <- as.matrix(scores1)
  }
  
  if(is.vector(scores2)){
    scores2 <- as.matrix(scores2)
  }
  
  if (is.null(evalwts1)){
    evalwts1 <- scores1
    evalwts1[,] <- 1
  }
  
  if (is.null(evalwts2)){
    evalwts2 <- scores2
    evalwts2[,] <- 1
  }
  
  estimate1 = aw_estimate(scores1, evalwts1)
  estimate2 = aw_estimate(scores2, evalwts2)
  destimate = estimate1 - estimate2
  
  dmus = mus1 - mus2
  dbias = destimate - dmus
  
  dstderr = aw_dstderr(scores1, scores2, estimate1, estimate2)
  drlerr = aw_relerror(dbias, dstderr)
  dcover <- as.numeric(abs(drlerr) < 1.96)
  dtstat <- aw_tstat(destimate, dstderr)
  dpower <- as.numeric(abs(dtstat) > 1.96)
  dsqerr <- dbias ^ 2
  
  return (list(destimate = destimate, dstderr = dstderr, 
               drlerr = drlerr, dcover = dcover, 
               dtstat = dtstat, dpower = dpower, dsqerr = dsqerr))
}



generate_scores <- function(xs, yobs, ws, probs, WH_idx, WR_idx, design = "static"){
  N <- nrow(xs)
  K <- ncol(probs)
  if(design == 'static'){
    # split the data in half
    folds <- split(1:N, sample(ceiling(seq_along(1:N)/(N/2))))
    h1 <- folds[[1]]
    h2 <- folds[[2]]
    # re-fit muhat so that it's on separate splits of the data
    forest1 <- multi_causal_forest(X = xs[h1,],
                                   Y = yobs[h1],
                                   W = ws[h1],
                                   W.hat = probs[h1,],
                                   min.node.size = 50, 
                                   honesty.prune.leaves = TRUE)
    forest2 <- multi_causal_forest(X = xs[h2,], 
                                   Y = yobs[h2], 
                                   W = ws[h2], 
                                   W.hat = probs[h2,],
                                   min.node.size = 50, 
                                   honesty.prune.leaves = TRUE)
    # Evaluation on separate splits of the data
    muhat <- rbind(conditional_means(forest1),
                   conditional_means(forest2))
  } else { # if adaptive
    h1 = 1:A
    h2 = (A+1):N
    
    forestlfo <- forest_muhat_lfo(xs[h1,], 
                                  ws[h1], 
                                  yobs[h1], 
                                  K, update_times)
    
    forest2 <- multi_causal_forest(X = xs,
                                   Y = yobs,
                                   W = ws,
                                   W.hat = probs)
    
    colnames(forestlfo) <- 1:ncol(probs)
    muhat2 <- as.matrix(conditional_means(forest2))[h2,]
    
    muhat <- rbind(forestlfo,
                   muhat2)
    
  }
  
  balwts <- (1/probs)[cbind(1:N,ws)]
  
  aipw_scores <- aw_scores(yobs, ws, balwts, K=K, muhat=muhat)
  
  aipw_scoresH <- sapply(WH_idx, function(x) rowMeans(aipw_scores[,x[]]), simplify = 'matrix')
  
  aipw_scoresR <- sapply(WR_idx, function(x) rowMeans(aipw_scores[,x[]]), simplify = 'matrix')
  
  scores_df_H <- data.frame(`estimate` = colMeans(aipw_scoresH[h1,]),
                            `se` = apply(aipw_scoresH[h1,], 2, function(x) sd(x)/sqrt(length(x))),
                            check.names = FALSE)
  
  scores_df_R <- data.frame(`estimate` = colMeans(aipw_scoresR[h1,]),
                            `se` = apply(aipw_scoresR[h1,], 2, function(x) sd(x)/sqrt(length(x))),
                            check.names = FALSE)
  
  return (list(muhat = muhat, aipw_scores = aipw_scores, 
               aipw_scoresH = aipw_scoresH, aipw_scoresR = aipw_scoresR,
               df_H = scores_df_H, df_R = scores_df_R))
}

aw_scores_learn <- function(xs_h, xs_r, 
                            ws_h, ws_r, ws, 
                            yobs, 
                            K_h, K_r, K, 
                            balwts, balwts_r, balwts_h, 
                            probs_r, probs_h, probsK,
                            chunks){
  
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


# aw_scores_eval <- function(xs, yobs, ws, balwts, optimal_assignment){
#   muhat_eval <- causal_forest_muhat(xs = xs, yobs = yobs, ws = ws)
#   
#   aipw_scores_eval <- aw_scores(yobs, ws_alt, balwts, length(unique(ws)), 
#                                 muhat = muhat_eval)
#   aipw_scores_eval[,6] <-  aipw_scores_eval[cbind(1:nrow(aipw_scores_eval), optimal_assignment)]
#   
#   return(aipw_scores_eval)
# }

aw_scores_eval <- function(xs, yobs, ws, optimal_assignment, what = NULL, 
                           sample.weights = NULL,...){
  # run multi-causal forest
  mcf <- multi_arm_causal_forest(X = xs,
                                 Y = yobs,
                                 W = ws,
                                 W.hat = what,
                                 sample.weights = sample.weights)
  
  # get scores of treatment effects
  scores_mcf <- get_scores(mcf)
  scores_mcf[,,1][, 5] <- scores_mcf[,,1][cbind(1:nrow(xs), 
                                                optimal_assignment-1)]
  
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

cens_weights <- function(censored, ws, xs){
  # make sure ws are factor
  XX_cens <- model.matrix(cens ~ ., data = data.frame(cens = censored, 
                                                      ws = ws, 
                                                      xs))
  denom_cens <- grf::regression_forest(Y = cens,
                                       X = XX_cens)
  
  ## probability of response
  pd_cens <- 1-predict(denom_cens)[['predictions']]
  
  sw <- 1/pd_cens
  sw
}

treat_cens_weights <- function(censored, ws, xs){
  # make sure ws are factor
  # add missing level
  ws_censored <- interaction(ws, censored)
  
  mcf <- grf::multi_arm_causal_forest(Y = rnorm(n = length(ws_censored)),#ys don't matter
                                      W = ws_censored,
                                      X = xs)
  
  ## probability of response
  pd_cens <- mcf$W.hat[,(1:length(unique(ws)))]
  
  sw <- 1/pd_cens
  sw
}

aw_scores_eval_missing <- function(xs, yobs, ws, optimal_assignment, what, censored, ...){
  ## predict probability of censoring, conditional on treatment and covariates
  sw <- cens_weights(censored = censored,
                     ws = ws,
                     xs = xs)
  
  # run multi-causal forest
  mcf <- multi_arm_causal_forest(X = xs[!censored,],
                                 Y = yobs[!censored],
                                 W = ws[!censored], 
                                 W.hat = what[!censored,], 
                                 sample.weights = sw[!censored])
  
  # get scores of treatment effects
  tau_hat <- predict(mcf, newdata = xs)
  scores_mcf <- get_scores(mcf)
  scores_mcf[,,1][, 5] <- scores_mcf[,,1][cbind(1:nrow(xs[!censored,]), 
                                                optimal_assignment[!censored]-1)]
  
  # get prediction for control condition
  Y_forest <- multi_regression_forest(X = xs[!censored,], 
                                      Y = yobs[!censored],
                                      sample.weights = sw[!censored])
  Y_hat <- predict(Y_forest)$predictions[,1]
  
  tau_hat <- predict(mcf)$predictions[,,]
  Y_hat_baseline <- Y_hat - rowSums(what[!censored, -1, drop = FALSE] * tau_hat)
  
  # save all scores together; first column is control, remaining are TE
  scores_control <- yobs[!censored]*(ws[!censored] == 1)/what[!censored,1] + # Y[t]*W[t]/e[t] term
    (1 - (ws[!censored] == 1)/what[!censored,1] )* Y_hat_baseline # (1 - W[t]/e[t])*mu[t,w] term
  
  # save all scores together; first column is control, remaining are TE
  aipw_scores_eval <- cbind(scores_control, scores_mcf[,,1])
  
  return(aipw_scores_eval)
}


hj <- function(y, bw){
  numer <- ifelse(is.na(y * bw), 0, y * bw)
  # denominator takes 1/assignment probabilities, only if actually assigned
  denom <- sum(bw * !is.na(y))
  est <- sum( numer/denom )
  return(est)
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
estimate <- function(w, gammahat, policy, policy_value){
  # Return bias and variance of policy evaluation via non-contextual weighting.
  # 
  # INPUT
  #     - w: non-contextual weights of shape [T]
  #     - gammahat: AIPW score of shape [T, K]
  #     - policy: policy matrix pi(X_t, w), shape [T, K]
  #     - policy_value: ground truth policy value
  # 
  # OUTPUT
  #     - np.array([bias, var])
  estimate <- aw_estimate(gammahat, policy, w)
  var <- aw_var(gammahat, estimate, policy, w)
  bias <- estimate - policy_value
  
  return(c(`estimate` = estimate, `bias` = bias, `var` = var))
}

aw_estimate <- function(scores, policy, evalwts=NULL){
  # Estimate policy value via non-contextual adaptive weighting.
  # 
  # INPUT
  #     - scores: AIPW score, shape [T, K]
  #     - policy: policy matrix pi(X_t, w), shape [T, K]
  #     - evalwts: non-contextual adaptive weights h_t, shape [T]
  # OUTPUT
  #     - estimated policy value.
  if(is.null(evalwts)){
    evalwts <- matrix(1, nrow = nrow(scores))
  }
  
  return(sum(evalwts*rowSums(scores * policy))/sum(evalwts))
}

aw_var <- function(scores, estimate, policy, evalwts=NULL){
  # Variance of policy value estimator via non-contextual adaptive weighting.
  # 
  # INPUT
  #     - scores: AIPW score, shape [T, K]
  #     - estimate: policy value estimate
  #     - policy: policy matrix pi(X_t, w), shape [T, K]
  #     - evalwts: non-contextual adaptive weights h_t, shape [T]
  # OUTPUT
  #     - variance of policy value estimate
  # 
  # var =  sum[t=0 to T] h[t]^2 * (sum[w] scores[t, w] * policy[t, w] - estimate)^2 
  #   __________________________________________________________________________
  #                       (sum[t=0 to T] h[t])^2
  
  if(is.null(evalwts)){
    evalwts <- matrix(1, nrow = nrow(scores))
  }
  
  return(sum((rowSums(scores * policy)-estimate)^2*evalwts^2)/sum(evalwts)^2)
}

calculate_continuous_X_statistics <- function(h, gammahat, policy, policy_value){
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
  bias <- estimate - policy_value
  
  return(c(`estimate` = estimate, `bias`= bias, `var` = var))
}

contextual_estimates <- function(id0 = NULL, idA, gammahat, contextual_probs, 
                                 policy_value=0){
  # id0: location of control policy (can be NULL if not estimating treatment effect)
  # idA list length >=1 with locations of counterfactual treatments
  # gammahat: scores matrix
  # contextual probabilities: T * T * K matrix for time, contexts, treatment arms
  
  A <- nrow(gammahat)
  
  results <- vector(mode = "list", length = length(idA))
  
  for(j in 1:length(idA)){
    
    x <- idA[[j]]
    
    # Define policies
    policy0 <- policy1 <- matrix(0, nrow = A, ncol = ncol(gammahat))
    policy0[, id0] <- 1/length(id0)
    policy1[, x] <- 1/length(x)
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
    
    # using stablevar version
    result <- calculate_continuous_X_statistics(sqrt(all_condVars_inverse), 
                                                gammahat, policy, policy_value=policy_value)
    
    result['std.error'] <- sqrt(result['var'])
    results[[j]] <- result[c('estimate', 'std.error')]
  }
  return(results)
}




# Ridge Functions ####

ridge_muhat_cf <- function(xs, ws, yobs, K, nfolds=5){
  N <- nrow(xs) 
  muhat <- matrix(NA, N, K)
  
  for(w in 1:K){
    # Predict on obs that *were not* assigned w
    widx <- which(ws == w)
    oidx <- which(ws != w)
    
    lambda <- cv.glmnet(x = xs[widx,], y = yobs[widx], nfold = 5)$lambda.min
    ridgefit <- glmnet(x = xs[widx,], y = yobs[widx], lambda = lambda)
    muhat[oidx, w] <- predict(ridgefit, newx = xs[oidx,])
    
    # Cross-fit on obs that *were* assigned to w
    folds <- split(sample(widx), ceiling(seq_along(widx)/ (length(widx)/nfolds) ) )
    for(f in 1:nfolds){
      wfold <- folds[[f]]
      wnonfold <- unlist(folds[-f])
      ridgefitnonf <- glmnet(x = xs[wnonfold,], y = yobs[wnonfold], lambda = lambda)
      muhat[wfold, w] <- predict(ridgefit, newx = xs[wfold,])
    }
  }
  return(muhat)
}

ridge.search <- function( pf ){
  p.fac <- rep(c(0, 1, pf), times = omegas)
  modelcv <- cv.glmnet(xx, yy, alpha = 0, 
                       penalty.factor = p.fac, 
                       foldid = foldvals,
                       weights = ww)
  mse.min <- modelcv$cvm[modelcv$lambda == modelcv$lambda.min]
  return(mse.min)
}

eval.g0 <- function( x ) {
  return(  c(-x[1]+1, -x[2]+x[1]) )
}



# Forest Functions ####


forest_muhat_lfo <- function(xs, ws, yobs, K, chunks, factor = FALSE, sw = NULL){
  
  # Fits a sequence of grf::regression_forests sequentially, ensuring that
  # each prediction is only using past information. To be used for constructing
  # doubly-robust scores in an adaptive experiment.
  # Fitting and prediction are made in "chunks", so that predictions for
  # the bth chunk are computed using the first b-1 chunks. Size of chunks
  # is fixed and governed by 'chunks' argument.
  # Chunks need not correspond to batches in an adaptive experiment.
  
  N <- dim(xs)[1]
  if (length(chunks)==1 & is.integer(chunks)){
    timepoints <- seq(chunks, N, chunks)
  } else {
    timepoints <- chunks
  }
  
  t0 <- timepoints[1]
  muhat <- matrix(nrow = N, ncol = K)
  if(factor){
    muhatK <- matrix(nrow = N, ncol = 40)
  }
  if(!is.null(sw)){
    swx <- sw[1:t0]
  } else {
    swx <- sw
  }
  forest <- fit_multi_forest(xs[1:t0,], ws[1:t0], yobs[1:t0], K = K, sw = swx)  
  
  muhat[1:t0,] <- predict_multi_forest_oob(forest)
  
  if(factor){
    
    # prediction under counterfactual headline
    treat_cols <- grepl(pattern = 'treatment', colnames(xs))
    xs_new <- xs[1:t0,]
    
    # predict under each potential headline assignment
    for(wh_id in 1:5){
      # identify the relevant headline level
      wh <- substring(WH_levels[wh_id], 
                      first = 3)
      wh_dummy <- rep(0, sum(treat_cols))
      wh_dummy[wh_id] <- 1
      
      # create new prediction matrix
      xs_new[, treat_cols] <- matrix(wh_dummy, 
                                     nrow = nrow(xs_new), 
                                     ncol = sum(treat_cols), byrow = TRUE)
      X.test <- xs_new
      
      # for each regression forest, predict under relevant headline
      for(w in 1:K){
        colidx <- WH_idx[[wh_id]][w]
        muhatK[1:t0, colidx] <- predict(forest[[w]], X.test)$predictions
      }
    }
  }
  
  for (i in 1:(length(timepoints)-1)){
    t0 <- timepoints[i]+1
    t1 <- timepoints[i+1]
    
    if(!is.null(sw)){
      swx <- sw[1:t1]
    } else {
      swx <- sw
    }
    
    forest <- fit_multi_forest(xs[1:t1,], ws[1:t1], yobs[1:t1], K = K, sw = swx) 
    
    muhat[t0:t1,] <- predict_multi_forest_oob(forest)[t0:t1,]
    
    if(factor){
      
      # prediction under counterfactual headline
      xs_new <- xs[t0:t1,]
      
      # predict under each potential headline assignment
      for(wh_id in 1:5){
        # identify the relevant headline level
        wh <- substring(WH_levels[wh_id], 
                        first = 3)
        wh_dummy <- rep(0, sum(treat_cols))
        wh_dummy[wh_id] <- 1
        
        # create new prediction matrix
        xs_new[, treat_cols] <- matrix(wh_dummy, 
                                       nrow = nrow(xs_new), 
                                       ncol = sum(treat_cols), byrow = TRUE)
        X.test <- xs_new
        
        # for each regression forest, predict under relevant headline
        for(w in 1:K){
          colidx <- WH_idx[[wh_id]][w]
          muhatK[t0:t1, colidx] <- predict(forest[[w]], X.test)$predictions
        }
      }
    }
    
  }
  
  if(factor){
    return(list(muhat, muhatK))
  }else{
    return(muhat) 
  }
}

fit_multi_forest <- function(xs, ws, yobs, K, compute_oob_predictions = TRUE, sw = NULL){
  N <- dim(xs)[1]
  forests <- vector("list",K)
  for (w in 1:K){
    widx <- which(ws == w)
    oidx <- which(ws != w)
    if(!is.null(sw)){
      swx <- sw[widx]
    } else {
      swx <- sw
    }
    forests[[w]] <- fr <- grf::regression_forest(X = xs[widx,], Y = yobs[widx], 
                                                 compute.oob.predictions = compute_oob_predictions, sample.weights = swx)
    # Keep these indices if the forest is used for cross-fitting.
    if (compute_oob_predictions){
      
      forests[[w]]$widx = widx
      forests[[w]]$oidx = oidx
      
      wpred = predict(fr, estimate.variance=TRUE)
      opred = predict(fr, xs[oidx,], estimate.variance=TRUE)
      
      forests[[w]]$oob_predictions = rep(0, N)
      forests[[w]]$oob_predictions[widx] = wpred$predictions
      forests[[w]]$oob_predictions[oidx] = opred$predictions
      
      forests[[w]]$oob_stderr = rep(0, N)
      forests[[w]]$oob_stderr[widx] = sqrt(wpred$variance.estimates)
      forests[[w]]$oob_stderr[oidx] = sqrt(opred$variance.estimates)
    }
  }
  return (forests)
}


fit_multi_forest_factor <- function(xs, ws, yobs, K, compute_oob_predictions = TRUE, sw = NULL){
  N <- dim(xs)[1]
  forests <- vector("list",K)
  
  # for computing predictions
  xs_new <- xs
  treat_cols <- grepl(pattern = 'treatment', colnames(xs))
  xs_new[, treat_cols] <- matrix(c(1, rep(0, sum(treat_cols) -1 )), 
                                 nrow = N, 
                                 ncol = sum(treat_cols), byrow = TRUE)
  for (w in 1:K){
    widx <- which(ws == w)
    oidx <- which(ws != w)
    if(!is.null(sw)){
      swx <- sw[widx]
    } else {
      swx <- sw
    }
    forests[[w]] <- fr <- grf::regression_forest(X = xs[widx,], Y = yobs[widx], 
                                                 compute.oob.predictions = compute_oob_predictions, sample.weights = swx)
    # Keep these indices if the forest is used for cross-fitting.
    if (compute_oob_predictions){
      
      forests[[w]]$widx = widx
      forests[[w]]$oidx = oidx
      
      # note on-assignment predictions are no longer oob
      wpred = predict(fr, xs_new[widx,], estimate.variance=TRUE)
      opred = predict(fr, xs_new[oidx,], estimate.variance=TRUE)
      
      forests[[w]]$oob_predictions = rep(0, N)
      forests[[w]]$oob_predictions[widx] = wpred$predictions
      forests[[w]]$oob_predictions[oidx] = opred$predictions
      
      forests[[w]]$oob_stderr = rep(0, N)
      forests[[w]]$oob_stderr[widx] = sqrt(wpred$variance.estimates)
      forests[[w]]$oob_stderr[oidx] = sqrt(opred$variance.estimates)
    }
  }
  return (forests)
}

causal_forest_muhat <- function(xs, yobs, ws){
  
  mcf <- multi_arm_causal_forest(X = xs,
                                 Y = yobs,
                                 W = ws)
  
  Y_forest <- multi_regression_forest(X = xs, Y = yobs)
  Y_hat <- predict(Y_forest)$predictions[,1]
  
  W_hat <- mcf$W.hat
  tau_hat <- predict(mcf)$predictions[,,]
  Y_hat_baseline <- Y_hat - rowSums(W_hat[, -1, drop = FALSE] * tau_hat)
  
  muhat <- cbind(Y_hat_baseline, Y_hat_baseline + tau_hat)
  return(muhat)
}



causal_forest_muhat_lfo <- function(xs, ws, yobs, K, chunks, probs, probsK = NULL,
                                    compute_oob_predictions = TRUE, sw = NULL){
  
  # Fits a sequence of grf::multi_arm_causal_forests sequentially, ensuring that
  # each prediction is only using past information. To be used for constructing
  # doubly-robust scores in an adaptive experiment.
  # Fitting and prediction are made in "chunks", so that predictions for
  # the bth chunk are computed using the first b-1 chunks. Size of chunks
  # is fixed and governed by 'chunks' argument.
  # Chunks need not correspond to batches in an adaptive experiment.
  # Output is a list of length 1. [[1]] is prediction under a single factor, 
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



greedy_forestpolicy_model <- function(xt, model){
  N = dim(xt)[1]
  muhat = predict_multi_forest(model$forests, xt)
  w = apply(muhat, 2, which.max)
  ps = matrix(0, nrow = N, ncol = model$K)
  ps[1:N, w] = 1
  return (list(w = w, ps = ps))
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

predict_multi_forest_oob <- function(forests, return_stderr = FALSE){
  predictions = do.call(cbind, lapply(1:length(forests), function(x) forests[[x]]$oob_predictions))
  if (return_stderr){
    variances = do.call(cbind, lapply(1:length(forests), function(x) forests[[x]]$oob_stderr))
    return (list(predictions = predictions, variances = variances))
  } else {
    return (predictions)
  }
}


# Tree Functions ####

predict_policy_tree <- function(pol, xtest){
  w <-predict(pol, xtest)
  return (w)
}

# Helper Functions ####

collect <- function(a, indices){
  if (!is.vector(indices)){
    print("indices not 1-dim")
    print("failed to collect")
  } else {
    l = ifelse(is.null(dim(a)), length(a), dim(a)[1])
    rows = 1:l
    a <- data.frame(a)
    out = a[matrix(c(rows, indices), ncol = 2)]
    return (out)
  }
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
                                  'Digital\nLiteracy Index',
                                covariate_names == 'hhi' ~ 
                                  'Household\nWealth Index',
                                covariate_names == 'nigeria' ~ 
                                  'Country==Nigeria',
                                covariate_names == 'pol' ~ 
                                  'Political Allegiance\nw/ Governing Party',
                                covariate_names == 'science' ~ 
                                  'Scientific Beliefs\nIndex',
                                covariate_names == 'Y_pre' ~ 
                                  'Pre-treatment\nResponse',
                                covariate_names == 'urban' ~ 
                                  'Location==Urban',
                                covariate_names == 'locus' ~ 
                                  'Locus\nof Control',
                                covariate_names == 'ed' ~ 
                                  'Education\nLevel',
                                covariate_names == 'denom_pentecostal' ~ 
                                  'Pentecostal\nChristian',
                                covariate_names == 'cash' ~ 
                                  'Cash Income',
                                covariate_names == 'religiosity' ~ 
                                  'Frequency of\nReligious Practice',
                                covariate_names == 'rel_christian' ~ 
                                  'Christian',
                                covariate_names == 'rel_muslim' ~ 
                                  'Muslim',
                                covariate_names == 'hh' ~ 
                                  'Household Size',
                                covariate_names == 'fb_post' ~ 
                                  'Frequency of\nFacebook Posting',
                                covariate_names == 'fb_msg' ~ 
                                  'Frequency of\nFacebook Messaging',
                                covariate_names == 'crt' ~ 
                                  'Cognitive\nReflection Test',
                                covariate_names == 'cov_efficacy' ~ 
                                  'COVID\nPolicy Approval',
                                covariate_names == 'age' ~ 
                                  'Age',
                                covariate_names == 'male' ~ 
                                  'Male',
                                covariate_names == 'cov_concern' ~ 
                                  'Concern About\nCOVID-19',
                                TRUE ~ covariate_names)
  
  # Extract the mean and standard deviation of each covariate per subgroup
  cov_means <- lapply(covariate_names, function(covariate) {
    as.data.frame(cbind(
      t(coef(summary(lm_robust(as.formula(paste0(covariate, " ~ 0 + ", "leaf")), data = .df)))[,c("Estimate", "Std. Error")]),
      'leafDifference' = t(coef(summary(lm_robust(as.formula(paste0(covariate, " ~  + ", "leaf")), data = .df)))[,c("Estimate", "Std. Error")])[,2]))
  })
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate <- gsub('`', '', covariate_names[j])
    label <- covariate_labels[j]
    .mean <- mean(.df[, covariate], na.rm = TRUE)
    .sd <- sd(.df[, covariate], na.rm = TRUE)
    m <- as.matrix(round(signif(cov_means[[j]], digits=4), 3))
    .standardized <- (m["Estimate",1:2] - .mean) / .sd
    .standardized_mean <- m["Estimate",3]/m["Std. Error",3]
    return(data.frame(covariate = label, 
                      group = substring(colnames(m), 5),
                      estimate = m["Estimate",], se = m["Std. Error",], 
                      standardized = c(.standardized, NA)))
  })
  table <- rbindlist(table)
  
  # Preparation to color the chart
  temp_standardized <- sapply(seq_along(covariate_names), function(j) {
    covariate_name <- gsub('`', '', covariate_names[j])
    .mean <- mean(.df[, covariate_name], na.rm = TRUE)
    .sd <- sd(.df[, covariate_name], na.rm = TRUE)
    m <- as.matrix(round(signif(cov_means[[j]], digits=4), 3))
    .standardized <- (m["Estimate",1:2] - .mean) / .sd
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
  
  table_dat <- table[covariate %in% head(gsub('`', '', covariate_labels)[ordering], n_top)] %>%
    mutate(info = paste0(estimate, "\n(", se, ")"))
  
  
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
                                     '#FFFFFFFF',
                                     cbPalette[2]), 
                         name = 'Standard Deviation on\nNormalized Distribution',
                         na.value = 'white') +
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



plot_score_means_by_group <- function(.df = .df, n_top = 15, 
                                      .title = "Score averages within leaf",
                                      rowvars = covariate_names,
                                      ignore.order = TRUE) {
  
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  # Regress each covariate on =subgroup assignment to means p
  cov_means <- lapply(covariate_names, function(covariate) {
    as.data.frame(cbind(
      t(coef(summary(lm_robust(as.formula(paste0(covariate, " ~ 0 + ", "leaf")), data = .df)))[,c("Estimate", "Std. Error")]),
      'leafDifference' = t(coef(summary(lm_robust(as.formula(paste0(covariate, " ~  + ", "leaf")), data = .df)))[,c("Estimate", "Std. Error")])[,2]))
  })
  
  # Preparation to color the chart
  temp_standardized <- sapply(seq_along(covariate_names), function(j) {
    # covariate_name <- gsub('`', '', covariate_names[j])
    # .mean <- mean(.df[, covariate_name], na.rm = TRUE)
    # .sd <- sd(.df[, covariate_name], na.rm = TRUE)
    as.matrix(cov_means[[j]])['Estimate',]
    # m <- as.matrix(round(signif(cov_table[[j]], digits=4), 3))
    # .standardized <- (m["Estimate",] - .mean) / .sd
    # .standardized
  })
  
  
  temp_standardized <- sapply(cov_table, function(x) as.numeric(x['Estimate',]))
  temp_standardized <- t(apply(temp_standardized, 1, function(x) (x-mean(x))/sd(x)))
  
  colnames(temp_standardized) <- covariate_names
  rownames(temp_standardized) <- colnames(cov_table[[1]])
  
  color_scale <- max(abs(c(max(temp_standardized, na.rm = TRUE), min(temp_standardized, na.rm = TRUE))))
  color_scale <- color_scale * c(-1,1)
  max_std_dev <- floor(max(color_scale))
  breaks <- c(-max_std_dev, max_std_dev)
  labels <- c(-1, 1)
  
  
  # Little trick to display the standard errors
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate <- gsub('`', '', covariate_names[j])
    m <- as.matrix(round(signif(cov_table[[j]], digits=4), 3))
    .standardized <- temp_standardized[, j]
    return(data.frame(covariate = covariate, 
                      group = substring(colnames(m), 5),
                      estimate = m["Estimate",], se = m["Std. Error",], 
                      standardized = .standardized))
  })
  table <- rbindlist(table)
  
  setnames(table, "group", "leaf")
  table[, covariate := factor(covariate, levels = rev(gsub('`', '', covariate_names)), ordered = TRUE)]
  
  if(is.factor(.df$leaf)){
    table$leaf <- factor(table$leaf, levels = levels(.df$leaf))
  }
  
  table[covariate %in% head(gsub('`', '', covariate_names), n_top)] %>%
    mutate(info = paste0(estimate, "\n(", se, ")")) %>%
    ggplot(aes_string(y = "leaf", x = "covariate")) +
    scale_y_discrete(limits=rev) +
    scale_x_discrete(limits = rev, position = "top") +
    # Add coloring
    geom_raster(aes(fill = standardized)
                , alpha = 0.9
    ) +
    scale_fill_gradientn(colours = c(cbPalette[3],
                                     '#FFFFFFFF',
                                     cbPalette[2]), 
                         name = 'Within row variation') +
    # add numerics
    geom_text(aes(label = info), size=2.5) +
    # reformat
    labs(title = .title,
         y = "Assignment under Optimal Policy",
         x = "Counterfactual Mean") +
    theme_minimal() +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white', color = 'white'))
}


# evaluate_partial_dependency <- function(var_of_interest, is_binary) {
#   if(is_binary){
#     # Get two unique values for the variable
#     x_grid <- sort(unique(df_train[,var_of_interest]))
#   } else {
#     # Get quantile values for the variable
#     x_grid <- quantile(df_train[,var_of_interest], probs = seq(0, 1, length.out = 5))
#   }
#   df_grid <- setNames(data.frame(x_grid), var_of_interest)
#   
#   # For the other variables, keep them at their median
#   other_covariates <- covariate_names[which(covariate_names != var_of_interest)]
#   df_median <- data.frame(lapply(df_train[,other_covariates], median))
#   df_eval <- expand_grid(df_median, df_grid)
#   
#   # Predict the treatment effect
#   pred <- predict(cf, newdata=df_eval[,covariate_names], estimate.variance=TRUE)
#   rbind('Tau Hat' = pred$predictions,
#         'Std. Error' = sqrt(pred$variance.estimates))
# }

hte_lin_tab_two <- function(factor, value1, value0, rowvars = covariate_names){
  covariate_names <- rowvars
  # environment(evaluate_partial_dependency) <- environment()
  
  df_train <- bind_rows(value0 = df_treat, 
                        value1 = df_treat, .id = 'level') %>% 
    mutate(Scores = case_when(level == 'value0' ~ !!sym(paste0(toupper(substr(factor, 11, 11)),
                                                               '_', value0, '_scores')),
                              level == 'value1' ~ !!sym(paste0(toupper(substr(factor, 11, 11)), '_', value1, '_scores'))))
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    
    if (length(unique(df_train[, covariate_name]))<=2 ){
      df_train <- df_train %>% 
        mutate(groups = factor(case_when((level == 'value0') & (get(covariate_name)==0)~1,
                                         (level == 'value1') & (get(covariate_name)==0)~2,
                                         (level == 'value0') & (get(covariate_name)==1)~3,
                                         (level == 'value1') & (get(covariate_name)==1)~4),
                               levels = 1:4))
      
      coef_newnames <- c(
        paste0(stringr::str_to_sentence(value0), ', not', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', not', covariate_name),
        paste0(stringr::str_to_sentence(value0), ', ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', ', covariate_name))
    }else{
      df_train <- df_train %>% 
        mutate(groups = factor(case_when((level == 'value0') & 
                                           (get(covariate_name)<= median(df_treat[,covariate_name]))~1,
                                         (level == 'value1') & 
                                           (get(covariate_name)<= median(df_treat[,covariate_name]))~2,
                                         (level == 'value0') & 
                                           (get(covariate_name)> median(df_treat[,covariate_name]))~3,
                                         (level == 'value1') & 
                                           (get(covariate_name)> median(df_treat[,covariate_name]))~4),
                               levels = 1:4))
      
      coef_newnames <- c(
        paste0(stringr::str_to_sentence(value0), ', below median ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', below median ', covariate_name),
        paste0(stringr::str_to_sentence(value0), ', above median ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', above median ', covariate_name))
    }
    
    lm0 <- lm_robust(Scores ~-1 + groups, data = df_train)
    plot_dat <- tidy(lm0)
    plot_dat$term <- coef_newnames[1:length(plot_dat$term)]
    ggplot(plot_dat, aes(x = estimate, moe = std.error, y = term)) +
      stat_confidence_density(fill = cbPalette[3], height = 0.8, confidence = 0.68) +
      geom_point(aes(x = estimate), size = 2) +
      geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.05) +
      ylab('Group') + 
      xlab('Estimate') +
      scale_y_discrete(limits = coef_newnames[1:length(plot_dat$term)])+
      theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    ggsave(paste0('../figures/', paste(c(value0, value1, covariate_name), collapse = '_'), '.png'))
    
    m <- round(signif(rbind('Tau Hat' = lm0$coefficients, 
                            'Std. Error' = lm0$std.error), digits=4), 3)
    m["Tau Hat",] <- as.character(m["Tau Hat",])
    m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
    
    if(dim(m)[2]==4){
      m <- cbind(m, c(sprintf("%.4f", round(lm0$p.value[4],4)), ''))
      m["Tau Hat",5] <- cell_spec(m["Tau Hat",5], 
                                  color = "White", 
                                  background = spec_color(lm0$p.value[4],
                                                          scale_from = c(0,1))) 
    } else {
      m <- cbind(m, matrix('', nrow = 2, ncol = 3))
    }
    m
  })
  
  order_list <- lapply(table, function(x) {
    if(dim(x)[2]>=5){
      x = x['Tau Hat',5]
      gsub('</span>', '', regmatches(x, gregexpr('[0-9]*\\.*[0-9]*</span>$', x))) 
    } else {
      999
    }
  } )
  ordering <- order(unlist(order_list))
  table <- table[ordering]
  
  table <- do.call(rbind, table)
  
  colnames(table) <- c('(Intercept)', stringr::str_to_title(value1), 'Covariate', 
                       'Interaction', 'Interaction p.value')
  # Covariate names
  covnames <- rep("", nrow(table))
  covnames[seq(1, length(covnames), 2)] <-
    cell_spec(covariate_names[ordering], format = "html", 
              escape = FALSE, color = "black", bold= TRUE)
  table <- cbind(covariates=covnames, table)
  # Title of table
  caption <- paste0("Interaction of treament and an indicator for covariates (above median if nonbinary);  \n Treatment effect is the difference between the ",
                    ifelse(factor == "treatment_r", "respondent level ", "headline level "), 
                    value1, 
                    " treatment, and the baseline ", value0, " treatment.")
  table <- table %>%
    kable(format="html", digits=2, caption=caption, escape = FALSE, 
          row.names = FALSE) %>%
    kable_styling(bootstrap_options=c("condensed", "responsive"), 
                  full_width=FALSE)
  
  return(table)
}


hte_tab_diff <- function(factor, value1, value0,
                         n_top = 15, rowvars = covariate_names){
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  # environment(evaluate_partial_dependency) <- environment()
  
  df_train <- bind_rows(value0 = df_treat[h1,], 
                        value1 = df_treat[h1,], .id = 'level') %>% 
    mutate(Scores = case_when(level == 'value0' ~ !!sym(paste0(toupper(substr(factor, 11, 11)),
                                                               '_', value0, '_scores')),
                              level == 'value1' ~ !!sym(paste0(toupper(substr(factor, 11, 11)), '_', value1, '_scores'))))
  
  df_train2 <- df_treat[h1,] %>% 
    mutate(diff_scores = get(paste0(toupper(substr(factor, 11, 11)),
                                    '_', value1, '_scores'))-
             get(paste0(toupper(substr(factor, 11, 11)),
                        '_', value0, '_scores')))
  
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    
    if (length(unique(df_train[, covariate_name]))<=2 ){
      df_train <- df_train %>% 
        mutate(groups = factor(case_when((level == 'value0') & (get(covariate_name)==0)~1,
                                         (level == 'value1') & (get(covariate_name)==0)~2,
                                         (level == 'value0') & (get(covariate_name)==1)~3,
                                         (level == 'value1') & (get(covariate_name)==1)~4),
                               levels = 1:4))
      
      coef_newnames <- c(
        paste0(stringr::str_to_sentence(value0), ', not ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', not ', covariate_name),
        paste0(stringr::str_to_sentence(value0), ', ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', ', covariate_name))
    }else{
      df_train <- df_train %>% 
        mutate(groups = factor(case_when((level == 'value0') & 
                                           (get(covariate_name)<= median(df_treat[,covariate_name]))~1,
                                         (level == 'value1') & 
                                           (get(covariate_name)<= median(df_treat[,covariate_name]))~2,
                                         (level == 'value0') & 
                                           (get(covariate_name)> median(df_treat[,covariate_name]))~3,
                                         (level == 'value1') & 
                                           (get(covariate_name)> median(df_treat[,covariate_name]))~4),
                               levels = 1:4))
      
      coef_newnames <- c(
        paste0(stringr::str_to_sentence(value0), ', below median ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', below median ', covariate_name),
        paste0(stringr::str_to_sentence(value0), ', above median ', covariate_name),
        paste0(stringr::str_to_sentence(value1), ', above median ', covariate_name))
    }
    
    lm0 <- lm_robust(Scores ~-1 + groups, data = df_train)
    plot_dat <- tidy(lm0)
    plot_dat$term <- coef_newnames[1:length(plot_dat$term)][1:length(plot_dat$term)]
    ggplot(plot_dat, aes(x = estimate, moe = std.error, y = term)) +
      stat_confidence_density(fill = cbPalette[3], height = 0.8, confidence = 0.68) +
      geom_point(aes(x = estimate), size = 2) +
      geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.05) +
      ylab('Group') + 
      xlab('Estimate') + 
      labs(title = paste(stringr::str_to_title(value1), 'vs.', stringr::str_to_title(value0)),
           subtitle = paste('by', covariate_name)) +
      scale_y_discrete(limits = coef_newnames[1:length(plot_dat$term)])+
      theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    suppressMessages(ggsave(paste0('../figures/', paste(c(value0, value1, covariate_name), collapse = '_'), '.png')))
    
    
    if(length(unique(df_train2[, covariate_name]))<=2 ){
      df_train2 <- df_train2 %>%
        mutate(groups = factor(case_when(get(covariate_name)==0~1,
                                         get(covariate_name)==1~2)))
    } else{
      df_train2 <- df_train2 %>% 
        mutate(groups = factor(case_when(get(covariate_name)<= median(get(covariate_name))~1,
                                         get(covariate_name)> median(get(covariate_name))~2)))
    }
    
    if(length(unique(df_train2$groups))==1){
      lm1 <-lm_robust(diff_scores ~ 1, data = df_train2)
    } else {
      lm1 <-lm_robust(diff_scores ~ groups, data = df_train2)
    }
    
    
    m <- round(signif(rbind('Tau Hat' = lm1$coefficients, 
                            'Std. Error' = lm1$std.error), digits=4), 3)
    m["Tau Hat",] <- as.character(m["Tau Hat",])
    m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
    
    if(dim(m)[2]==2){
      value_diff <- (prod(sign(lm1$coefficients))==-1)*(abs(lm1$coefficients[2])>abs(lm1$coefficients[1]))
      m <- cbind(m, matrix(c(sprintf("%.4f", round(lm1$p.value[2],4)), '', value_diff, '') , ncol = 2))
      m["Tau Hat",3] <- cell_spec(m["Tau Hat",3],
                                  color = "White",
                                  background = spec_color(ifelse(value_diff, lm1$p.value[2], 1),
                                                          scale_from = c(0,1)))
    } else {
      m <- cbind(m, matrix('', nrow = 2, ncol = 3))
    }
    m
  })
  
  order_list <- lapply(table, function(x) {
    if(dim(x)[2]>=3){
      x1 <- x['Tau Hat',3]
      x2 <- suppressWarnings(as.numeric(x['Tau Hat',4]))
      x1 <- suppressWarnings(as.numeric(gsub('</span>', '', regmatches(x1, gregexpr('[0-9]*\\.*[0-9]*</span>$', x1)))))
      ifelse(x2, -1 -x1*-1, x1)
    } else {
      999
    }
  } )
  ordering <- order(unlist(order_list))
  table <- table[ordering]
  
  table <- do.call(rbind, table)
  table <- table[, 1:3]
  
  colnames(table) <- c('Baseline/below-median ATE', 'Difference for indicator/above-median ATE', 
                       'Difference in treatment effects p.value')
  # Covariate names
  covnames <- rep("", nrow(table))
  covnames[seq(1, length(covnames), 2)] <-
    cell_spec(covariate_names[ordering], format = "html", 
              escape = FALSE, color = "black", bold= TRUE)
  table <- cbind(covariates=covnames, table)
  table <- table[1:(n_top*2),]
  # Title of table
  caption <- paste0("Interaction of treament effect and an indicator for covariates (above median if nonbinary);  \n Treatment effect is the difference between the ",
                    ifelse(factor == "treatment_r", "respondent level ", "headline level "), 
                    value1, 
                    " treatment, and the baseline ", value0, " treatment.")
  table <- table %>%
    kable(format="html", digits=2, caption=caption, escape = FALSE, 
          row.names = FALSE) %>%
    kable_styling(bootstrap_options=c("condensed", "responsive"), 
                  full_width=FALSE)
  
  return(table)
}



hte_tab_means <- function(factor, rowvars = covariate_names){
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  
  values <- sort(unique(df_treat[, factor]))
  
  if(factor == 'treatment_r'){
    df_train <- bind_rows('control' = df_treat[h1,],
                          'accuracy' = df_treat[h1,],
                          'deliberation' = df_treat[h1,],
                          'emotion' = df_treat[h1,],
                          'pledge' = df_treat[h1,],
                          'tips_africacheck' = df_treat[h1,],
                          'tips_facebook' = df_treat[h1,],
                          'video' = df_treat[h1,], .id = 'level') %>%
      mutate(Scores = case_when(level == 'control' ~ R_control_scores,
                                level == 'accuracy' ~ R_accuracy_scores,
                                level == 'deliberation' ~ R_deliberation_scores,
                                level == 'emotion' ~ R_emotion_scores,
                                level == 'pledge' ~ R_pledge_scores,
                                level == 'tips_africacheck' ~ R_tips_africacheck_scores,
                                level == 'tips_facebook' ~ R_tips_facebook_scores,
                                level == 'video' ~ R_video_scores
      ))
    labels <- c('Accuracy nudge', 'Control', 'Deliberation nudge', 'Emotion suppression', 'Pledge', 'AfricaCheck Tips', 'Facebook Tips', 'Video training')
    
  } else {
    df_train <- bind_rows('control' = df_treat[h1,],
                          'factcheck' = df_treat[h1,],
                          'more_info' = df_treat[h1,],
                          'real_info' = df_treat[h1,],
                          'related' = df_treat[h1,], .id = 'level') %>%
      mutate(Scores = case_when(level == 'control' ~ H_control_scores,
                                level == 'factcheck' ~ H_factcheck_scores,
                                level == 'more_info' ~ H_more_info_scores,
                                level == 'real_info' ~ H_real_info_scores,
                                level == 'related' ~ H_related_scores
      ))
    
    labels <- c('Control', 'Factcheck', 'More information', 'Real information', 'Related articles')
  }
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    
    if(length(unique(df_train[, covariate_name]))<=2 ){
      df_train <- df_train %>%
        mutate(groups = factor(case_when(get(covariate_name)==0~1,
                                         get(covariate_name)==1~2)))
    } else{
      df_train <- df_train %>%
        mutate(groups = factor(case_when(get(covariate_name)<= median(get(covariate_name))~1,
                                         get(covariate_name)> median(get(covariate_name))~2)))
    }
    
    if(length(unique(df_train$groups))==1){
      lm1 <-tidy(lm_robust(Scores ~ -1 + level, data = df_train))
      lm1 <- cbind(`Covariate value` = 'Combined', lm1)
    } else {
      lm1 <-lm_robust(Scores ~ -1 + level, data = df_train[which(df_train$groups == sort(unique(df_train$groups))[1]),])
      lm2 <-lm_robust(Scores ~ -1 + level, data = df_train[which(df_train$groups == sort(unique(df_train$groups))[2]),])
      lm1 <- bind_rows(tidy(lm1),
                       tidy(lm2), .id = 'Covariate value')
      
      if (length(unique(df_train[, covariate_name]))<=2 ){
        coef_newnames <- c(paste0('Not ', covariate_name),
                           stringr::str_to_sentence(covariate_name))
      }else {
        coef_newnames <- c(
          paste0('Below median ', covariate_name),
          paste0('Above median ', covariate_name))
      }
      
      lm1 <- lm1 %>% 
        mutate(`Covariate value` = case_when(`Covariate value` ==1 ~ coef_newnames[1],
                                             `Covariate value` == 2~ coef_newnames[2]))
      
      if(factor == 'treatment_r'){
        lm1$term <- rep(labels, length(unique(lm1$`Covariate value`)))
      } else{
        lm1$term <- rep(labels, length(unique(lm1$`Covariate value`)))
      }
      lm1$term <- relevel(as.factor(lm1$term), 'Control')
      
    }
    
    
    ggplot(lm1, aes(x = term, y = estimate, color = `Covariate value`)) +
      geom_point(position=position_dodge(width=0.5)) +
      geom_errorbar(aes(x=term,ymin = estimate - 1.96 * std.error,ymax = estimate + 1.96 * std.error),width = .1,position=position_dodge(width=0.5)) +
      xlab('Treatment') +
      ylab('Estimate') +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust =1)) +
      # coord_cartesian(ylim=c(0.25, 0.75)) +
      scale_color_manual(values = c('steelblue2', 'firebrick')) +
      ggtitle(paste0(ifelse(factor == 'treatment_r', 'Respondent-level Scores by ', 'Headline-level Scores by '), stringr::str_to_sentence(covariate_name)))+
      theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    ggsave(paste0('../figures/', paste(c(factor, covariate_name), collapse = '_'), '.png'),
           width = 8, height = 6)
    
    
    m <- round(signif(rbind(lm1$estimate,
                            lm1$std.error), digits=4), 3)
    m[1,] <- as.character(m[1,])
    m[2,] <- paste0("(", m[2,], ")")
    if(ncol(m)> length(unique(lm1$term))){
      m <- rbind(m[, 1: length(unique(lm1$term))],
                 m[, (length(unique(lm1$term))+1):ncol(m)])
      rownames(m) <- c(rbind(coef_newnames, ''))
      m[1, which.max(as.numeric(m[1,]))] <- cell_spec(m[1, which.max(as.numeric(m[1,]))], 
                                                      color = "White", 
                                                      background = spec_color(0.5,
                                                                              scale_from = c(0,1)))
      m[3, which.max(as.numeric(m[3,]))] <- cell_spec(m[3, which.max(as.numeric(m[3,]))], 
                                                      color = "White", 
                                                      background = spec_color(0.5,
                                                                              scale_from = c(0,1)))
    } else {
      rownames(m) <- c(stringr::str_to_sentence(covariate_name), '')
      m[1, which.max(as.numeric(m[1,]))] <- cell_spec(m[1, which.max(as.numeric(m[1,]))], 
                                                      color = "White", 
                                                      background = spec_color(0.5,
                                                                              scale_from = c(0,1)))
    }
    
    m
  })
  
  table <- do.call(rbind, table)
  
  colnames(table) <- labels
  # Covariate names
  table <- cbind(covariates=cell_spec(rownames(table), format = "html",
                                      escape = FALSE, color = "black", bold= TRUE), table)
  # table <- table[1:(n_top*2),]
  # Title of table
  caption <- paste0('Mean ',
                    ifelse(factor == 'treatment_r', 'respondent-level ', 'headline-level '),
                    'scores by covariates')
  table <- table %>%
    kable(format="html", digits=2, caption=caption, escape = FALSE,
          row.names = FALSE) %>%
    kable_styling(bootstrap_options=c("condensed", "responsive"),
                  full_width=FALSE)
  
  return(table)
}


hte_tab_diff_scores_alt <- function(value1, value0,
                                    n_top = 15, rowvars = covariate_names){
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  # environment(evaluate_partial_dependency) <- environment()
  covariate_labels <- relabel_covariates(covariate_names)
  
  idx0 <- (get(value0)!=get(value1)) # exclude where overlap
  
  df_train <- bind_rows(value0 = df_eval[idx0,],
                        value1 = df_eval[idx0,], .id = 'level') %>%
    mutate(Scores = c(get(value0)[idx0], get(value1)[idx0]))
  
  df_train2 <- df_eval[idx0,] %>%
    mutate(diff_scores = get(value0)[idx0] - get(value1)[idx0])
  
  if(value0=='control_scores'){
    df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- get(value1)[idx0] + get(value0)[idx0]
    df_train2$diff_scores <- get(value1)[idx0]
  } else if(value1=='control_scores'){
    df_train$Scores[1:(nrow(df_train)/2)] <- get(value1)[idx0] + get(value0)[idx0]
    df_train2$diff_scores <- get(value0)[idx0]
  } else {
    df_train$Scores[1:(nrow(df_train)/2)] <- aipw_scores[idx0, 1] + get(value0)[idx0]
    df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- aipw_scores[idx0,1] + get(value1)[idx0]
  }
  
  
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    covariate_label <- covariate_labels[j]
    
    
    if(length(unique(df_train2[, covariate_name]))<=2 ){
    } else{
      
    }
    if (length(unique(df_train[, covariate_name]))<=2 ){
      df_train2 <- df_train2 %>%
        mutate(groups = factor(case_when(get(covariate_name)==0~1,
                                         get(covariate_name)==1~2)))
      
      coef_newnames <- c(
        paste0('Not ', 
               covariate_label, '\n(n=', table(df_train2$groups)[1], ')'),
        paste0(stringr::str_to_sentence(covariate_label), '\n(n=', table(df_train2$groups)[2], ')'))
    }else{
      df_train2 <- df_train2 %>%
        mutate(groups = factor(case_when(get(covariate_name)<= median(df_eval[,covariate_name])~1,
                                         get(covariate_name)> median(df_eval[,covariate_name])~2)))
      
      coef_newnames <- c(
        paste0('Below median ',
               covariate_label, '\n(n=', table(df_train2$groups)[1], ')'),
        paste0('Above median ',
               covariate_label, '\n(n=', table(df_train2$groups)[2], ')'))
    }
    
    
    if(length(unique(df_train2$groups))==1){
      lm1 <-lm_robust(diff_scores ~ 1, data = df_train2)
    } else {
      lm1 <-lm_robust(diff_scores ~ groups, data = df_train2)
    }
    
    plot_dat <- tidy(lm1)
    
    
    plot_dat$Scores <- coef_newnames[1:length(plot_dat$term)]
    
    ggplot(plot_dat, aes(x = estimate, y = Scores)) +
      stat_gradientinterval(aes(y = Scores, 
                                xdist = distributional::dist_normal(estimate, std.error)),
                            .width = 0, size = 0, color = cbPalette[3], fill = cbPalette[3]) + 
      geom_point(aes(x = estimate), size = 2) +
      geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.05) +
      geom_vline(xintercept = 0, colour = 'grey60', linetype = 2) +
      ylab('Condition') + 
      xlab('Estimate') + 
      labs(title = paste0(paste(stringr::str_to_title(
        gsub('_', ' ', gsub('r_|h_', '', sub('_any.*', '', value1)))), 'vs.', stringr::str_to_title(gsub('_', ' ', gsub('r_|h_', '', sub('_any.*', '', value0))))), ', ', stringr::str_to_title(type)),
        subtitle = paste('by', covariate_label)) +
      {if('r_optimal_scores' %in% c(value0, value1)){
        labs(caption = paste0('Note: Among respondents for whom the optimal policy is not ', 
                              stringr::str_to_sentence(gsub('_|scores', '', 
                                                            gsub('r_|h_', '', 
                                                                 c(value0, value1)[!(c(value0, value1) == 'r_optimal_scores')],3))), '.'))
      } 
      }+
      theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            plot.caption.position =  "plot",
            plot.caption = element_text(hjust = 0)) + 
      coord_cartesian( xlim = c(min(plot_dat$estimate - 2.5 * plot_dat$std.error),
                                max(plot_dat$estimate + 2.5 * plot_dat$std.error)))
    
    suppressMessages(ggsave(paste0('../figures/', paste(c(value0, value1, covariate_name), collapse = '_'), '.png'),
                            width = 8, height = 6))
    
    
    m <- round(signif(rbind('Tau Hat' = lm1$coefficients,
                            'Std. Error' = lm1$std.error), digits=4), 3)
    m["Tau Hat",] <- as.character(m["Tau Hat",])
    m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
    
    if(dim(m)[2]==2){
      value_diff <- (prod(sign(lm1$coefficients))==-1)*(abs(lm1$coefficients[2])>abs(lm1$coefficients[1]))
      m <- cbind(m, matrix(c(sprintf("%.4f", round(lm1$p.value[2],4)), '', value_diff, '') , ncol = 2))
      m["Tau Hat",3] <- cell_spec(m["Tau Hat",3],
                                  color = "White",
                                  background = spec_color(ifelse(value_diff, lm1$p.value[2], 1),
                                                          scale_from = c(0,1)))
    } else {
      m <- cbind(m, matrix('', nrow = 2, ncol = 3))
    }
    m
  })
  
  order_list <- lapply(table, function(x) {
    if(dim(x)[2]>=3){
      x1 <- x['Tau Hat',3]
      x2 <- suppressWarnings(as.numeric(x['Tau Hat',4]))
      x1 <- suppressWarnings(as.numeric(gsub('</span>', '', regmatches(x1, gregexpr('[0-9]*\\.*[0-9]*</span>$', x1)))))
      ifelse(x2, -1 -x1*-1, x1)
    } else {
      999
    }
  } )
  ordering <- order(unlist(order_list))
  table <- table[ordering]
  
  table <- do.call(rbind, table)
  table <- table[, 1:3]
  
  colnames(table) <- c('Baseline/below-median ATE', 'Difference for indicator/above-median ATE',
                       'Difference in treatment effects p.value')
  # Covariate names
  covnames <- rep("", nrow(table))
  covnames[seq(1, length(covnames), 2)] <-
    cell_spec(covariate_names[ordering], format = "html",
              escape = FALSE, color = "black", bold= TRUE)
  table <- cbind(covariates=covnames, table)
  table <- table[1:(n_top*2),]
  # Title of table
  caption <- paste0("Interaction of treament effect and an indicator for covariates (above median if nonbinary);  \n Treatment effect is the difference between the ",
                    value1,
                    " treatment, and the baseline ", value0, " treatment.")
  table <- table %>%
    kable(format="html", digits=2, caption=caption, escape = FALSE,
          row.names = FALSE) %>%
    kable_styling(bootstrap_options=c("condensed", "responsive"),
                  full_width=FALSE)
  
  return(table)
}

hte_tab_diff_scores <- function(value1, value0,
                                n_top = 15, rowvars = covariate_names){
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  # environment(evaluate_partial_dependency) <- environment()
  covariate_labels <- relabel_covariates(covariate_names)
  
  idx0 <- (get(value0)!=get(value1)) # exclude where overlap
  
  df_train <- bind_rows(value0 = df_eval[idx0,],
                        value1 = df_eval[idx0,], .id = 'level') %>%
    mutate(Scores = c(get(value0)[idx0], get(value1)[idx0]))
  
  df_train2 <- df_eval[idx0,] %>%
    mutate(diff_scores = get(value0)[idx0] - get(value1)[idx0])
  
  if(value0=='control_scores'){
    df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- get(value1)[idx0] + get(value0)[idx0]
    df_train2$diff_scores <- get(value1)[idx0]
  } else if(value1=='control_scores'){
    df_train$Scores[1:(nrow(df_train)/2)] <- get(value1)[idx0] + get(value0)[idx0]
    df_train2$diff_scores <- get(value0)[idx0]
  } else {
    df_train$Scores[1:(nrow(df_train)/2)] <- aipw_scores[idx0, 1] + get(value0)[idx0]
    df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- aipw_scores[idx0,1] + get(value1)[idx0]
  }
  
  
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    covariate_label <- covariate_labels[j]
    
    if (length(unique(df_train[, covariate_name]))<=2 ){
      if(is.factor(df_train[, covariate_name])){
        cov_levels <- levels(df_train[, covariate_name])
        df_train <- df_train %>%
          mutate(groups = factor(case_when((level == 'value0') & (get(covariate_name)==cov_levels[1])~1,
                                           (level == 'value1') & (get(covariate_name)==cov_levels[1])~2,
                                           (level == 'value0') & (get(covariate_name)==cov_levels[2])~3,
                                           (level == 'value1') & (get(covariate_name)==cov_levels[2])~4),
                                 levels = 1:4))
        
        coef_newnames <- c(
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', ', 
                 cov_levels[1], '\n(n=', table(df_train$groups)[1], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', ', 
                 cov_levels[1], '\n(n=', table(df_train$groups)[2], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', ', 
                 cov_levels[2], '\n(n=', table(df_train$groups)[3], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', ', 
                 cov_levels[2], '\n(n=', table(df_train$groups)[4], ')'))
        
      }else{
        
        df_train <- df_train %>%
          mutate(groups = factor(case_when((level == 'value0') & (get(covariate_name)==0)~1,
                                           (level == 'value1') & (get(covariate_name)==0)~2,
                                           (level == 'value0') & (get(covariate_name)==1)~3,
                                           (level == 'value1') & (get(covariate_name)==1)~4),
                                 levels = 1:4))
        
        coef_newnames <- c(
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', not ', 
                 covariate_label, '\n(n=', table(df_train$groups)[1], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', not ', 
                 covariate_label, '\n(n=', table(df_train$groups)[2], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', ', 
                 covariate_label, '\n(n=', table(df_train$groups)[3], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', ', 
                 covariate_label, '\n(n=', table(df_train$groups)[4], ')'))
      }
    }else{
      df_train <- df_train %>%
        mutate(groups = factor(case_when((level == 'value0') &
                                           (get(covariate_name)<= median(df_eval[,covariate_name]))~1,
                                         (level == 'value1') &
                                           (get(covariate_name)<= median(df_eval[,covariate_name]))~2,
                                         (level == 'value0') &
                                           (get(covariate_name)> median(df_eval[,covariate_name]))~3,
                                         (level == 'value1') &
                                           (get(covariate_name)> median(df_eval[,covariate_name]))~4),
                               levels = 1:4))
      
      coef_newnames <- c(
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
               ',\nbelow median ', 
               covariate_label, '\n(n=', table(df_train$groups)[1], ')'),
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
               ',\nbelow median ', 
               covariate_label, '\n(n=', table(df_train$groups)[2], ')'),
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
               ',\nabove median ', 
               covariate_label, '\n(n=', table(df_train$groups)[3], ')'),
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
               ',\nabove median ', 
               covariate_label, '\n(n=', table(df_train$groups)[4], ')'))
    }
    
    lm0 <- lm_robust(Scores ~-1 + groups, data = df_train)
    plot_dat <- tidy(lm0)
    
    
    plot_dat$Scores <- sub(" scores.*", "", coef_newnames[1:length(plot_dat$term)])
    plot_dat$Group <- stringr::str_to_sentence(gsub(".* scores,\\n|.* scores, ", "", 
                                                    coef_newnames[1:length(plot_dat$term)]))
    plot_dat$term <- coef_newnames[1:length(plot_dat$term)]
    
    ggplot(plot_dat, aes(x = estimate, y = Scores)) +
      stat_gradientinterval(aes(y = Scores, 
                                xdist = distributional::dist_normal(estimate, std.error)),
                            .width = 0, size = 0, color = cbPalette[3], fill = cbPalette[3]) + 
      facet_wrap(~Group, ncol = 1) + 
      geom_point(aes(x = estimate), size = 2) +
      geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.05) +
      ylab('Condition') + 
      xlab('Estimate') + 
      labs(title = paste(stringr::str_to_title(gsub('_', ' ', gsub('r_|h_', '', value1))), 'vs.', stringr::str_to_title(gsub('_', ' ', gsub('r_|h_', '', value0)))),
           subtitle = paste('by', covariate_label)) +
      {if('r_optimal_scores' %in% c(value0, value1)){
        labs(caption = paste0('Note: Among respondents for whom the optimal policy is not ', 
                              stringr::str_to_sentence(gsub('_|scores', '', 
                                                            gsub('r_|h_', '', 
                                                                 c(value0, value1)[!(c(value0, value1) == 'r_optimal_scores')],3))), '.'))
      } 
      }+
      theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            plot.caption.position =  "plot",
            plot.caption = element_text(hjust = 0)) + 
      coord_cartesian( xlim = c(min(plot_dat$estimate - 2.5 * plot_dat$std.error),
                                max(plot_dat$estimate + 2.5 * plot_dat$std.error)))
    
    suppressMessages(ggsave(paste0('../figures/', paste(c(value0, value1, covariate_name), collapse = '_'), '.png'),
                            width = 8, height = 6))
    
    
    if(length(unique(df_train2[, covariate_name]))<=2 ){
      if(is.factor(df_train2[, covariate_name])){
        
        df_train2 <- df_train2 %>%
          mutate(groups = factor(case_when(get(covariate_name)==cov_levels[1]~1,
                                           get(covariate_name)==cov_levels[2]~2)))
        
      }else
      {
        
        df_train2 <- df_train2 %>%
          mutate(groups = factor(case_when(get(covariate_name)==0~1,
                                           get(covariate_name)==1~2)))
      }
    } else{
      df_train2 <- df_train2 %>%
        mutate(groups = factor(case_when(get(covariate_name)<= median(df_eval[,covariate_name])~1,
                                         get(covariate_name)> median(df_eval[,covariate_name])~2)))
    }
    
    if(length(unique(df_train2$groups))==1){
      lm1 <-lm_robust(diff_scores ~ 1, data = df_train2)
    } else {
      lm1 <-lm_robust(diff_scores ~ groups, data = df_train2)
    }
    
    
    m <- round(signif(rbind('Tau Hat' = lm1$coefficients,
                            'Std. Error' = lm1$std.error), digits=4), 3)
    m["Tau Hat",] <- as.character(m["Tau Hat",])
    m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
    
    if(dim(m)[2]==2){
      value_diff <- (prod(sign(lm1$coefficients))==-1)*(abs(lm1$coefficients[2])>abs(lm1$coefficients[1]))
      m <- cbind(m, matrix(c(sprintf("%.4f", round(lm1$p.value[2],4)), '', value_diff, '') , ncol = 2))
      m["Tau Hat",3] <- cell_spec(m["Tau Hat",3],
                                  color = "White",
                                  background = spec_color(ifelse(value_diff, lm1$p.value[2], 1),
                                                          scale_from = c(0,1)))
    } else {
      m <- cbind(m, matrix('', nrow = 2, ncol = 3))
    }
    m
  })
  
  order_list <- lapply(table, function(x) {
    if(dim(x)[2]>=3){
      x1 <- x['Tau Hat',3]
      x2 <- suppressWarnings(as.numeric(x['Tau Hat',4]))
      x1 <- suppressWarnings(as.numeric(gsub('</span>', '', regmatches(x1, gregexpr('[0-9]*\\.*[0-9]*</span>$', x1)))))
      ifelse(x2, -1 -x1*-1, x1)
    } else {
      999
    }
  } )
  ordering <- order(unlist(order_list))
  table <- table[ordering]
  
  table <- do.call(rbind, table)
  table <- table[, 1:3]
  
  colnames(table) <- c('Baseline/below-median ATE', 'Difference for indicator/above-median ATE',
                       'Difference in treatment effects p.value')
  # Covariate names
  covnames <- rep("", nrow(table))
  covnames[seq(1, length(covnames), 2)] <-
    cell_spec(covariate_names[ordering], format = "html",
              escape = FALSE, color = "black", bold= TRUE)
  table <- cbind(covariates=covnames, table)
  table <- table[1:(n_top*2),]
  # Title of table
  caption <- paste0("Interaction of treament effect and an indicator for covariates (above median if nonbinary);  \n Treatment effect is the difference between the ",
                    value1,
                    " treatment, and the baseline ", value0, " treatment.")
  table <- table %>%
    kable(format="html", digits=2, caption=caption, escape = FALSE,
          row.names = FALSE) %>%
    kable_styling(bootstrap_options=c("condensed", "responsive"),
                  full_width=FALSE)
  
  return(table)
}



hte_tab_diff_scores_any <- function(value1, value0, type,
                                    n_top = 15, rowvars = covariate_names){
  
  covariate_names <- rowvars[!grepl('flag', rowvars)]
  # environment(evaluate_partial_dependency) <- environment()
  covariate_labels <- relabel_covariates(covariate_names)
  
  if(type == 'true'){
    value1 <- paste0(value1, '_any_true')
    value0 <- paste0(value0, '_any_true')
  } else {
    value1 <- paste0(value1, '_any_false')
    value0 <- paste0(value0, '_any_false')
  }
  
  idx0 <- (get(value0)!=get(value1)) # exclude where overlap
  
  df_train <- bind_rows(value0 = df_eval[idx0,],
                        value1 = df_eval[idx0,], .id = 'level') %>%
    mutate(Scores = c(get(value0)[idx0], get(value1)[idx0]))
  
  df_train2 <- df_eval[idx0,] %>%
    mutate(diff_scores = get(value0)[idx0] - get(value1)[idx0])
  
  if(substr(value0, 0,14)=='control_scores'){
    df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- get(value1)[idx0] + get(value0)[idx0]
    df_train2$diff_scores <- get(value1)[idx0]
  } else if(substr(value1, 0,14)=='control_scores'){
    df_train$Scores[1:(nrow(df_train)/2)] <- get(value1)[idx0] + get(value0)[idx0]
    df_train2$diff_scores <- get(value0)[idx0]
  } else {
    if(type == 'true'){
      df_train$Scores[1:(nrow(df_train)/2)] <- aipw_scores_any_true_eval[idx0, 1] + get(value0)[idx0]
      df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- aipw_scores_any_true_eval[idx0,1] + get(value1)[idx0] 
    } else {
      df_train$Scores[1:(nrow(df_train)/2)] <- aipw_scores_any_false_eval[idx0, 1] + get(value0)[idx0]
      df_train$Scores[(nrow(df_train)/2 + 1):nrow(df_train)] <- aipw_scores_any_false_eval[idx0,1] + get(value1)[idx0]
    }
  }
  
  
  
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    covariate_label <- covariate_labels[j]
    
    if (length(unique(df_train[, covariate_name]))<=2 ){
      
      if(is.factor(df_train[, covariate_name])){
        cov_levels <- levels(df_train[, covariate_name])
        df_train <- df_train %>%
          mutate(groups = factor(case_when((level == 'value0') & (get(covariate_name)==cov_levels[1])~1,
                                           (level == 'value1') & (get(covariate_name)==cov_levels[1])~2,
                                           (level == 'value0') & (get(covariate_name)==cov_levels[2])~3,
                                           (level == 'value1') & (get(covariate_name)==cov_levels[2])~4),
                                 levels = 1:4))
        
        coef_newnames <- c(
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', ', 
                 cov_levels[1], '\n(n=', table(df_train$groups)[1], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', ', 
                 cov_levels[1], '\n(n=', table(df_train$groups)[2], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', ', 
                 cov_levels[2], '\n(n=', table(df_train$groups)[3], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', ', 
                 cov_levels[2], '\n(n=', table(df_train$groups)[4], ')'))
        
      }else{
        df_train <- df_train %>%
          mutate(groups = factor(case_when((level == 'value0') & (get(covariate_name)==0)~1,
                                           (level == 'value1') & (get(covariate_name)==0)~2,
                                           (level == 'value0') & (get(covariate_name)==1)~3,
                                           (level == 'value1') & (get(covariate_name)==1)~4),
                                 levels = 1:4))
        
        coef_newnames <- c(
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', not ', 
                 covariate_label, '\n(n=', table(df_train$groups)[1], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', not ', 
                 covariate_label, '\n(n=', table(df_train$groups)[2], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
                 ', ', 
                 covariate_label, '\n(n=', table(df_train$groups)[3], ')'),
          paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
                 ', ', 
                 covariate_label, '\n(n=', table(df_train$groups)[4], ')'))
      }
      
    }else{
      df_train <- df_train %>%
        mutate(groups = factor(case_when((level == 'value0') &
                                           (get(covariate_name)<= median(df_eval[,covariate_name]))~1,
                                         (level == 'value1') &
                                           (get(covariate_name)<= median(df_eval[,covariate_name]))~2,
                                         (level == 'value0') &
                                           (get(covariate_name)> median(df_eval[,covariate_name]))~3,
                                         (level == 'value1') &
                                           (get(covariate_name)> median(df_eval[,covariate_name]))~4),
                               levels = 1:4))
      
      coef_newnames <- c(
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
               ',\nbelow median ', 
               covariate_label, '\n(n=', table(df_train$groups)[1], ')'),
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
               ',\nbelow median ', 
               covariate_label, '\n(n=', table(df_train$groups)[2], ')'),
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value0))), 
               ',\nabove median ', 
               covariate_label, '\n(n=', table(df_train$groups)[3], ')'),
        paste0(stringr::str_to_sentence(gsub('_', ' ', gsub('r_|h_', '', value1))), 
               ',\nabove median ', 
               covariate_label, '\n(n=', table(df_train$groups)[4], ')'))
    }
    
    lm0 <- lm_robust(Scores ~-1 + groups, data = df_train)
    plot_dat <- tidy(lm0)
    
    plot_dat$Scores <- sub(" scores.*", "", coef_newnames[1:length(plot_dat$term)])
    plot_dat$Group <- paste0('Respondents for whom\n',
      gsub(".* scores[a-z]*[a-z]*,\\n|.* scores [a-z]* [a-z]*, ", "", 
                                                    coef_newnames[1:length(plot_dat$term)]))
    plot_dat$term <- coef_newnames[1:length(plot_dat$term)]

    # PLOT    
    ggplot(plot_dat, aes(x = estimate, moe = std.error, y = Scores)) +
      stat_gradientinterval(aes(y = Scores, 
                                xdist = distributional::dist_normal(estimate, std.error)),
                            .width = 0, size = 0, color = cbPalette[3], fill = cbPalette[3]) +
      facet_wrap(~Group, ncol = 1) + 
      geom_point(aes(x = estimate), size = 2) +
      geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.05) +
      ylab('Conditions') + 
      xlab(paste0('Estimated mean response,\nAny sharing of ', type, ' stimuli')) + 
      labs(title = 'Estimated sharing under counterfactual conditions',
           subtitle = paste('by', tolower(covariate_label))) +
      {if( any(grepl('r_optimal_scores', c(value0, value1))) ){
        labs(caption = paste0('Note: Among respondents for whom the optimal policy is not ', 
                              gsub('_|scores|any|true|false', '', 
                                   gsub('r_|h_', '', 
                                        c(value0, value1)[!grepl('r_optimal_scores', c(value0, value1))],3)), '.'))
      } 
      }+
      theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            plot.caption.position =  "plot",
            plot.caption = element_text(hjust = 0)) + 
      coord_cartesian(xlim = c(min(plot_dat$estimate - 2.5 * plot_dat$std.error),
                               max(plot_dat$estimate + 2.5 * plot_dat$std.error)))
    
    suppressMessages(ggsave(paste0('../figures/', paste(c(value0, value1, covariate_name), collapse = '_'), '.png'),
                            width = 8, height = 6))
    
    
    if(length(unique(df_train2[, covariate_name]))<=2 ){
      if(is.factor(df_train2[, covariate_name])){
        
        df_train2 <- df_train2 %>%
          mutate(groups = factor(case_when(get(covariate_name)==cov_levels[1]~1,
                                           get(covariate_name)==cov_levels[2]~2)))
        
      }else
      {
        df_train2 <- df_train2 %>%
          mutate(groups = factor(case_when(get(covariate_name)==0~1,
                                           get(covariate_name)==1~2)))
        
      }
      
    } else{
      df_train2 <- df_train2 %>%
        mutate(groups = factor(case_when(get(covariate_name)<= median(df_eval[,covariate_name])~1,
                                         get(covariate_name)> median(df_eval[,covariate_name])~2)))
    }
    
    if(length(unique(df_train2$groups))==1){
      lm1 <-lm_robust(diff_scores ~ 1, data = df_train2)
    } else {
      lm1 <-lm_robust(diff_scores ~ groups, data = df_train2)
    }
    
    
    m <- round(signif(rbind('Tau Hat' = lm1$coefficients,
                            'Std. Error' = lm1$std.error), digits=4), 3)
    m["Tau Hat",] <- as.character(m["Tau Hat",])
    m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
    
    if(dim(m)[2]==2){
      value_diff <- (prod(sign(lm1$coefficients))==-1)*(abs(lm1$coefficients[2])>abs(lm1$coefficients[1]))
      m <- cbind(m, matrix(c(sprintf("%.4f", round(lm1$p.value[2],4)), '', value_diff, '') , ncol = 2))
      m["Tau Hat",3] <- cell_spec(m["Tau Hat",3],
                                  color = "White",
                                  background = spec_color(ifelse(value_diff, lm1$p.value[2], 1),
                                                          scale_from = c(0,1)))
    } else {
      m <- cbind(m, matrix('', nrow = 2, ncol = 3))
    }
    m
  })
  
  order_list <- lapply(table, function(x) {
    if(dim(x)[2]>=3){
      x1 <- x['Tau Hat',3]
      x2 <- suppressWarnings(as.numeric(x['Tau Hat',4]))
      x1 <- suppressWarnings(as.numeric(gsub('</span>', '', regmatches(x1, gregexpr('[0-9]*\\.*[0-9]*</span>$', x1)))))
      ifelse(x2, -1 -x1*-1, x1)
    } else {
      999
    }
  } )
  ordering <- order(unlist(order_list))
  table <- table[ordering]
  
  table <- do.call(rbind, table)
  table <- table[, 1:3]
  
  colnames(table) <- c('Baseline/below-median ATE', 'Difference for indicator/above-median ATE',
                       'Difference in treatment effects p.value')
  # Covariate names
  covnames <- rep("", nrow(table))
  covnames[seq(1, length(covnames), 2)] <-
    cell_spec(covariate_names[ordering], format = "html",
              escape = FALSE, color = "black", bold= TRUE)
  table <- cbind(covariates=covnames, table)
  table <- table[1:(n_top*2),]
  # Title of table
  caption <- paste0("Interaction of treament effect and an indicator for covariates (above median if nonbinary);  \n Treatment effect is the difference between the ",
                    value1,
                    " treatment, and the baseline ", value0, " treatment.")
  table <- table %>%
    kable(format="html", digits=2, caption=caption, escape = FALSE,
          row.names = FALSE) %>%
    kable_styling(bootstrap_options=c("condensed", "responsive"),
                  full_width=FALSE)
  
  return(table)
}

relabel_covariates <- function(covariate_names){
  cov_mat <- matrix(
    c('ed', 'education',
      'rel_christian', 'Christian', 
      'rel_muslim', 'Muslim',
      'denom_pentecostal', 'pentecostal', 
      'locus', 'locus of control',
      'science', 'scientific belief index', 
      'dli', 'digital literacy index',
      'fb_post', 'Facebook posting',
      'fb_msg', 'Facebook messaging',
      'crt', 'Cognitive reflection test',
      'hhi', 'index of household posessions',
      'hh', 'household size',
      'cash', 'job with cash income',
      'pol', 'political affiliation with governing party',
      'cov_concern', 'concern regarding COVID-19',
      'cov_efficacy', 'perceived government efficacy on COVID-19',
      'Y_pre', 'pre-treatment media discernement',
      'nigeria', 'Nigerian respondent',
      'optimal_assignment', 'Optimal assignment'
    ), byrow = TRUE, ncol = 2)
  
  
  idx <- match(cov_mat[,1], covariate_names, nomatch = 0)
  idx2 <- match(covariate_names, cov_mat[,1], nomatch = 0)
  covariate_names[idx] <- cov_mat[cbind(sort(idx2),2)]
  
  return(covariate_names)
}


