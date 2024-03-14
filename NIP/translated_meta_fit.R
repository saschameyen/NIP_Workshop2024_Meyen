library(stats)
library(optimx)

fit_meta_d_MLE_Meyen <- function(nR_S1, nR_S2, s = 1, fncdf = pnorm, fninv = qnorm) {
  
  if (length(nR_S1) %% 2 != 0) {
    stop('input arrays must have an even number of elements')
  }
  if (length(nR_S1) != length(nR_S2)) {
    stop('input arrays must have the same number of elements')
  }
  if (any(nR_S1 == 0) || any(nR_S2 == 0)) {
    cat('\n')
    cat('WARNING!!\n')
    cat('---------\n')
    cat('Your inputs\n')
    cat('\n')
    cat('nR_S1:\n')
    print(nR_S1)
    cat('nR_S2:\n')
    print(nR_S2)
    cat('\n')
    cat('contain zeros! This may interfere with proper estimation of meta-d\'.\n')
    cat('See help fit_meta_d_MLE for more information.\n')
    cat('\n')
    cat('\n')
  }
  nRatings <- length(nR_S1) / 2
  nCriteria <- 2 * nRatings - 1
  
  A <- matrix(0, nrow = nCriteria - 2, ncol = nCriteria)
  ub <- rep(-1e-5, nCriteria - 2)
  lb <- rep(-Inf, nCriteria - 2)
  
  for (ii in 1:(nCriteria - 2)) {
    tempArow <- c(rep(0, ii), 1, -1, rep(0, (nCriteria - 2) - ii))
    A[ii, ] <- tempArow
  }
  
  LB <- c(-10, rep(-20, (nCriteria - 1) / 2), rep(0, (nCriteria - 1) / 2))
  UB <- c(10, rep(0, (nCriteria - 1) / 2), rep(20, (nCriteria - 1) / 2))
  
  constant_criterion <- 'meta_d1 * (t1c1 / d1)'
  
  ratingHR <- numeric()
  ratingFAR <- numeric()
  
  for (c in 1:(nRatings * 2 - 1)) {
    ratingHR[c] <- sum(nR_S2[c:length(nR_S2)]) / sum(nR_S2)
    ratingFAR[c] <- sum(nR_S1[c:length(nR_S1)]) / sum(nR_S1)
  }
  
  t1_index <- nRatings
  t2_index <- setdiff(1:(2 * nRatings - 1), t1_index)
  d1 <- (1 / s) * fninv(ratingHR[t1_index]) - fninv(ratingFAR[t1_index])
  meta_d1 <- d1
  c1 <- (-1 / (1 + s)) * (fninv(ratingHR) + fninv(ratingFAR))
  t1c1 <- c1[t1_index]
  t2c1 <- c1[t2_index]
  
  guess <- c(meta_d1, t2c1 - eval(parse(text = constant_criterion)))
  
  inputObj <- list(nR_S1 = nR_S1, nR_S2 = nR_S2, nRatings = nRatings, d1 = d1, t1c1 = t1c1, s = s, constant_criterion = constant_criterion, fncdf = fncdf, fninv = fninv)
  # bounds <- optimx::optimx.bounds(LB, UB)
  linear_constraint <- optimx::optimx.constraints(A, lb, ub)
  
  results <- optimx::optimx(guess, fit_meta_d_logL, inputObj = inputObj, method = 'L-BFGS-B', lower = LB, upper = UB, constraints = linear_constraint, control = list(trace = 1))
  
  meta_d1 <- results$par[1]
  t2c1 <- results$par[-1] + eval(parse(text = constant_criterion))
  logL <- -results$value
  
  I_nR_rS2 <- nR_S1[(nRatings+1):length(nR_S1)]
  I_nR_rS1 <- rev(nR_S2[1:nRatings])
  C_nR_rS2 <- nR_S2[(nRatings+1):length(nR_S2)]
  C_nR_rS1 <- rev(nR_S1[1:nRatings])

  obs_FAR2_rS2 <- sapply(0:(nRatings-2), function(i) sum(I_nR_rS2[(i+2):length(I_nR_rS2)]) / sum(I_nR_rS2))
  obs_HR2_rS2 <- sapply(0:(nRatings-2), function(i) sum(C_nR_rS2[(i+2):length(C_nR_rS2)]) / sum(C_nR_rS2))
  obs_FAR2_rS1 <- sapply(0:(nRatings-2), function(i) sum(I_nR_rS1[(i+2):length(I_nR_rS1)]) / sum(I_nR_rS1))
  obs_HR2_rS1 <- sapply(0:(nRatings-2), function(i) sum(C_nR_rS1[(i+2):length(C_nR_rS1)]) / sum(C_nR_rS1))

  S1mu <- -meta_d1/2
  S1sd <- 1
  S2mu <- meta_d1/2
  S2sd <- S1sd/s
  mt1c1 <- eval(parse(text = constant_criterion))
  C_area_rS2 <- 1-pnorm(mt1c1, mean = S2mu, sd = S2sd)
  I_area_rS2 <- 1-pnorm(mt1c1, mean = S1mu, sd = S1sd)
  C_area_rS1 <- pnorm(mt1c1, mean = S1mu, sd = S1sd)
  I_area_rS1 <- pnorm(mt1c1, mean = S2mu, sd = S2sd)

  est_FAR2_rS2 <- vector()
  est_HR2_rS2 <- vector()
  est_FAR2_rS1 <- vector()
  est_HR2_rS1 <- vector()

  for (i in 0:(nRatings-2)) {
    t2c1_lower <- t2c1[(nRatings-1)-(i+1)]
    t2c1_upper <- t2c1[(nRatings-1)+i]
    I_FAR_area_rS2 <- 1-pnorm(t2c1_upper, mean = S1mu, sd = S1sd)
    C_HR_area_rS2 <- 1-pnorm(t2c1_upper, mean = S2mu, sd = S2sd)
    I_FAR_area_rS1 <- pnorm(t2c1_lower, mean = S2mu, sd = S2sd)
    C_HR_area_rS1 <- pnorm(t2c1_lower, mean = S1mu, sd = S1sd)
    est_FAR2_rS2 <- c(est_FAR2_rS2, I_FAR_area_rS2 / I_area_rS2)
    est_HR2_rS2 <- c(est_HR2_rS2, C_HR_area_rS2 / C_area_rS2)
    est_FAR2_rS1 <- c(est_FAR2_rS1, I_FAR_area_rS1 / I_area_rS1)
    est_HR2_rS1 <- c(est_HR2_rS1, C_HR_area_rS1 / C_area_rS1)
  }

  fit <- list()
  fit$da <- sqrt(2/(1+s^2)) * s * d1
  fit$s <- s
  fit$meta_da <- sqrt(2/(1+s^2)) * s * meta_d1
  fit$M_diff <- fit$meta_da - fit$da
  fit$M_ratio <- fit$meta_da / fit$da
  mt1c1 <- eval(parse(text = constant_criterion))
  fit$meta_ca <- (sqrt(2)*s / sqrt(1+s^2)) * mt1c1
  t2ca <- (sqrt(2)*s / sqrt(1+s^2)) * t2c1
  fit$t2ca_rS1 <- t2ca[1:(nRatings-1)]
  fit$t2ca_rS2 <- t2ca[nRatings:length(t2ca)]
  fit$S1units <- list()
  fit$S1units$d1 <- d1
  fit$S1units$meta_d1 <- meta_d1
  fit$S1units$s <- s
  fit$S1units$meta_c1 <- mt1c1
  fit$S1units$t2c1_rS1 <- t2c1[1:(nRatings-1)]
  fit$S1units$t2c1_rS2 <- t2c1[nRatings:length(t2c1)]
  fit$logL <- logL
  fit$est_HR2_rS1 <- est_HR2_rS1
  fit$obs_HR2_rS1 <- obs_HR2_rS1
  fit$est_FAR2_rS1 <- est_FAR2_rS1
  fit$obs_FAR2_rS1 <- obs_FAR2_rS1
  fit$est_HR2_rS2 <- est_HR2_rS2
  fit$obs_HR2_rS2 <- obs_HR2_rS2
  fit$est_FAR2_rS2 <- est_FAR2_rS2
  fit$obs_FAR2_rS2 <- obs_FAR2_rS2

  fit


}

