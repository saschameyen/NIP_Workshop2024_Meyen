# 2024-03-04
# This is a collection of meta-measures by Sascha Meyen, saschameyen@gmail.com.


# File: get_entropy.R


#' Get Entropy 
#'
#' @param p Probability vector
#' @param base Base for the logarithm. If base = 2, the entropy is measured in
#'  bit. If base = e, the entropy is measured in nat. Default is base = 2.
#'
#' @return Entropy H(p)
get_entropy <- H <- function(p, base = 2)
{
  stopifnot( round(sum(p) - 1, 6) == 0 )

  p <- p[p != 0] # By convention, 0*log(1/0) = 0 
                # because lim_{p->0} p*log(1/p) = 0

  entropy <- sum( p * log(1/p, base) )
  entropy
}


# File: get_binary_entropy.R


#' Get Binary Entropy
#'
#' @param p Probability of one realiziation. Which of the two realiziations of
#'  a binary variable doesn't matter because of symmetry, H2(p) = H2(1-p).
#'
#' @return Entropy of the binary random variable
get_binary_entropy <- H2 <- function(p) 
{ 
  binary_entropy <- sapply(p, function(p) { get_entropy(c(p, 1-p)) } )
  binary_entropy
}



# File: get_pointwise_binary_transmitted_information.R


#' Binary Transmitted Mutual Information (PTI)
#' 
#' Mutual information of a binary random variable (typically with equal base
#' rates, p = 0.5) and a prediction with confidence x
#'
#' @param c Confidence in the predicted label
#' @param p Base rate
#'
#' @return Transmitted Information between binary random variable and prediction
get_pointwise_binary_transmitted_information <- pti2 <- function(c, p = 0.5) 
{ 
  H2(p) - H2(c) 
}


# File: get_pointwise_transmitted_information.R


#' Pointwise Transmitted Information (PTI)
#' 
#' Transmitted information of a random variable and response with confidence
#' vector c. Transmitted information and mutual information is the same
#' thing, there are just different conventional names.
#'
#' @param c Confidence vector
#' @param p Base rate
#'
#' @return Transmitted Information between random variable and prediction
get_pointwise_transmitted_information <- pti <- function(c, prior)
{
  H(prior) - H(c)
}


# File: get_accuracy.R


#' Compute accuracy of a classifier
#'
#' The accuracy is the sum of highest entries for each response in the
#' classifier contingency matrix. 
#' 
#' @param classifier as contingency matrix
#'
#' @return Accuracy of the classifier
get_accuracy <- function(classifier)
{
  accuracy <- sum( apply(classifier, 2, max) )
  accuracy
}


# File: get_information.R


#' Compute transmitted information of a classifier
#'
#' @param classifier
#'
#' @return Transmitted information of the classifier's output about the label
get_information = function(classifier)
{
  stopifnot( round( sum(classifier) - 1, 6) == 0 )
  info <- H(colSums(classifier)) + H(rowSums(classifier)) - H(classifier)
  info
}


# File: estimate_classifier.R


#' Estimate Classifier
#' 
#' Construct the maximum likelihood contingency table as an estimate for the
#' classifier's joint probability distribution.
#' 
#' @param msd Data frame with true labels "y" and confidence-binned
#'  predictions "response"
#' 
#' @return Classifier as contingency table with true labels as rows and
#'  responses as columns
estimate_classifier <- function(msd)
{
  estimated_classifier <- table(msd$y, msd$response) / nrow(msd)
  estimated_classifier
}


# File: estimate_accuracy.R


#' Estimate Accuracy
#' 
#' Estimate the probability of correct predictions either from a data frame
#' with varibles correct (0/1) and rating (level of confidence, for example
#' values 1/2 for low and high confidence), or from a matrix with counts of
#' observed true labels (rows) and responses (columns).
#' 
#' @param x Data frame or matrix
#' 
#' @return Accuracy
estimate_accuracy <- function(x)
{
  UseMethod("estimate_accuracy")
}

estimate_accuracy.data.frame <- function(msd)
{
  estimated_classifier <- estimate_classifier(msd)

  acc <- get_accuracy(estimated_classifier)
  acc
}

estimate_accuracy.matrix <- function(tab)
{
  estimated_classifier <- tab/sum(tab)

  acc <- get_accuracy(estimated_classifier)
  acc
}

estimate_accuracy.table <- function(tab)
{
  estimated_classifier <- tab/sum(tab)

  acc <- get_accuracy(estimated_classifier)
  acc
}


# File: estimate_information.R


#' Estimate Information
#' 
#' Estimate the information that a classifier's responses transmit about the
#' true label. Here, we use a simple plug-in estimate.
#' 
#' @param x Data frame or matrix
#' 
#' @return Transmitted information
estimate_information <- function(x)
{
  UseMethod("estimate_information")
}

estimate_information.data.frame <- function(msd)
{
  estimated_classifier <- estimate_classifier(msd)

  info <- get_information(estimated_classifier)
  info
}
estimate_information.matrix <- function(tab)
{
  estimated_classifier <- tab/sum(tab)
  
  info <- get_information(estimated_classifier)
  info
}
estimate_information.table <- function(tab)
{
  estimated_classifier <- tab/sum(tab)

  info <- get_information(estimated_classifier)
  info
}


# File: estimate_meta_sensitivity.R


# install.packages("devtools")
# devtools::install_github("craddm/metaSDT")

library(metaSDT)

#' Estimate Meta Sensitivity
#' 
#' Estimate Dayan (2023)'s meta-I either from a data frame with varibles
#' correct (0/1) and rating (level of confidence, for example values 1/2 for
#' low and high confidence), or from a matrix with counts of observed true
#' labels (rows) and responses (columns).
#' 
#' @param x Data frame or matrix
#' 
#' @return Meta-d' value
estimate_meta_sensitivity <- function(x)
{
  UseMethod("estimate_meta_sensitivity")
}

estimate_meta_sensitivity.data.frame <- function(msd)
{
  total_n <- nrow(msd)
  estimated_classifier <- estimate_classifier(msd)
  tab    <- estimated_classifier * total_n
  meta_sensitivity <- estimate_meta_sensitivity(tab)
  meta_sensitivity
}

estimate_meta_sensitivity.matrix <- function(tab)
{
  meta_sensitivity <- fit_meta_d_MLE(tab[1,], tab[2,])$meta_da
  meta_sensitivity
}

estimate_meta_sensitivity.table <- function(tab)
{
  meta_sensitivity <- fit_meta_d_MLE(tab[1,], tab[2,])$meta_da
  meta_sensitivity
}


# File: estimate_sensitivity.R


# install.packages("devtools")
# devtools::install_github("craddm/metaSDT")

library(metaSDT)

#' Estimate Meta Sensitivity
#' 
#' Estimate Dayan (2023)'s meta-I either from a data frame with varibles
#' correct (0/1) and rating (level of confidence, for example values 1/2 for
#' low and high confidence), or from a matrix with counts of observed true
#' labels (rows) and responses (columns).
#' 
#' @param x Data frame or matrix
#' 
#' @return Sensitivity value
estimate_sensitivity <- function(x)
{
  UseMethod("estimate_sensitivity")
}

estimate_sensitivity.data.frame <- function(msd)
{
  tab <- table(msd$y, msd$response)
  sensitivity <- estimate_sensitivity(tab)
  sensitivity
}

estimate_sensitivity.matrix <- function(tab)
{
  sensitivity <- fit_meta_d_MLE(tab[1,], tab[2,])$da
  sensitivity
}

estimate_sensitivity.table <- function(tab)
{
  sensitivity <- fit_meta_d_MLE(tab[1,], tab[2,])$da
  sensitivity
}


# File: estimate_M_ratio.R


# install.packages("devtools")
# devtools::install_github("craddm/metaSDT")

library(metaSDT)

#' Estimate Meta Sensitivity
#' 
#' Estimate Dayan (2023)'s meta-I either from a data frame with varibles
#' correct (0/1) and rating (level of confidence, for example values 1/2 for
#' low and high confidence), or from a matrix with counts of observed true
#' labels (rows) and responses (columns).
#' 
#' @param x Data frame or matrix
#' 
#' @return M-ratio value
estimate_M_ratio <- function(x)
{
  UseMethod("estimate_M_ratio")
}

estimate_M_ratio.data.frame <- function(msd)
{
  total_n <- nrow(msd)
  estimated_classifier <- estimate_classifier(msd)
  tab         <- estimated_classifier * total_n
  sensitivity <- estimate_M_ratio(tab)
  sensitivity
}

estimate_M_ratio.matrix <- function(tab)
{
  sensitivity <- fit_meta_d_MLE(tab[1,], tab[2,])$M_ratio
  sensitivity
}

estimate_M_ratio.table <- function(tab)
{
  sensitivity <- fit_meta_d_MLE(tab[1,], tab[2,])$M_ratio
  sensitivity
}


# File: estimate_meta_I.R


#' Estimate Meta-I
#' 
#' Estimate Dayan (2023)'s meta-I either from a data frame with
#' varibles "correct"(0/1) and confidence-binned "response", for example,
#' with values -2/-1/+1/+2 or from a matrix with counts of observed true
#' labels (rows) and responses (columns). We compute it as the information
#' minus the minimal information, 
#' 
#'   meta-I = I(Y; \hat{Y}, C) - I(Y; \hat{Y}), 
#' 
#' which is equivalent to Dayan's formulation 
#' 
#'   meta-I = I(Y = \hat{Y}; C).
#' 
#' @param x Data frame or matrix
#' 
#' @return Meta-I value
estimate_meta_I <- function(x)
{
  UseMethod("estimate_meta_I")
}

estimate_meta_I.data.frame <- function(msd)
{
  estimated_classifier <- estimate_classifier(msd)
  meta_I <- estimate_meta_I.matrix(estimated_classifier)
  meta_I
}

estimate_meta_I.matrix <- function(estimated_classifier)
{
  estimated_classifier <- estimated_classifier/sum(estimated_classifier)
  p                    <- rowSums(estimated_classifier) # Prior

  info       <- get_information(estimated_classifier)
  a          <- get_accuracy(estimated_classifier)
  info_lower <- get_lower_info_for_one(p, a)
  
  meta_I <- info - info_lower
  meta_I
}

estimate_meta_I.table <- function(tab)
{
  estimated_classifier <- tab/sum(tab)
  meta_I <- estimate_meta_I.matrix(estimated_classifier)
  meta_I
}


# File: estimate_meta_Ir1.R


#' Estimate Meta Ir1
#' 
#' Estimate Dayan (2023)'s meta-I^r_1 either from a data frame with
#' varibles "correct"(0/1) and confidence-binned "response", for example,
#' with values -2/-1/+1/+2 or from a matrix with counts of observed true
#' labels (rows) and responses (columns). 
#' 
#' This is normalizing meta-I by the meta-I that would be produced by normal
#' noise with the same accuracy.
#' 
#' @param x Data frame or matrix
#' 
#' @return Meta-I^r_1 value
estimate_meta_Ir1 <- function(x)
{
  UseMethod("estimate_meta_Ir1")
}

estimate_meta_Ir1.data.frame <- function(msd)
{
  estimated_classifier <- estimate_classifier(msd)
  meta_I <- estimate_meta_Ir1.matrix(estimated_classifier)
  meta_I
}

estimate_meta_Ir1.matrix <- function(estimated_classifier)
{
  estimated_classifier <- estimated_classifier/sum(estimated_classifier)

  meta_I <- estimate_meta_I(estimated_classifier)

  p          <- rowSums(estimated_classifier)
  a          <- get_accuracy(estimated_classifier)
  info       <- get_information(estimated_classifier)
  info_lower <- get_lower_info_for_one(p, a)

  info_normal   <- get_normal_noise_information(accuracies = a)$info
  meta_I_normal <- info_normal - info_lower

  meta_I_r1 <- meta_I / meta_I_normal

  meta_I_r1
}

estimate_meta_Ir1.table <- function(tab)
{
  estimated_classifier <- tab/sum(tab)
  meta_Ir1 <- estimate_meta_Ir1.matrix(estimated_classifier)
  meta_Ir1
}


# File: get_normal_noise_classifier.R


#' Get Normal Noise Classifier
#' 
#' Define a classifier with underlying normal distributions on observations.
#' The normal distribution is of equal variance (= 1) and the means are
#' shifted by the sensitivity.
#' 
#' Either accuracy or sensitivity has to be specified to determine the
#' classifier. If both are specified (not advised), accuracies are
#' prioritizied.
#' 
#' @param sensitivity Shift of the normal distributions
#' @param accuracy Resulting classifier accuracy
#' 
#' @return Normal noise classifier
get_normal_noise_classifier = function(sensitivity = NULL,
                                       accuracy    = NULL)
{
  if (is.null(sensitivity))
    sensitivity <- transform_normal_accuracy_to_sensitivity(accuracy)

  if (is.null(accuracy))
    accuracy <- transform_normal_sensitivity_to_accuracy(sensitivity)

  prior <- c(.5, .5)
  if (sensitivity == 0)         return(as.matrix(prior))
  if (is.infinite(sensitivity)) return(diag(prior))

  x  <- seq(-7 - sensitivity, 7 + sensitivity, length.out = 5000)
  delta <- x[2] - x[1]
  f1 <- dnorm(x, mean = -sensitivity/2) * delta * 1/2
  f2 <- dnorm(x, mean = +sensitivity/2) * delta * 1/2

  classifier <- rbind(f1, f2)
  classifier
}

transform_normal_accuracy_to_sensitivity <- function(accuracy)
{
  sensitivity <- 2 * qnorm(accuracy)
  sensitivity
}

transform_normal_sensitivity_to_accuracy <- function(sensitivity)
{
  accuracy <- pnorm(sensitivity / 2)
  accuracy
}




# File: get_normal_noise_information.R


#' Get Normal Noise Information
#' 
#' Determine the transmitted information of a normal noise classifier. Either
#' accuracies or sensitivities have to be specified to determine the
#' corresponding transmitted information. If both are specified(not advised),
#' accuracies are prioritizied.
#' 
#' @param accuracies Vector of accuracies
#' @param sensitivities Vector of sensitivities
#' 
#' @return Transmitted information of a normal noise classifier
get_normal_noise_information <- function(accuracies    = NULL,
                                         sensitivities = NULL)
{
  if (is.null(sensitivities) & is.null(accuracies))
    stop("Specify accuracies or sensitivities")

  if (is.null(sensitivities))
    sensitivities <- transform_normal_accuracy_to_sensitivity(accuracies)

  if (is.null(accuracies))
    accuracies <- transform_normal_sensitivity_to_accuracy(sensitivities)

  normal_noise_information <- data.frame()
  for (i in seq_along(sensitivities))
  {
    classifier <- get_normal_noise_classifier(sensitivity = sensitivities[i])
    row <- data.frame(accuracy    = accuracies[i],
                      sensitivity = sensitivities[i],
                      info        = get_information(classifier))
    normal_noise_information <- rbind(normal_noise_information, row)
  }

  normal_noise_information
}


# File: estimate_meta_Ir2.R


#' Estimate Meta Ir2
#' 
#' Estimate Dayan (2023)'s meta-I^r_1 either from a data frame with
#' varibles "correct"(0/1) and confidence-binned "response", for example,
#' with values -2/-1/+1/+2 or from a matrix with counts of observed true
#' labels (rows) and responses (columns). 
#' 
#' We compute meta-I as usual and H2(accuracy) as the normalizer,
#' 
#'   meta-Ir1 = meta-I / H(Y = \hat{Y}). 
#' 
#' @param x Data frame or matrix
#' 
#' @return Meta-I^r_2 value
estimate_meta_Ir2 <- function(x)
{
  UseMethod("estimate_meta_Ir2")
}

estimate_meta_Ir2.data.frame <- function(msd)
{
  estimated_classifier <- estimate_classifier(msd)
  meta_I <- estimate_meta_Ir2.matrix(estimated_classifier)
  meta_I
}

estimate_meta_Ir2.matrix <- function(estimated_classifier)
{
  estimated_classifier <- estimated_classifier/sum(estimated_classifier)

  meta_I <- estimate_meta_I(estimated_classifier)

  a          <- get_accuracy(estimated_classifier)
  H_accuracy <- H2(a)
  
  meta_I_r2 <- meta_I / H_accuracy

  meta_I_r2
}

estimate_meta_Ir2.table <- function(tab)
{
  estimated_classifier <- tab/sum(tab)
  meta_Ir2 <- estimate_meta_Ir2.matrix(estimated_classifier)
  meta_Ir2
}


# File: estimate_RMI.R


#' Estimate Relative Meta-Information
#' 
#' Estimate relative meta-information measure either from a data frame with
#' varibles "correct" (0/1) and confidence-binned "response", for example,
#' with values -2/-1/+1/+2, or from a matrix with counts of observed true
#' labels (rows) and responses (columns).
#' 
#' This is a simple plug-in estimate without bias-correction.
#' 
#' @param x Data frame or matrix
#' 
#' @return Relative meta-information value
estimate_RMI <- function(x)
{
  UseMethod("estimate_RMI")
}

estimate_RMI.data.frame <- function(msd)
{
  estimated_classifier <- estimate_classifier(msd)
  estimated_RMI <- estimate_RMI.matrix(estimated_classifier)
  estimated_RMI
}

estimate_RMI.matrix <- function(estimated_classifier)
{
  estimated_classifier <- estimated_classifier/sum(estimated_classifier)
  estimated_RMI <- get_RMI(estimated_classifier)
  estimated_RMI
}

estimate_RMI.table <- function(tab)
{
  estimated_classifier <- tab/sum(tab)
  estimated_RMI <- get_RMI(estimated_classifier)
  estimated_RMI
}



# File: get_RMI.R


#' Get Relative Meta-Information
#' 
#' Compute the meta-information as a relative measure in the possible range
#' given the accuracy: Highest information for a given accuracy produces 
#' RMI = 1, lowest information for a given accuracy produces RMI = 0.
#' 
#' @param classifier Matrix with contingency table of true labels (rows) and
#'  the classifier's responses (columns)
#' 
#' @return Relative meta-information value
get_RMI <- function(classifier)
{
  prior       <- rowSums(classifier)
  accuracy    <- get_accuracy(classifier)
  information <- get_information(classifier)
  
  # Get bounds
  information_bounds <- get_analytic_information_bounds(prior, accuracy)
  lower_bound <- information_bounds$lowest
  upper_bound <- information_bounds$highest

  # Normalize by bounds
  RMI <- ( information - lower_bound ) / 
         ( upper_bound - lower_bound )

  # Where bounds collapse because of accuracy edge cases, return NaN
  RMI[lower_bound == upper_bound] <- NaN

  RMI
}


# File: get_analytic_information_bounds.R


#' Get Analytic Binary Information Bounds
#'
#' Given the accuracies, compute the highest and lowest transmitted
#' information of classifiers.
#'
#' @param accuracy Accuracy of classifiers
#'
#' @return Data frame with highest and lowest possible transmitted information
get_analytic_binary_information_bounds = function(prior, accuracies)
{
  information_bounds <- data.frame(accuracies = accuracies                 ,
                                   highest    = H(prior) - 2*(1-accuracies), 
                                   lowest     = H(prior) - H2(accuracies))
  information_bounds
}

#' Get Analytical Information Bounds
#'
#' Given the accuracies, compute the highest and lowest transmitted
#' information of classifiers. Information is given in bit (log base is 2).
#'
#' @param accuracy Accuracy of classifiers
#'
#' @return Data frame with highest and lowest possible transmitted information
get_analytic_information_bounds = function(prior, accuracies)
{
  upper <- sapply(accuracies, \(a) { get_upper_info_for_one(prior, a) })
  lower <- sapply(accuracies, \(a) { get_lower_info_for_one(prior, a) })

  information_bounds <- data.frame(accuracies = accuracies,
                                   highest    = upper     ,
                                   lowest     = lower     )
  information_bounds
}

get_upper_info_for_one <- function(prior, accuracy)
{
  m1 <- floor(1/accuracy)
  m2 <- floor(1/accuracy)+1

  HY  <- get_entropy(prior)
  HYC <- ( (1/m1 - accuracy) * log(m2, 2) + (accuracy - 1/m2) * log(m1, 2) ) / (1/m1 - 1/m2)

  info <- HY - HYC
  info
}

get_lower_info_for_one <- function(prior, accuracy)
{  
  a <- accuracy
  p <- sort(prior, decreasing = TRUE)
  L <- length(prior)

  if (a == 1) 
  {
    info <- sum(p * log(1/p, 2))
    return(info)
  }

  if (a == max(p))
  {
    info <- 0
    return(info)
  }

  s  <- p >= (cumsum(p) - a)/(1:L - 1)
  m3 <- tail(which(s), 1)
  q <- sum(p[1:m3])
  pl <- p[1:m3]  

  HY  <- sum(pl * log(1/pl, 2))
  HYC <- a * log(1/a, 2) + (q - a) * log((m3 - 1)/(q - a), 2)

  info <- HY - HYC
  info
}



# File: estimate_RMI_with_bias_reduction.R


#' Estimate Relative Meta-Information
#' 
#' Estimate relative meta-information measure either from a data frame with
#' varibles "y" for labels and confidence-binned "response", for example,
#' with values -2/-1/+1/+2, or from a matrix with counts of observed true
#' labels (rows) and responses (columns).
#' 
#' Remove bias from plug-in RMI estimate by simulating estimated RMI values
#' from the observed data by simulating data sets from the estimated
#' contingency table and subtracting the simulated mean RMI from the RMI of
#' the given table. This is not perfect but the better the actual contingency
#' table is estimated, the more accurately the bias is removed.
#' 
#' @param x Data frame or matrix
#' 
#' @return Relative meta-information with bias reduction value
estimate_RMI_with_bias_reduction <- function(x)
{
  UseMethod("estimate_RMI_with_bias_reduction")
}

estimate_RMI_with_bias_reduction.table <- function(tab)
{  
  estimated_classifier <- tab/sum(tab)
  ns <- rowSums(tab)
  
  # Estimate bias
  estimated_RMI  <- get_RMI(estimated_classifier)
  expected_RMI   <- get_expected_RMI(estimated_classifier, ns)
  estimated_bias <- expected_RMI - estimated_RMI

  # Subtract estimated bias
  estimated_RMI_debiased <- estimated_RMI - estimated_bias
  estimated_RMI_debiased

  estimated_RMI_debiased
}

#' Get Expected RMI
#' 
#' Simulate classifier by row-wise sampling ns[j] observations for the j-th
#' row from its joint probability distribution. For each simulation, compute
#' the mean RMI. Repeat simulations until the mean RMI is sufficiently
#' precisely determined.
#' 
#' @param classifier Classifier as joint probability distribution matrix
#' @param ns Number of trials per label (row)
#' 
#' @return Simulated estimate for the mean RMI to be expected from sampling
#'  the classifier.
get_expected_RMI <- function(classifier, ns)
{
  nsim      <- 1000
  se        <- Inf
  se_crit   <- .001 # Desired standard error from simulation noise
  precision <- 0

  simulated_RMIs <- c()
  while (se > se_crit)
  { 
    # Simulate classifier multiple times
    for (i in 1:nsim + precision*nsim)
    {
      # Simulate one classifier row-wise
      counts <- classifier*0
      for (j in 1:nrow(classifier)) 
      {
        counts[j, ] <- rmultinom(1, ns[j], classifier[j, ])
      }
      simulated_classifier <- counts/sum(counts)

      simulated_RMIs[i] <- get_RMI(simulated_classifier)
    }

    simulated_RMIs <- na.omit(simulated_RMIs)
    
    # Calculate standard error and, if too low, repeat simulation
    se <- sd(simulated_RMIs) / sqrt(length(simulated_RMIs))
    precision <- precision + 1
  }
  
  expected_RMI <- mean(simulated_RMIs)
  expected_RMI
}

estimate_RMI_with_bias_reduction.data.frame <- function(msd)
{  
  tab <- table(msd$y, msd$response)

  estimated_RMI <- estimate_RMI_with_bias_reduction.table(tab)
  estimated_RMI
}

estimate_RMI_with_bias_reduction.matrix <- function(estimated_classifier)
{  
  if (sum(estimated_classifier) > 1)
  {
    tab <- as.table(estimated_classifier)
    estimated_RMI <- estimate_RMI_with_bias_reduction(tab)
    return(estimated_RMI)
  }

  # This is not bias-reduced because we would need the number of observations
  # for this. Use a table instead.
  estimated_RMI <- get_RMI(estimated_classifier)
  estimated_RMI
}
