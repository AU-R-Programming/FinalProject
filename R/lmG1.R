#' @title Linear Regression with Optimized Beta - Final Project Group 1
#'
#' @description This function performs linear regression on user-defined
#' predictor variable X, response variable y and significance level alpha where
#' the parameter vector beta is estimated using optimization. Additionally, the
#' function provides statistics related to the linear regression model, including:
#'
#' 1) Estimated coefficient vector beta found via optimization
#'
#' 2) Confidence interval for beta corresponding to the significance level provided by the user
#'
#' 3) Plots with ggplot2 including: a) Residuals vs. fitted values; b) qq-plot of residuals; c) Histogram of residuals
#'
#' 4) Model evaluation metric values including: a) R^2 (coefficient of determination); b) Cp (Mallow's Cp)
#'
#' 5) F-test statistic and p-value
#'
#' @param y A \code{vector} of length n representing the response variable that is to be predicted
#' @param X A \code{matrix} of dimension n x p representing the predictor variables that is used to estimate @param y
#' @param alpha A \code{numeric} value, the significance level used for calculating confidence interval. The default value is 0.05
#' @param plot A \code{boolean} value representing whether the plots should be printed when running the linear regression function
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{beta}{The coefficient vector beta found via optimization}
#'      \item{y_pred}{The predicted values for the response variable y}
#'      \item{sigma2}{The estimation of the residual variance}
#'      \item{variance_beta}{The estimation of the variance of beta}
#'      \item{confint}{The confidence interval of beta corresponding to the significance level}
#'      \item{r_sqrd}{The coefficient of determination which indicates how much variation of y is explained by X}
#'      \item{mallow_cp}{Mallow's Cp which assesses the fit of the model}
#'      \item{f_stat}{The F-test statistics}
#'      \item{pf_value}{The p-value for the F-test, specifically P(F > F*)}
#' }
#' @author Stanley Thomas Wijaya, Ruoyun Mei, Cunhao Li
#' @import ggplot2
#' @import patchwork
#' @export
#' @examples
#' library(lmG1)
#' set.seed(1)
#' crop_data <- read.csv("crop.data.csv", header = TRUE)
#' crop_data <- na.omit(crop_data)
#' density <- as.factor(crop_data$density)
#' block <- as.factor(crop_data$block)
#' fertilizer <- as.factor(crop_data$fertilizer)
#' X <- cbind(density, block, fertilizer)
#' y <- crop_data$yield
#' result <- lmG1(y, X, alpha = 0.05, plot = TRUE)
#'
#' result$beta
#' result$y_pred
#' result$sigma2
#' result$variance_beta
#' result$confint
#' result$r_sqrd
#' result$mallow_cp
#' result$f_stat
#' result$pf_value
lmG1 <- function(y, X, alpha = 0.05, plot = FALSE) {

  library(ggplot2)
  library(patchwork)

  # make sure the response variable is a vector
  # and predictor variable is a matrix
  y <- as.vector(y)
  X <- as.matrix(X)

  # add vector of ones to be first column of X
  ones <- rep(1, nrow(X))
  X <- cbind(ones, X)

  # define parameters
  n <- length(y)
  p <- ncol(X)
  df <- n - p

  # fill initial value for beta
  beta_0 <- rep(NA, p)
  beta_0[1] <- mean(y)
  for (i in 2:p) {
    beta_0[i] <- (cov(y, X[, i]))/(var(X[, i]))
  }

  # minimization routine of beta in matrix form
  minimize_beta <- function(b) {
    (t(y - X%*%b))%*%(y - X%*%b)
  }

  # perform optimization based on the minimize_beta function
  beta_hat <- optim(par = beta_0, fn = minimize_beta)$par

  # predicted values for y
  y_hat = X%*%beta_hat

  # estimation of the residual variance (sigma2)
  residual <- y - X%*%as.matrix(beta_hat)
  sigma2_hat <- (1/df)*t(residual)%*%residual

  # estimation of the variance of the estimated beta
  # note the solve function is to find the inverse of the matrix
  var_beta <- as.numeric(sigma2_hat) * diag(solve(t(X)%*%X))

  # estimation of confidence interval based on alpha
  quant <- 1 - alpha/2
  ci_beta <- c(beta_hat - qnorm(p = quant, mean = 0, sd = 1)*sqrt(var_beta),
               beta_hat + qnorm(p = quant, mean = 0, sd = 1)*sqrt(var_beta))

  # evaluate the linear model using 2 different metrics:
  # 1. Coefficient of Determination (R^2)
  SSE <- sum((y - y_hat)^2)
  SST <- sum((y - mean(y))^2)
  R_squared <- 1 - (SSE / SST)

  # 2. Mallow's Cp
  Cp <- SSE + 2 * p * sigma2_hat

  # perform a F-test
  DFM <- p - 1
  DFE <- n - p
  SSM <- sum((y_hat - mean(y))^2)
  MSM <- SSM / DFM
  MSE <- SSE / DFE
  F_stat <- MSM / MSE

  # compute the p-value for the f-test: P(F > F_stat)
  f_test_p_val <- pf(F_stat, DFM, DFE, lower.tail = FALSE)

  if (plot == TRUE) {
    # residual vs fitted-values plot
    res_fit <- ggplot(mapping = aes(x = y_hat, y = residual)) +
      labs(title = 'Residuals vs Fitted', x = 'Fitted values', y = 'Residuals') +
      geom_point() + geom_smooth()

    # qq-plot of residuals
    qq <- ggplot(mapping = aes(sample = residual)) +
      labs(title = 'Residual QQ-plot', x = 'Theoretical Quantiles', y = 'Sample Quantiles') +
      stat_qq()

    # histogram (or density) of residuals
    hist <- ggplot(mapping = aes(x = residual)) +
      geom_histogram(fill = 'steelblue', color = 'black') +
      labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

    print(res_fit + qq + hist)
  }

  # return all estimated values
  return(list(beta = beta_hat, y_pred = y_hat, sigma2 = sigma2_hat,
              variance_beta = var_beta, confint = ci_beta, r_sqrd = R_squared,
              mallow_cp = Cp, f_stat = F_stat, pf_value = f_test_p_val))

}
