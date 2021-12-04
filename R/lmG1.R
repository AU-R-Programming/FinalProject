lmG1 <- function(y, X, alpha = 0.05, plot = FALSE) {

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
    library(ggplot2)
    library(patchwork)

    # residual vs fitted-values plot
    res_fit <- ggplot(data = Boston, aes(x = y_hat, y = residual)) +
      labs(title = 'Residuals vs Fitted', x = 'Fitted values', y = 'Residuals') +
      geom_point() + geom_smooth()

    # qq-plot of residuals
    qq <- ggplot(Boston, aes(sample = residual)) +
      labs(title = 'Residual QQ-plot', x = 'Theoretical Quantiles', y = 'Sample Quantiles') +
      stat_qq()

    # histogram (or density) of residuals
    hist <- ggplot(data = Boston, aes(x = residual)) +
      geom_histogram(fill = 'steelblue', color = 'black') +
      labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

    print(res_fit + qq + hist)
  }

  # return all estimated values
  return(list(beta = beta_hat, y_pred = y_hat, sigma2 = sigma2_hat,
              variance_beta = var_beta, confint = ci_beta, r_sqrd = R_squared,
              mallow_cp = Cp, f_stat = F_stat, pf_value = f_test_p_val))

}



# testing function with Boston dataset
# plot determines whether the residual vs fitted, qq-plot of residuals
# and histogram of residuals graphs are shown or not
library(MASS)
lmG1_results <- lmG1(Boston$medv, Boston[-ncol(Boston)], plot = TRUE)

# printing each of the results
lmG1_results$beta
lmG1_results$y_pred
lmG1_results$sigma2
lmG1_results$variance_beta
lmG1_results$confint
lmG1_results$r_sqrd
lmG1_results$mallow_cp
lmG1_results$f_stat
lmG1_results$pf_value
