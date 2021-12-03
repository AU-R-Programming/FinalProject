lmG1 <- function(y, X, alpha = 0.05) {

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
  print("SIGMA_2_HAT")
  print(sigma2_hat)
  print("SOLVE XTX")
  print(solve(t(X)%*%X))
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

  # return all estimated values
  return(list(beta = beta_hat, y_pred = y_hat, sigma2 = sigma2_hat,
              variance_beta = var_beta, confint = ci_beta, r_sqrd = R_squared,
              mallow_cp = Cp, f_stat = F_stat, pf_value = f_test_p_val))

}

# testing function with Boston dataset
library(MASS)
lmG1_results <- lmG1(Boston$medv, Boston[-ncol(Boston)])
