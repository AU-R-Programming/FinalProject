#' @title Linear Regression Package - Group 1
#'
#' @description Performs linear regression on user-defined vector, matrix, and significance level.
#' Provides statistics related to this model, including:
#' 1) Coefficient vector found via optimization
#' 2) Confidence interval for beta corresponding to the significance level
#' 3) Plots including: a) Residuals vs. fitted values; b) qq-plot if residuals; c) Histogram of residuals
#' 4) R^2 (coefficient of determination) and Cp (Mallow's Cp)
#' 5) F-test statistic and p-value
#' @param y A \code{vector} of length n that is to be approximated
#' @param X A \code{matrix} of dimension n x p that is used to estimate @param y
#' @param a A \code{numeric} value, the significance level used for calculating confidence interval
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{beta}{The coefficient vector found via optimization}
#'      \item{confint}{The confidence interval of beta corresponding to the significance level}
#'      \item{r_sqrd}{The coefficient of determination}
#'      \item{mallow_cp}{Mallow's Cp that assesses fitting}
#'      \item{f_stat}{The F-test statistics for this model}
#'      \item{pf_value}{The p-value for the F-test}
#'      \item{beta}{The coefficient vector found via optimization}
#'      \item{beta}{The coefficient vector found via optimization}
#'      \item{beta}{The coefficient vector found via optimization}
#'      \item{beta}{The coefficient vector found via optimization}
#' }
#' @author Stanley Thomas Wijaya, Ruoyun Mei, Cunhao Li
#' @importFrom ggplot2
#' @export
