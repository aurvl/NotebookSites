#' Pbgtest_feols: Performs Breusch-Godfrey/Wooldridge test for serial
#' correlation in panel data using Fixed Effects Ordinary Least Squares (FEOLS)
#' estimation.
#'Args:
#   model: Fitted fixest model object.
#
#' This function calculates the Breusch-Godfrey/Wooldridge test statistic to
#' assess serial correlation in panel data models. It computes the test statistic
#' and corresponding p-value, indicating whether there is evidence of serial
#' correlation in the idiosyncratic errors. The test is conducted under the
#' assumption of Fixed Effects Ordinary Least Squares (FEOLS) estimation.
#' @param model
#'
#' @return the results of the pdg test on fixest objects
#' @export
#'
#' @examples
#' pbgtest_feols(feols(y ~ x | i + t, data = pdata, panel.id = c('i', 't')))
pbgtest_feols <- function(model) {
  # Extract residuals
  resid <- resid(model)

  # Create lagged residuals
  lag_resid <- stats::lag(resid, -1)

  # Create a matrix of lagged residuals
  X <- cbind(1, lag_resid)

  # Fit a Prais-Winsten regression model of residuals on lagged residuals
  reg <- stats::lm(resid ~ X - 1)

  # Extract test statistic and degrees of freedom
  chisq <- summary(reg)$fstatistic[1] * reg$df.residual
  df <- length(lag_resid) - 1

  # Calculate p-value
  p_value <- stats::pchisq(chisq, df, lower.tail = FALSE)

  # Return results
  cat("Breusch-Godfrey/Wooldridge test for serial correlation\n")
  cat("chisq =", round(chisq, 4), ", df =", df, ", p-value < 0.05 : ", ifelse(p_value < 0.05, "TRUE", "FALSE"), "\n")
  cat("alternative hypothesis: serial correlation in idiosyncratic errors\n")

  return()
}
