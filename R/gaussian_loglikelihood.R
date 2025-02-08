#' Calculate log-likelihood for a linear model
#' @noRd
gaussian_logp <- function(beta, design, outcome, model = "linear", noise_var = 1) {

  if (model != "linear") {
    stop("Only linear models are currently supported.")
  }

  outcome <- matrix(outcome, ncol = 1)

  # Compute residuals
  residuals <- outcome - design %*% beta

  # Compute log-likelihood
  n <- length(outcome)
  log_lik <- -0.5 * n * log(2 * pi * noise_var) - (0.5 / noise_var) * sum(residuals^2)

  return(log_lik)
}

#' Calculate gradient of the Gaussian log-likelihood
#' @noRd
gaussian_grad <- function(beta, design, outcome, model = "linear", noise_var = 1) {

  if (model != "linear") {
    stop("Only linear models are currently supported.")
  }

  outcome <- matrix(outcome, ncol = 1)

  # Compute residuals
  residuals <- outcome - design %*% beta

  # Compute gradient
  grad <- (t(design) %*% residuals) / noise_var

  return(grad)
}


