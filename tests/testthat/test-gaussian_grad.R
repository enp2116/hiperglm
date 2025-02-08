test_that("Gradient matches numerical one", {

  set.seed(410)
  n_param <- 4
  X <- matrix(rnorm(2 * n_param^2), nrow = 2 * n_param, ncol = n_param)  # Design matrix
  y <- rnorm(2 * n_param)  # Simulated outcome vector
  x <- c(3, 1, 4, 1)

  # Approximate gradient using finite differences
  approx_grad <- function(func, beta, design, outcome, dx = .Machine$double.eps^(1/3)) {
    numerical_grad <- sapply(seq_along(beta), function(i) {
      beta_forward <- beta
      beta_backward <- beta
      beta_forward[i] <- beta[i] + dx
      beta_backward[i] <- beta[i] - dx

      (func(beta_forward, design, outcome) - func(beta_backward, design, outcome)) / (2 * dx)
    })

    return(numerical_grad)
  }

  # Compute analytical and numerical gradients
  analytical_grad <- gaussian_grad(x, X, y)
  numerical_grad <- approx_grad(gaussian_logp, x, X, y)

  print(analytical_grad)
  print(numerical_grad)

  # Compare
  expect_true(are_all_close(
    analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
  ))
})


