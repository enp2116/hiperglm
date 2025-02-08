test_that("Linear algebra and Optim() least squares coincide", {

  # Simulate data
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  outcome <- data$outcome
  design <- data$design
  outcome <- matrix(outcome, ncol = 1)

  # MLE using analytical formula
  linalg_out <- hiper_glm(design, outcome, model = "linear",
                          optimizer = "pseudo-inverse")

  # MLE using stats::optim()
  optim_out <- hiper_glm(design, outcome, model = "linear",
                         optimizer = "BFGS")

  print(coef(linalg_out))
  print(coef(optim_out))

  # Compare
  expect_true(are_all_close(coef(linalg_out), coef(optim_out),
                            abs_tol = 1e-3, rel_tol = 1e-3))
})

