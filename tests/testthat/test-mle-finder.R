test_that("linalg and optim least-sq coincide", {
  # Simulate data
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = linear, seed = 1918)
  outcome <- data$outcome
  design <- data$design

   # Get MLE using analytical formula
   linalg_out <- hiper_glm(design, outcome, model = "linear",
                           option = list(mle_finder = "pseudo-inverse"),

   # Get MLE using stats::optim()
   optim_out <- hiper_glm(design, outcome, model = "linear",
                          option = list(mle_finder = "BFGS")),

   # Compare the two
   expect_true(are_all_close(coef(linalg_out), coef(optim_out),
                             abs_tol = 1e-3, rel_tol = 1e-3)))

})
