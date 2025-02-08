test_that("are_all_close() works correctly", {
  x <- 5
  y <- 5.0000001
  expect_true(are_all_close(x, y, rel_tol = 1e-6, abs_tol = 1e-6))

  z <- 5.00001
  expect_false(are_all_close(x, z, rel_tol = 1e-6, abs_tol = 1e-12))
  expect_false(are_all_close(x, z, rel_tol = 1e-3, abs_tol = 1e-6))

})

