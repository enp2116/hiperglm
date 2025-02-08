#' @export
hiper_glm <- function(design, outcome, model = c("linear"), optimizer = c("BFGS", "pseudo-inverse")) {

  optimizer <- match.arg(optimizer)
  model <- match.arg(model)
  outcome <- matrix(outcome, ncol = 1)

  # Design matrix includes an intercept if missing
  if (!all(design[,1] == 1)) {design <- cbind(1, design)}

  if (optimizer == "BFGS") {

    initial <- rep(0, ncol(design))

    # Define negative log-likelihood and gradient functions
    neg_logp <- function(beta) -gaussian_logp(beta, design, outcome, model = model)
    neg_grad <- function(beta) -gaussian_grad(beta, design, outcome, model = model)

    # Optimize using BFGS
    result <- optim(par = initial, fn = neg_logp, gr = neg_grad, method = "BFGS")

    beta_hat <- result$par

    hglm_out <- list(coefficients = beta_hat)
    class(hglm_out) <- "hglm"
    return(hglm_out)

  } else if (optimizer == "pseudo-inverse") {

    # Compute the pseudo-inverse solution using Singular Value Decomposition (SVD)
    decomp <- svd(design)
    D_inv <- diag(1 / decomp$d)
    X_pseudo_inv <- decomp$v %*% D_inv %*% t(decomp$u)
    beta_hat <- X_pseudo_inv %*% outcome

    hglm_out <- list(coefficients = beta_hat)
    class(hglm_out) <- "hglm"
    return(hglm_out)
  } else {
    stop("Invalid optimizer specified.")
  }
}


