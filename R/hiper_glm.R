#' @export
hiper_glm <- function(design, outcome, optimizer = c("BFGS", "pseudo-inverse")) {

  optimizer <- match.arg(optimizer)

  if (optimizer == "BFGS") {
    warning("This function is yet to be implemented.")
    return(NULL)

  } else if (optimizer == "pseudo-inverse") {

    if (ncol(design) != length(outcome)) {
      stop("The number of columns in the design matrix must match the number of columns in the outcome matrix.")
    }
    if (!all(design[,1] == 1)) {design <- cbind(1, design)}

    beta_hat <- solve(t(design) %*% design, t(design) %*% outcome)
    hglm_out <- list(coefficients = beta_hat)
    class(hglm_out) <- "hglm"
    return(hglm_out)
  } else {
    warning("Invalid optimizer specified.")
  }
}
