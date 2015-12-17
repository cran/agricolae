#' qtukey
#'
#' Update the native stats::qtukey function to make it more stable
#'
#' @param p a vector of probabilities
#' @param nmeans the number of gaussians distributions
#' @param df the degree of freedom
#'
#' @return The quantiles values of the Student distribution
#' @export
#' 
qtukey <- Vectorize(function(p, nmeans, df, ...) {
  tryCatch(
    # It is possible that the native R qtukey function does not converge. (for nmeans big)
    # If so, "Tprob" will be NaN, so does "minimo", and "s" will be NA
    # This will lead to an error in the "if(s)" statement
    # -> We want to fail faster than that.
    return(stats::qtukey(p, nmeans, df, ...)),
    warning = function(w) {
      if("convergence failed in 'qtukey'" == w$message) {
        # We can manually found the qtukey value whithout inversing the ptukey function. (more stable)
        f <- Vectorize(function(x) stats::ptukey(x, nmeans, df) - p)
        warning(simpleWarning("The qtukey value was manually found, since the native qtukey function did not converge"))
        return(uniroot(f, c(-10,10))$root)
      } else warning(w)
    }
  )
})
