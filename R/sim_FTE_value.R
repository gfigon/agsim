#' Title
#'
#' @param n_sim Nr of values to simulate.
#' @param min_value Minimum value of FTE.
#' @param max_value Maximum value of FTE.
#' @param min_share Share of minimal values.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' sim_fte_value(12, 5000, 24000, 0.5)
sim_fte_value <- function(n_sim, min_value, max_value, min_share){
  mins <- round(n_sim * min_share)
  others <- n_sim - mins

  min_fct <- 0.01
  max_fct <- 0.1
  mins_sim <- vector(mode="numeric", length = mins)
  max_sim <- vector(mode="numeric", length = others)



  min_fct <-  min_value / 100
  max_fct <- max_value / 100

  mins_sim <- stats::runif(mins, min_fct, min_fct * 2) * 100
  max_sim <- stats::runif(others, min_fct, max_fct) * 100



  all_sim <- c(mins_sim, max_sim)
  all_sample <- sample(all_sim, size = length(all_sim))
  trunc(all_sample)

}
