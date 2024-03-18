#' Title
#'
#' @param n_sim Number of months to simulate.
#' @param eff_min Minimum monthly efficiency.
#' @param eff_max Maximum monthly efficiency.
#'
#' @return A numeric vector with values between 0 and 1.
#' @export
#'
#' @examples
#' sim_monthly_eff(12, 0.2, 0.5)
sim_monthly_eff <- function(n_sim, eff_min, eff_max){
  stats::runif(n_sim, eff_min, eff_max)
  }
