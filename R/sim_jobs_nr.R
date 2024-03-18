#' Title
#'
#' @param n_sim Number of months to simulate.
#' @param jobs_mean_month Mean jobs per month.
#'
#' @return A numeric vector of n_sim size.
#' @export
#'
#' @examples
#' sim_jobs_nr(60, 20)
sim_jobs_nr <- function(n_sim, jobs_mean_month){
  stats::rpois(n_sim, jobs_mean_month)

}
