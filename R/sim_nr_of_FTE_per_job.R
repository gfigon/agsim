#' Title
#'
#' @param n_sim Nr of months to simulate.
#' @param min_fte Minimum FTE per process.
#' @param max_fte Maximum FTE per process.
#' @param min_share Share of low FTE processes.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' sim_nr_of_FTE_per_job(12, 1,10,0.5)
sim_nr_of_FTE_per_job <- function(n_sim, min_fte, max_fte, min_share){
  mins <- round(n_sim * min_share)
  others <- n_sim - mins

  min_fct <- 0.01
  max_fct <- 0.1
  mins_sim <- vector(mode="numeric", length = mins)
  max_sim <- vector(mode="numeric", length = others)


  if(max_fte <= 100){
    min_fct <-  min_fte / 100
    max_fct <- max_fte / 100

    mins_sim <- stats::runif(mins, min_fct, min_fct * 2) * 100
    max_sim <- stats::runif(others, min_fct, max_fct) * 100

  } else{
    min_fct <-  min_fte / 100
    max_fct <- 100 / 100

    mins_sim <- stats::runif(mins, min_fct, min_fct * 2) * 100
    max_sim <- stats::runif(others, min_fct, max_fct) * 100


  }

  all_sim <- c(mins_sim, max_sim)
  all_sample <- sample(all_sim, size = length(all_sim))
  trunc(all_sample)

}
