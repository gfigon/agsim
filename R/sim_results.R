#' Title
#'
#' @param n_sim_jobs Number of months to simulate.
#' @param jobs_mean_month Number of jobs proccesed monthly.
#' @param eff_min Minimum efficiency per job (FTE confirmed / FTE goal).
#' @param eff_max Maximum efficiency per job (FTE confirmed / FTE goal).
#' @param min_fte Minimum FTEs per job.
#' @param max_fte Maximum FTEs per job.
#' @param min_share_fte Share of jobs with minimal or near number of FTEs.
#' @param min_value Minimum value of one FTE.
#' @param max_value Maximum value of one FTE.
#' @param min_share_value Share of FTEs valued near minimum.
#'
#' @return A tibble with simulated data for each month, job and other factors.
#' @export
#'
#' @examples
#' sim_results(n_sim_jobs = 12, jobs_mean_month = 20,  eff_min = 0.2, eff_max = 0.4, min_fte = 1, max_fte = 5, min_share_fte = 0.5, min_value = 5000, max_value = 15000, min_share_value = 0.5)
sim_results <- function(n_sim_jobs, jobs_mean_month,  eff_min, eff_max, min_fte,
                        max_fte, min_share_fte, min_value, max_value, min_share_value){


  jobs <- sim_jobs_nr(n_sim_jobs, jobs_mean_month)



  effs <- purrr::map(jobs, sim_monthly_eff, eff_min=eff_min, eff_max=eff_max)
  ftes <- purrr::map(jobs, sim_nr_of_FTE_per_job, min_fte, max_fte, min_share_fte)
  values <- purrr::map(jobs, sim_fte_value, min_value, max_value, min_share_value)



  data_frame <- dplyr::tibble()

  for(n in 1:length(effs)){




    for(x in 1:length(effs[[n]])){


      #print(paste(n, x, effs[[n]][x], ftes[[n]][x], values[[n]][x],  sep = "|"))



      data_frame <- data_frame |> dplyr::bind_rows(dplyr::tribble(
        ~month, ~job, ~eff, ~fte, ~value,
        n, x, effs[[n]][x], ftes[[n]][x], values[[n]][x]
      ))


    }


  }

  data_frame



}











