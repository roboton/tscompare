#' @import dplyr
#' @importFrom rlang .data
compute_worker_models <- function(app_workers_data, start_date, period,
                                  sig_p) {
  app_workers_data_wide <- app_workers_data %>%
    filter(.data$active) %>% select(-.data$active) %>%
    arrange(date) %>%
    tidyr::pivot_wider(names_from = .data$worker_id, values_from = count,
                       values_fill = 0)

  app_workers_zoo <- zoo::zoo(app_workers_data_wide %>% select(-date),
                              app_workers_data_wide$date)
  # CausalImpact can't handle variables that begin with numbers
  names(app_workers_zoo) <- paste0("worker_",
                                   names(app_workers_zoo))

  # CausalImpact can only handle one seasonality adjustment (use bsts for more)
  season_duration <- 1
  period <- "month"
  if (period == "month") {
    num_seasons <- 12
  } else if (period == "week") {
    num_seasons <- 52
  } else if (period == "day") {
    num_seasons <- 7
  }

  # CausalImpact model per worker

  date_idx <- zoo::index(app_workers_zoo)
  pre_period <- range(date_idx[date_idx <= start_date])
  post_period <- range(date_idx[date_idx > start_date])
  app_workers_model <- lapply(1:ncol(app_workers_zoo), function(i) {
    worker_dat <- cbind(app_workers_zoo[,c(i)], app_workers_zoo[,-i])
    names(worker_dat) <- c(names(app_workers_zoo)[i],
                           names(app_workers_zoo)[-i])
    worker_mdl <- CausalImpact::CausalImpact(
      worker_dat, pre_period, post_period, alpha = sig_p,
      # monthly seasonal component
      model.args = list(nseasons = num_seasons,
                        season.duration = season_duration,
                        standardize.data = FALSE))
    return(worker_mdl)
  })

  names(app_workers_model) <- names(app_workers_zoo)
  return(app_workers_model)
}
