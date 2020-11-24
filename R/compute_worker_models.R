#' @import dplyr
#' @importFrom rlang .data
compute_worker_models <- function(app_workers_data, period = "month",
                                  start_date = as.POSIXct("2020-03-01"),
                                  sig_p = 0.05) {
  app_workers_data <- app_workers_data %>%
    filter(.data$active) %>% select(-.data$active) %>%
    arrange(date) %>%
    tidyr::pivot_wider(names_from = .data$worker_id, values_from = count,
                       values_fill = 0)

  app_workers_zoo <- zoo::zoo(app_workers_data %>% select(-date),
                              app_workers_data$date)
  # CausalImpact can't handle variables that begin with numbers
  names(app_workers_zoo) <- paste0("worker_",
                                   names(app_workers_zoo))

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
  app_workers_model <- lapply(1:ncol(app_workers_zoo), function(i) {
    worker_dat <- cbind(app_workers_zoo[,c(i)], app_workers_zoo[,-i])
    names(worker_dat) <- c(names(app_workers_zoo)[i],
                           names(app_workers_zoo)[-i])
    worker_mdl <- CausalImpact::CausalImpact(
      worker_dat,
      c(min(app_workers_data$date), start_date),
      c(start_date, max(app_workers_data$date)),
      alpha = sig_p,
      # monthly seasonal component
      model.args = list(nseasons = num_seasons,
                        season.duration = season_duration))
    return(worker_mdl)
  })

  names(app_workers_model) <- names(app_workers_zoo)
  return(app_workers_model)
}
