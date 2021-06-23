#' @import dplyr
#' @importFrom rlang .data
compute_ts_models <- function(group_timeseries_data, start_date, period,
                              sig_p) {
  group_timeseries_data_wide <- group_timeseries_data %>%
    filter(.data$active) %>% select(-.data$active) %>%
    arrange(date) %>%
    tidyr::pivot_wider(names_from = .data$ts_id, values_from = count,
                       values_fill = 0)

  group_timeseries_zoo <- zoo::zoo(group_timeseries_data_wide %>% select(-date),
                                   group_timeseries_data_wide$date)
  # CausalImpact can't handle variables that begin with numbers
  names(group_timeseries_zoo) <- paste0("ts_", names(group_timeseries_zoo))

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

  # CausalImpact model per ts

  date_idx <- zoo::index(group_timeseries_zoo)
  pre_period <- range(date_idx[date_idx <= start_date])
  post_period <- range(date_idx[date_idx > start_date])
  group_timeseries_model <- lapply(1:ncol(group_timeseries_zoo), function(i) {
    ts_dat <- cbind(group_timeseries_zoo[,c(i)], group_timeseries_zoo[,-i])
    names(ts_dat) <- c(names(group_timeseries_zoo)[i],
                       names(group_timeseries_zoo)[-i])
    ts_mdl <- CausalImpact::CausalImpact(
      ts_dat, pre_period, post_period, alpha = sig_p,
      # monthly seasonal component
      model.args = list(nseasons = num_seasons,
                        season.duration = season_duration,
                        standardize.data = FALSE))
    return(ts_mdl)
  })

  names(group_timeseries_model) <- names(group_timeseries_zoo)
  return(group_timeseries_model)
}
