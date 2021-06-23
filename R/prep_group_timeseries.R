#' @import dplyr
#' @importFrom rlang .data
prep_group_timeseries <- function(
  group_timeseries_data, period, min_pre_periods, min_post_periods, min_timeseries,
  start_date) {

  group_timeseries_data <- group_timeseries_data %>% # distinct() %>%
    mutate(date = lubridate::floor_date(date, period)) %>%
    group_by(.data$ts_id, .data$date) %>%
    summarise(across(count, sum), .groups = "drop_last") %>%
    mutate(active = sum(date <= start_date) >= min_pre_periods &
             sum(date > start_date) >= min_post_periods) %>%
    ungroup() %>%
    # tidyr::nesting doesn't recognize .data
    tidyr::complete(tidyr::nesting(ts_id, active),
                    date = seq.Date(min(.data$date), max(.data$date),
                                    by = period),
                    fill = list(count = 0)) %>%
    arrange(.data$active, .data$ts_id, .data$date)

  num_active_timeseries <-
    length(unique(group_timeseries_data$ts_id[group_timeseries_data$active]))
  if (num_active_timeseries < min_timeseries) {
    stop(paste("Does not meet minimum number of timeseries:",
               num_active_timeseries, "<", min_timeseries))
  }
  return(group_timeseries_data)
}
