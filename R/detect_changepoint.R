#' Compute aggregate change point(s) on ts-level data
#'
#' Aggregate group-level counts to detect significant change point(s) in counts.
#'
#' @param group_timeseries_data contains time-series data for each ts over the
#'  same period of time. Requires the following columns:
#'  - date: the date by which activity was recorded.
#'  - ts_id: a unique identifier for the ts.
#'  - count: number of units of activity for the corresponding date/ts.
#' @param group_id unique identifier for the group
#' @param num_cpts maximum number of change points to detect
#' @param include_data whether to include data in response
#' @param include_model whether to include change point model in response
#' @examples
#' num_dates <- 90
#' num_timeseries <- 30
#' test_data <- merge(paste0("ts_", 1:num_timeseries),
#'       seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
#'       colnames = c("foo", "bar"))
#' start_date <- Sys.Date() + floor(num_dates / 2)
#' test_data$count <- sapply(1:(num_dates*num_timeseries),
#'                           function(x) { rnorm(1, 50, 20) })
#' test_data <- setNames(test_data, c("ts_id", "date", "count"))
#' detect_changepoint(
#'   test_data, "test_analysis", include_data = FALSE, include_model = FALSE)
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom changepoint cpt.mean cpts
#' @importFrom rlang .data
detect_changepoint <- function(group_timeseries_data, group_id, num_cpts = 1,
                               include_data = FALSE, include_model = FALSE) {
  group_agg_data <- group_timeseries_data %>%
    group_by(.data$date) %>%
    summarise(across(.data$count, sum), .groups = "drop") %>%
    complete(date = seq.Date(min(.data$date), max(.data$date), by = "month"),
             fill = list(count = 0)) %>%
    arrange(.data$date)

  cpt_mdl <- group_agg_data %>% pull(.data$count) %>%
    cpt.mean(method = "BinSeg", Q = num_cpts) %>%
    suppressWarnings()

  if (length(cpts(cpt_mdl)) == 0) {
    warning("detect_changepoint: No change point detected.\n")
    return(NULL)
  }

  cpt_df <- purrr::map_dfr(cpts(cpt_mdl), function(cpt) {
    cpt_row <- group_agg_data %>%
      mutate(group_id = {{ group_id }},
             change_date = .data$date[cpt],
             before_mean = mean(.data$count[.data$date <= .data$change_date]),
             after_mean = mean(.data$count[.data$date > .data$change_date]),
             mean_change = .data$after_mean - .data$before_mean,
             pct_change = .data$mean_change / .data$before_mean)
    if (!include_data) {
      cpt_row <- cpt_row %>%
        select(-date, -count) %>%
        distinct()
    } else {
      cpt_row <- cpt_row %>%
        nest_by(across(-c(date, count)))
    }
    if (include_model) {
      cpt_row <- cpt_row %>%
        mutate(cpt_mdl = list(cpt_mdl))
    }
    return(cpt_row)
  }) %>% bind_rows()
  return(cpt_df)
}
