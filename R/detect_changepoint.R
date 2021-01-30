#' Compute aggregate change point(s) on worker-level data
#'
#' Aggregate app-level counts to detect significant change point(s) in counts.
#'
#' @param app_workers_data contains time-series data for each worker over the
#'  same period of time. Requires the following columns:
#'  - date: the date by which activity was recorded.
#'  - worker_id: a unique identifier for the worker.
#'  - count: number of units of activity for the corresponding date/worker.
#' @param app_id unique identifier for the application
#' @param num_cpts maximum number of change points to detect
#' @param include_data whether to include data in response
#' @param include_model whether to include change point model in response
#' @examples
#' num_dates <- 90
#' num_workers <- 30
#' test_data <- merge(paste0("worker_", 1:num_workers),
#'       seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
#'       colnames = c("foo", "bar"))
#' start_date <- Sys.Date() + floor(num_dates / 2)
#' test_data$count <- sapply(1:(num_dates*num_workers),
#'                           function(x) { rnorm(1, 50, 20) })
#' test_data <- setNames(test_data, c("worker_id", "date", "count"))
#' detect_changepoint(
#'   test_data, "test_analysis", include_data = FALSE, include_model = FALSE)
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom changepoint cpt.mean cpts
#' @importFrom rlang .data
detect_changepoint <- function(app_workers_data, app_id, num_cpts = 1,
                               include_data = FALSE, include_model = FALSE) {
  app_agg_data <- app_workers_data %>%
    group_by(.data$date) %>%
    summarise(across(.data$count, sum), .groups = "drop") %>%
    complete(date = seq.Date(min(.data$date), max(.data$date), by = "month"),
             fill = list(count = 0)) %>%
    arrange(.data$date)

  cpt_mdl <- app_agg_data %>% pull(.data$count) %>%
    cpt.mean(method = "BinSeg", Q = num_cpts) %>%
    suppressWarnings()

  if (length(cpts(cpt_mdl)) == 0) {
    warning("detect_changepoint: No change point detected.\n")
    return(NULL)
  }

  cpt_df <- purrr::map_dfr(cpts(cpt_mdl), function(cpt) {
    cpt_row <- app_agg_data %>%
      mutate(app_id = {{ app_id }},
             change_date = .data$date[cpt],
             before_mean = mean(.data$count[.data$date <= .data$change_date]),
             after_mean = mean(.data$count[.data$date > .data$change_date]),
             mean_change = .data$after_mean - .data$before_mean,
             pct_change = .data$mean_change / .data$before_mean)
    if (include_model) {
      cpt_row <- cpt_row %>%
        mutate(cpt_mdl = list(cpt_mdl))
    }
    if (!include_data) {
      cpt_row <- cpt_row %>%
        select(-date, -count) %>%
        distinct()
    } else {
      cpt_row <- cpt_row %>%
        nest_by(across(-c(date, count)))
    }
    return(cpt_row)
  }) %>% bind_rows()
  return(cpt_df)
}
