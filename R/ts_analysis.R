#' Compute ts anomaly detection models for all timeseries within a group.
#'
#' Compare each ts to a set of comparable timeseries in the pre-period and
#' evaluate deviation from average in the post-period to detect anomalous
#' behavior.
#'
#' @param group_timeseries_data contains time-series data for each ts over the
#'  same period of time. Requires the following columns:
#'  - date: the date by which activity was recorded.
#'  - ts_id: a unique identifier for the ts.
#'  - count: number of units of activity for the corresponding date/ts.
#' @param group_id unique identifier for the group
#' @param start_date date separating the pre-period (matching) vs post-period
#'  evaluating. Uses detect_changepoint if missing (NULL).
#' @param period unit of time to compare timeseries with.
#' @param min_pre_periods number of time units to qualify as an active ts in
#'  pre-period.
#' @param min_post_periods number of time units to qualify as an active ts
#'  in post-period.
#' @param min_timeseries minimum number of active timeseries to run the analysis.
#' @param sig_p p-value threshold to determine statistical significance.
#' @param save_model_data save_model_data whether to save analysis data.
#' @param save_model save_model whether to save user models.
#' @param use_cache use saved data and models from previous run.
#' @return group_id string for completed group ts analysis
#' @examples
#' set.seed(143)
#' num_dates <- 90
#' num_timeseries <- 30
#' # generate synthetic data
#' test_data <- setNames(
#'   merge(as.character(1:num_timeseries),
#'         seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
#'         colnames = c("ts_id", "date")), c("ts_id", "date"))
#' start_date <- Sys.Date() + floor(num_dates / 2)
#' test_data$count <- sapply(1:(num_dates*num_timeseries),
#'                           function(x) { rpois(1, 50) })
#' # ts anomalies
#' test_data[test_data$ts_id == "1" &
#'             test_data$date > start_date, "count"] <- 100
#' test_data[test_data$ts_id == "2" &
#'             test_data$date > start_date, "count"] <- 1
#'
#' output_dir <- ts_analysis(
#'   test_data, "test_analysis", start_date = start_date, period = "day",
#'   sig_p = 0.01)
#' paste("ts analysis output in directory:", output_dir)
#' @export
#' @import dplyr

ts_analysis <- function(group_timeseries_data,
                        group_id = NA,
                        start_date = NULL,
                        period = "month",
                        min_pre_periods,
                        min_post_periods,
                        min_timeseries = 30,
                        sig_p = 0.05,
                        save_model_data = TRUE,
                        save_model = TRUE,
                        use_cache = FALSE) {
  # arg checks
  req_cols <- c("date", "ts_id", "count")
  default_group_id <- as.numeric(lubridate::now())
  if (is.null(start_date)) {
    cpt <- detect_changepoint(group_timeseries_data, group_id, num_cpts = 1)
    start_date <- unique(cpt$change_date)
    warning(paste("start_date not specified, detected change date:",
                  start_date))
  }

  # compute default pre/post periods
  if (missing(min_pre_periods)) {
    month_periods <- unique(lubridate::floor_date(group_timeseries_data$date,
                                                  "month"))
    min_pre_periods <- floor(sum(month_periods <= start_date) * 0.5)
  }
  if (missing(min_post_periods)) {
    month_periods <- unique(lubridate::floor_date(group_timeseries_data$date,
                                                  "month"))
    min_post_periods <- floor(sum(month_periods > start_date) * 0.25)
  }
  if (mean(req_cols %in% names(group_timeseries_data)) != 1) {
    stop(paste("Missing columns in group_timeseries_data:",
               setdiff(req_cols, names(group_timeseries_data))))
  }
  if (nrow(group_timeseries_data) < (min_timeseries * (min_pre_periods +
                                                       min_post_periods))) {
    stop(paste("Not enough rows in group_timeseries_data",
               "(min_timeseries* (min_pre_periods + min_post_periods:",
               min_timeseries* (min_pre_periods + min_post_periods)))
  }
  if (!between(start_date, min(group_timeseries_data$date),
               max(group_timeseries_data$date))) {
    stop(paste("start_date", start_date,
               "not within range(group_timeseries_data$date):",
               paste(range(group_timeseries_data$date), collapse = " to ")))
  }

  # set group_id
  if (is.na(group_id)) {
    warning(paste("group_id is NA, setting to default:", default_group_id))
    group_id <- default_group_id
  }
  warning(paste("computing ts models for group_id:", group_id))

  # output directory
  fs::dir_create(group_id)

  # prep group_timeseries_data
  model_data_file <- path(group_id, "group_timeseries_data.rds")
  if (file.exists(model_data_file) && use_cache) {
    group_timeseries_data <- readRDS(model_data_file)
  } else {
    group_timeseries_data <- prep_group_timeseries(
      group_timeseries_data, period, min_pre_periods, min_post_periods, min_timeseries,
      start_date)
    if (save_model_data) {
      saveRDS(group_timeseries_data, model_data_file)
    }
  }

  # run model
  model_file <- path(group_id, "group_timeseries_model.rds")
  if (file.exists(model_file) && use_cache) {
    group_timeseries_model <- readRDS(model_file)
  } else {
    group_timeseries_model <- compute_ts_models(group_timeseries_data,
                                                start_date = start_date,
                                                period = period, sig_p = sig_p)
    if (save_model) {
      saveRDS(group_timeseries_model, model_file)
    }
  }
  print(model_file)
  generate_output(group_timeseries_model, group_timeseries_data, group_id, sig_p)
}
