#' Compute worker anomaly detection models for all workers within an app.
#'
#' Compare each worker to a set of comparable workers in the pre-period and
#' evaluate deviation from average in the post-period to detect anomalous
#' behavior.
#'
#' @param app_workers_data contains time-series data for each worker over the
#'  same period of time. Requires the following columns:
#'  - date: the date by which activity was recorded.
#'  - worker_id: a unique identifier for the worker.
#'  - count: number of units of activity for the corresponding date/worker.
#' @param app_id unique identifier for the application
#' @param start_date date seperating the pre-period (matching) vs post-period
#'  evaluating. Uses detect_changepoint if missing (NULL).
#' @param period unit of time to compare workers with.
#' @param min_pre_periods number of time units to qualify as an active worker in
#'  pre-period.
#' @param min_post_periods number of time units to qualify as an active worker
#'  in post-period.
#' @param min_workers minimum number of active workers to run the analysis.
#' @param sig_p p-value threshold to determine statistical significance.
#' @param save_model_data save_model_data whether to save analysis data.
#' @param save_model save_model whether to save user models.
#' @param use_cache use saved data and models from previous run.
#' @return app_id string for compeleted app worker analysis
#' @examples
#' set.seed(143)
#' num_dates <- 90
#' num_workers <- 30
#' # generate synthetic data
#' test_data <- setNames(
#'   merge(as.character(1:num_workers),
#'         seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
#'         colnames = c("worker_id", "date")), c("worker_id", "date"))
#' start_date <- Sys.Date() + floor(num_dates / 2)
#' test_data$count <- sapply(1:(num_dates*num_workers),
#'                           function(x) { rnorm(1, 50, 20) })
#  # worker anomalies
#' test_data[test_data$worker_id == "1" &
#'             test_data$date > start_date, "count"] <- 99
#' test_data[test_data$worker_id == "2" &
#'             test_data$date > start_date, "count"] <- 1
#' output_dir <- worker_analysis(
#'   test_data, "test_analysis", start_date = start_date, period = "day",
#'   sig_p = 0.05 / num_workers) # bonferroni correction
#' paste("worker analysis output in directory:", output_dir)
#' @export
#' @import dplyr

worker_analysis <- function(app_workers_data,
                            app_id = NA,
                            start_date = NULL,
                            period = "month",
                            min_pre_periods,
                            min_post_periods,
                            min_workers = 30,
                            sig_p = 0.05,
                            save_model_data = TRUE,
                            save_model = TRUE,
                            use_cache = FALSE) {
  # arg checks
  req_cols <- c("date", "worker_id", "count")
  default_app_id <- as.numeric(lubridate::now())
  if (is.null(start_date)) {
    cpt <- detect_changepoint(app_workers_data, app_id, num_cpts = 1)
    start_date <- unique(cpt$change_date)
    warning(paste("start_date not specified, detected change date:",
                  start_date))
  }

  # compute default pre/post periods
  if (missing(min_pre_periods)) {
    month_periods <- unique(lubridate::floor_date(app_workers_data$date,
                                                  "month"))
    min_pre_periods <- floor(sum(month_periods <= start_date) * 0.5)
  }
  if (missing(min_post_periods)) {
    month_periods <- unique(lubridate::floor_date(app_workers_data$date,
                                                  "month"))
    min_post_periods <- floor(sum(month_periods > start_date) * 0.25)
  }
  if (mean(req_cols %in% names(app_workers_data)) != 1) {
    stop(paste("Missing columns in app_workers_data:",
               setdiff(req_cols, names(app_workers_data))))
  }
  if (nrow(app_workers_data) < (min_workers * (min_pre_periods +
                                               min_post_periods))) {
    stop(paste("Not enough rows in app_workers_data",
               "(min_workers* (min_pre_periods + min_post_periods:",
               min_workers* (min_pre_periods + min_post_periods)))
  }
  if (!between(start_date, min(app_workers_data$date),
               max(app_workers_data$date))) {
    stop(paste("start_date", start_date,
               "not within range(app_workers_data$date):",
               paste(range(app_workers_data$date), collapse = " to ")))
  }

  # set app_id
  if (is.na(app_id)) {
    warning(paste("app_id is NA, setting to default:", default_app_id))
    app_id <- default_app_id
  }
  warning(paste("computing worker models for app_id:", app_id))

  # output directory
  fs::dir_create(app_id)

  # prep app_workers_data
  model_data_file <- path(app_id, "app_workers_data.rds")
  if (file.exists(model_data_file) && use_cache) {
    app_workers_data <- readRDS(model_data_file)
  } else {
    app_workers_data <- prep_app_workers(
      app_workers_data, period, min_pre_periods, min_post_periods, min_workers,
      start_date)
    if (save_model_data) {
      saveRDS(app_workers_data, model_data_file)
    }
  }

  # run model
  model_file <- path(app_id, "app_workers_model.rds")
  if (file.exists(model_file) && use_cache) {
    app_workers_model <- readRDS(model_file)
  } else {
    app_workers_model <- compute_worker_models(app_workers_data,
                                               start_date = start_date,
                                               period = period, sig_p = sig_p)
    if (save_model) {
      saveRDS(app_workers_model, model_file)
    }
  }

  # run output
  generate_output(app_workers_model, app_workers_data, app_id, sig_p)
}
