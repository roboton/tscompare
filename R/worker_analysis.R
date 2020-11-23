#' Compute worker anomaly detection models for all workers within an app.
#'
#' Compare each worker to a set of comparable workers in the pre-period and
#' evaluate deviation from average in the post-period to detect anomalous
#` behavior.
#'
#' @param app_workers_data contains time-series data for each worker over a
#`  the same period of time. Requires the following columns:
#`  - date: the date by which activity was recorded.
#`  - worker_id: a unique identifier for the worker.
#`  - count: number of units of activity for the corresponding date/worker.
#` @param app_id unique identifier for the application
#` @param start_date date seperating the pre-period (matching) vs post-period
#`  evaluating.
#` @param period unit of time to compare workers with.
#` @param min_pre_periods number of time units to qualify as an active worker in #`
#`  pre-period.
#` @param min_post_periods number of time units to qualify as an active worker in #`
#`  post-period.
#` @param min_workers minimum number of active workers to run the analysis.
#` @param sig_p p-value threshold to determine statistical significance.
#` @param save_model_data save_model_data whether to save analysis data.
#` @param save_model save_model whether to save user models.
#` @param save_output save_output whether to save model output.

worker_analysis <- function(app_workers_data,
                            app_id = NA,
                            # date to start anomaly detection
                            start_date = as.POSIXct("2020-03-01"),
                            # unit of time to do analysis
                            period = "month",
                            # number of time units to be an active worker
                            # in the pre-period
                            min_pre_periods,
                            # number of time units to be an active worker
                            # in the post-period
                            min_post_periods,
                            # minimum workers args
                            min_workers = 30,
                            sig_p = 0.05,
                            # save data assets
                            save_model_data = TRUE,
                            save_model = TRUE,
                            save_output = TRUE) {
  # arg checks
  req_cols <- c("date", "worker_id", "count")
  default_app_id <- as.numeric(now())

  if (missing(min_pre_periods)) {
    if (period == "month") {
      min_pre_periods <- 6
    } else if (period == "week") {
      min_pre_periods <- 24
    } else if (period == "day") {
      min_pre_periods <- 180
    }
  }
  if (missing(min_post_periods)) {
    if (period == "month") {
      min_post_periods <- 1
    } else if (period == "week") {
      min_post_periods <- 4
    } else if (period == "day") {
      min_post_periods <- 30
    }
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
    stop(paste("start_date not within range(app_workers_data$date):",
               range(app_workers_data$date)))
  }

  # set app_id
  if (is.na(app_id)) {
    warning(paste("app_id is NA, setting to default:", default_app_id))
    app_id <- default_app_id
  }

  # output directory
  dir.create(app_id, recursive = TRUE)

  # prep app_workers_data
  model_data_file <- file.path(app_id, "app_workers_data.rds")
  if (file.exists(model_data_file)) {
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
  model_file <- file.path(app_id, "app_workers_model.rds")
  if (file.exists(model_file)) {
    app_workers_model <- readRDS(model_file)
  } else {
    app_workers_model <- compute_worker_models(app_workers_data, start_date)
    if (save_model) {
      saveRDS(app_workers_model, model_file)
    }
  }

  # run output
  generate_model_output(app_workers_model, app_workers_data, app_id)
}
