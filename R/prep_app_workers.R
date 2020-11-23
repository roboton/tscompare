prep_app_workers <- function(
  app_workers_data, period, min_pre_periods, min_post_periods,
  min_workers, start_date) {

  app_workers_data <- app_workers_data %>% distinct() %>%
    mutate(date = floor_date(date, period)) %>%
    group_by(worker_id, date) %>%
    summarise(across(count, sum), .groups = "drop_last") %>%
    mutate(active = sum(date <= start_date) >= min_pre_periods &
             sum(date >= start_date) >= min_post_periods) %>%
    arrange(active, worker_id, date) %>%
    ungroup()

  num_active_workers <-
    length(unique(app_workers_data$worker_id[app_workers_data$active]))
  if (num_active_workers < min_workers) {
    stop(paste("Does not meet minimum number of workers:",
               num_active_workers, "<", min_workers))
  }
  return(app_workers_data)
}
