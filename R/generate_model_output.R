#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @importFrom fs path
#' @importFrom readr write_csv
#' @importFrom graphics plot
#' @importFrom utils head
#' @importFrom rlang .data
generate_model_output <- function(
  app_workers_model, app_workers_data, app_id, sig_p) {
  # create output directory
  output_dir <- path(app_id, "output")
  fs::dir_create(output_dir)
  # extract start_date from model
  start_date <- max(app_workers_model[[1]]$model$pre.period)

  ## Descriptive stats
  app_summary <- app_workers_data %>%
    mutate(date = as.character(date)) %>%
    bind_rows(., mutate(., date = "all")) %>%
    group_by(date) %>%
    summarise(
      active_workers = n_distinct(.data$worker_id[.data$active]),
      inactive_workers= n_distinct(.data$worker_id[!.data$active]),
      unique_workers = n_distinct(.data$worker_id),
      count = sum(count),
      .groups = "drop")

  app_summary %>%
    write_csv(path(output_dir, "app_summary.csv"))

  # always monthly
  date_range <- range(
    lubridate::ymd(app_summary$date[app_summary$date != "all"]), na.rm = TRUE)
  date_seq <- seq(date_range[1], date_range[2], by = "month")
  yoy_summary <- app_summary %>%
    filter(date != "all") %>%
    mutate(date = lubridate::floor_date(lubridate::ymd(date), "month")) %>%
    pivot_longer(-date) %>%
    group_by(date, .data$name) %>%
    summarise(across(.data$value, sum), .groups = "drop") %>%
    # complete irregular month sequence (if needed)
    right_join(tibble(date = date_seq), by = "date") %>%
    replace_na(list(count = 0)) %>%
    group_by(.data$name) %>% arrange(.data$name, date) %>%
    mutate(yoy_change = .data$value - lag(.data$value, 12)) %>%
    ungroup()

  yoy_summary %>%
    write_csv(path(output_dir, "yoy_summary.csv"))

  ### Timeseries plot
  app_summary %>%
    filter(date != "all") %>%
    mutate(date = lubridate::ymd(date)) %>%
    pivot_longer(-date, names_to = "sub_metric", values_to = "value") %>%
    mutate(metric = if_else(stringr::str_detect(.data$sub_metric, "active"),
                            "active_workers", .data$sub_metric)) %>%
    ggplot(aes(date, .data$value, fill = .data$sub_metric,
               color = .data$sub_metric)) +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    geom_bar(stat = "identity") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_vline(xintercept = start_date, lty = 2, color = "grey")
  ggsave(path(output_dir, "desc_timeseries.png"))

  ### YoY plot
  if(yoy_summary %>% filter(!is.na(.data$yoy_change)) %>% nrow() > 0) {
    yoy_summary %>%
      filter(!is.na(.data$yoy_change)) %>%
      ggplot(aes(date, .data$yoy_change)) +
      geom_bar(stat = "identity") +
      ggtitle("YoY change") +
      facet_wrap(~ .data$name, scales = "free_y", ncol = 2)

    ggsave(path(output_dir, "desc_yoy.png"))
  }

  ### Overall
  app_summary %>%
    filter(date== "all") %>%
    pivot_longer(-date) %>%
    write_csv(path(output_dir, "desc_overall.csv"))

  ## CHW performance model

  model_summary <- tibble(
    worker_id = stringr::str_remove(names(app_workers_model), "worker_"),
    worker_perf = lapply(app_workers_model, function(mdl) {
      if (is.null(mdl$summary)) {
        return(NA)
      }
      mdl$summary %>%
        tibble::rownames_to_column("type") %>%
        pivot_wider(names_from = .data$type, values_from = -.data$type) %>%
        select(.data$AbsEffect_Average, .data$RelEffect_Average,
               .data$AbsEffect_Cumulative, .data$RelEffect_Cumulative,
               .data$p_Cumulative, .data$p_Average)
    })) %>%
    unnest(.data$worker_perf) %>%
    mutate(performance = case_when(
      .data$RelEffect_Cumulative > 0 & .data$p_Cumulative < sig_p ~ "over",
      .data$RelEffect_Cumulative < 0 & .data$p_Cumulative < sig_p ~ "under",
      TRUE ~ "average")) %>%
    arrange(.data$p_Cumulative)

  model_summary %>%
    write_csv(path(output_dir, "model_summary.csv"))

  ### CHW summary stats

  #### Monthly

  tibble(worker_id = names(app_workers_model),
         app_id = app_id,
         series = purrr::map(app_workers_model, ~ pluck(.x, "series") %>%
                               as.data.frame() %>%
                               rownames_to_column("month"))) %>%
    unnest(.data$series) %>%
    mutate(point_perf = case_when(point.effect.lower > 0 ~ "over",
                                  point.effect.upper < 0  ~ "under",
                                  TRUE ~ "average")) %>%
    select(.data$month, .data$app_id, .data$point_perf) %>%
    #mutate(month = forcats::fct_rev(month)) %>%
    mutate(month = lubridate::ymd(.data$month)) %>%
    filter(.data$month >= start_date) %>%
    group_by(.data$month, .data$app_id, .data$point_perf) %>%
    summarise(workers = n(), .groups = "drop") %>%
    arrange(.data$app_id, .data$month) %>%
    #filter(point_perf != "average") %>%
    ggplot(aes(.data$month, .data$workers, fill = .data$point_perf,
               color = .data$point_perf)) +
    geom_bar(stat = "identity") +
    #geom_line() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle("Monthly performance summary")

  ggsave(path(output_dir, "model_monthly_summary.png"))

  #### Cumulative

  model_summary %>%
    group_by(.data$performance) %>%
    summarize(workers = n(), .groups = "drop") %>%
    ggplot(aes(.data$performance, .data$workers, fill = .data$performance)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    ggtitle("Cumulative performance summary")

  ggsave(path(output_dir, "model_cumulative_summary.png"))

  ### CHW-specific stats

  model_summary %>%
    select(.data$worker_id, .data$p_Cumulative) %>%
    mutate(series = purrr::map(.data$worker_id, ~
                          app_workers_model[[paste0("worker_", .x)]]$series %>%
                          as.data.frame() %>% rownames_to_column("month"))) %>%
    unnest(.data$series) %>%
    mutate(month = lubridate::ymd(.data$month)) %>%
    write_csv(path(output_dir, "chw_timeseries.csv"))

  top_chw_plots <- 10

  tmp <- purrr::map(head(model_summary$worker_id, top_chw_plots), function(.x) {
    plot(app_workers_model[[paste0("worker_", .x)]], "original")
    ggsave(path(output_dir, paste0(.x, ".png")))
  })
  return(app_id)
}
