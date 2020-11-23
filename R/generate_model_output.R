generate_model_output <- function(
  app_workers_model, app_workers_data, app_id) {
  # create output directory
  output_dir <- file.path(app_id, "output")
  dir.create(output_dir, recursive = TRUE)
  # extract start_date from model
  start_date <- max(app_workers_model[[1]]$model$pre.period)

  ## Descriptive stats
  app_summary <- app_workers_data %>%
    mutate(date = as.character(date)) %>%
    bind_rows(., mutate(., date = "all")) %>%
    group_by(date) %>%
    summarise(
      active_workers = n_distinct(worker_id[active]),
      inactive_workers= n_distinct(worker_id[!active]),
      unique_workers = n_distinct(worker_id),
      count = sum(count),
      .groups = "drop")

  app_summary %>%
    write_csv(file.path(output_dir, "app_summary.csv"))

  # always monthly
  date_range <- range(ymd(app_summary$date), na.rm = TRUE)
  date_seq <- seq(date_range[1], date_range[2], by = "month")
  yoy_summary <- app_summary %>%
    filter(date != "all") %>%
    mutate(date = floor_date(ymd(date), "month")) %>%
    pivot_longer(-date) %>%
    group_by(date, name) %>%
    summarise(across(value, sum), .groups = "drop") %>%
    # complete irregular month sequence (if needed)
    right_join(tibble(date = date_seq), by = "date") %>%
    replace_na(list(count = 0)) %>%
    group_by(name) %>% arrange(name, date) %>%
    mutate(yoy_change = value - lag(value, 12)) %>%
    ungroup()

  yoy_summary %>%
    write_csv(file.path(output_dir, "yoy_summary.csv"))

  ### Timeseries plot
  app_summary %>%
    filter(date != "all") %>%
    mutate(date = ymd(date)) %>%
    pivot_longer(-date, names_to = "sub_metric", values_to = "value") %>%
    mutate(metric = if_else(str_detect(sub_metric, "active"), "active_workers",
                            sub_metric)) %>%
    ggplot(aes(date, value, fill = sub_metric, color = sub_metric)) +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    geom_bar(stat = "identity") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_vline(xintercept = start_date, lty = 2, color = "grey")
  ggsave(file.path(output_dir, "desc_timeseries.png"))

  ### YoY plot
  if(yoy_summary %>% filter(!is.na(yoy_change)) %>% nrow() > 0) {
    yoy_summary %>%
      filter(!is.na(yoy_change)) %>%
      ggplot(aes(date, yoy_change)) +
      geom_bar(stat = "identity") +
      ggtitle("YoY change") +
      facet_wrap(~ name, scale = "free_y", ncol = 2)

    ggsave(file.path(output_dir, "desc_yoy.png"))
  }

  ### Overall
  app_summary %>%
    filter(date== "all") %>%
    pivot_longer(-date) %>%
    write_csv(file.path(output_dir, "desc_overall.csv"))

  ## CHW performance model

  model_summary <- tibble(
    worker_id = str_remove(names(app_workers_model), "worker_"),
    worker_perf = lapply(app_workers_model, function(mdl) {
      mdl$summary %>%
        rownames_to_column("type") %>%
        pivot_wider(names_from = type, values_from = -type) %>%
        select(AbsEffect_Average, RelEffect_Average,
               AbsEffect_Cumulative, RelEffect_Cumulative,
               p_Cumulative, p_Average)
    })) %>%
    unnest(worker_perf) %>%
    mutate(performance = case_when(
      RelEffect_Cumulative > 0 & p_Cumulative < sig_p ~ "over",
      RelEffect_Cumulative < 0 & p_Cumulative < sig_p ~ "under",
      TRUE ~ "average")) %>%
    arrange(p_Cumulative)

  model_summary %>%
    write_csv(file.path(output_dir, "model_summary.csv"))

  ### CHW summary stats

  #### Monthly

  tibble(worker_id = names(app_workers_model),
         app_id = app_id,
         series = map(app_workers_model, ~ pluck(.x, "series") %>% as.data.frame() %>%
                        rownames_to_column("month"))) %>%
    unnest(series) %>%
    mutate(point_perf = case_when(point.effect.lower > 0 ~ "over",
                                  point.effect.upper < 0  ~ "under",
                                  TRUE ~ "average")) %>%
    select(month, app_id, point_perf) %>%
    #mutate(month = forcats::fct_rev(month)) %>%
    mutate(month = ymd(month)) %>%
    filter(month >= start_date) %>%
    group_by(month, app_id, point_perf) %>%
    summarise(workers = n(), .groups = "drop") %>%
    arrange(app_id, month) %>%
    #filter(point_perf != "average") %>%
    ggplot(aes(month, workers, fill = point_perf, color = point_perf)) +
    geom_bar(stat = "identity") +
    #geom_line() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle("Monthly performance summary")

  ggsave(file.path(output_dir, "model_monthly_summary.png"))

  #### Cumulative

  model_summary %>%
    group_by(performance) %>%
    summarize(workers = n(), .groups = "drop") %>%
    ggplot(aes(performance, workers,fill = performance)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    ggtitle("Cumulative performance summary")

  ggsave(file.path(output_dir, "model_cumulative_summary.png"))

  ### CHW-specific stats

  model_summary %>%
    select(worker_id, p_Cumulative) %>%
    mutate(series = map(worker_id, ~
                          app_workers_model[[paste0("worker_", .x)]]$series %>%
                          as.data.frame() %>% rownames_to_column("month"))) %>%
    unnest(series) %>%
    mutate(month = ymd(month)) %>%
    write_csv("chw_timeseries.csv")

  top_chw_plots <- 10

  tmp <- map(head(model_summary$worker_id, top_chw_plots), function(.x) {
    plot(app_workers_model[[paste0("worker_", .x)]], "original")
    ggsave(file.path(output_dir, paste0(.x, ".png")))
  })
  return(app_id)
}
