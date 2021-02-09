#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @importFrom fs path
#' @importFrom readr write_csv
#' @importFrom graphics plot
#' @importFrom utils head
#' @importFrom rlang .data
#' @importFrom stats median sd
#' @importFrom forcats fct_rev
#' @importFrom bsts SuggestBurn
generate_model_output <- function(
  app_workers_model, app_workers_data, app_id, sig_p, period = "day") {
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

  # always yoy monthly (independent of period argument)
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
    write_csv(path(output_dir, "app_yoy_summary.csv"))

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

  ### Worker count variance over time
  app_workers_data %>%
    group_by(date) %>%
    ggplot(aes(date, count, group = date)) +
    geom_boxplot()
  ggsave(path(output_dir, "desc_variance_boxplot.png"))

  app_workers_data %>%
    group_by(date) %>%
    summarise(across(count, list(mean = mean, sd = sd, med = median)),
              .groups = "drop") %>%
    mutate(count_lb = .data$count_mean - 2 * .data$count_sd,
           count_ub = .data$count_mean + 2 * .data$count_sd) %>%
    pivot_longer(c(.data$count_med, .data$count_mean),
                 names_to = "stat", values_to = "value") %>%
    pivot_longer(c(.data$value, .data$count_sd)) %>%
    mutate(across(c(.data$count_lb, .data$count_ub),
                  ~ if_else(name == "count_sd", NA_real_, .x))) %>%
    ggplot() +
    geom_line(aes(.data$date, .data$value, color = stat)) +
    geom_ribbon(aes(.data$date, .data$value,
                    ymin = .data$count_lb, ymax = .data$count_ub), alpha = 0.1,
                color = "grey") +
    geom_vline(xintercept = start_date, alpha = 0.3) +
    facet_wrap(~ .data$name, scales = "free_y", ncol = 1)
  ggsave(path(output_dir, "desc_variance_timeseries.png"))

  ## Distribution of monthly count
  app_workers_data %>%
    ggplot(aes(count)) +
    geom_histogram(bins = 50) +
    scale_x_log10()
  ggsave(path(output_dir, "desc_count_dist.png"))

  app_workers_data %>%
    group_by(period = if_else(.data$date <= {{start_date}}, "pre", "post"),
             .data$worker_id) %>%
    mutate(med_count = median(.data$count),
           mean_count = mean(.data$count)) %>%
    group_by(period) %>%
    mutate(med_tertile = ntile(.data$med_count, 3),
           mean_tertile = ntile(.data$mean_count, 3)) %>%
    group_by(.data$date) %>%
    mutate(month_percentile = percent_rank(.data$count),
           tertile = factor(.data$med_tertile, levels = c(1, 2, 3))) %>%
    group_by(.data$date, .data$tertile) %>%
    summarise(mean_percentile = mean(.data$month_percentile),
              .groups = "drop") %>%
    write_csv(path(output_dir, "desc_percentile_ts.csv")) %>%
    ggplot(aes(.data$date, .data$mean_percentile,
               color = .data$tertile, group = .data$tertile)) +
    geom_line() +
    geom_vline(xintercept = {{start_date}}, alpha = 0.3)
  ggsave(path(output_dir, "desc_percentile_ts.png"))


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

  #### Timeseries summary

  timeseries_summary <- tibble(worker_id = names(app_workers_model),
         app_id = app_id,
         series = purrr::map(app_workers_model, ~ purrr::pluck(.x, "series") %>%
                               as.data.frame() %>%
                               tibble::rownames_to_column("date"))) %>%
    unnest(.data$series) %>%
    mutate(point_perf = case_when(point.effect.lower > 0 ~ "over",
                                  point.effect.upper < 0  ~ "under",
                                  TRUE ~ "average")) %>%
    select(.data$date, .data$app_id, .data$point_perf) %>%
    mutate(date = lubridate::ymd(.data$date)) %>%
    filter(.data$date >= start_date) %>%
    group_by(.data$date , .data$app_id, .data$point_perf) %>%
    summarise(workers = n(), .groups = "drop") %>%
    arrange(.data$app_id, .data$date)

  timeseries_summary %>%
    write_csv(path(output_dir, "model_summary_timeseries.csv"))

  timeseries_summary %>%
    #filter(point_perf != "average") %>%
    ggplot(aes(.data$date, .data$workers, fill = .data$point_perf,
               color = .data$point_perf)) +
    geom_bar(stat = "identity") +
    #geom_line() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle("Performance summary over time")

  ggsave(path(output_dir, "model_timeseries_summary.png"))

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

  chw_timeseries <- model_summary %>%
    select(.data$worker_id, .data$p_Cumulative) %>%
    mutate(series = purrr::map(
      .data$worker_id, ~ app_workers_model[[paste0("worker_", .x)]]$series %>%
        as.data.frame() %>% tibble::rownames_to_column("date"))) %>%
    unnest(.data$series) %>%
    mutate(date = lubridate::ymd(.data$date))
  write_csv(chw_timeseries, path(output_dir, "model_worker_ts.csv"))

  top_chw_plots <- 10

  peer_data <- purrr::map_dfr(
    head(model_summary$worker_id, top_chw_plots), function(.x) {
      worker_mdl <- app_workers_model[[paste0("worker_", .x)]]
      plot(worker_mdl, "original")
      ggsave(path(output_dir, paste0("worker_", .x, ".png")))
      peer_data <- peer_plot(worker_mdl, target_worker_id = .x, period = period)
      ggsave(path(output_dir, paste0("worker_", .x, "_peers.png")))
      return(peer_data)
  }) %>% bind_rows()
  write_csv(peer_data, path(output_dir, "model_worker_peers.csv"))

  ### Performance groups over time

  chw_timeseries %>%
    select(.data$worker_id, .data$date, count = .data$response) %>%
    left_join(
      model_summary %>%
        select(.data$worker_id, .data$performance), by = "worker_id") %>%
    group_by(.data$date, .data$performance) %>%
    summarise(across(.data$count, list(mean = mean, sd = sd))) %>%
    ggplot(aes(.data$date, .data$count_mean, color = .data$performance)) +
    geom_line() + geom_point()
    geom_errorbar(aes(ymin = .data$count_mean - 1.96 * .data$count_sd,
                      ymax = .data$count_mean + 1.96 * .data$count_sd),
                  alpha = 0.3)
  ggsave(path(output_dir, "model_perf_groups_timeseries.png"))

  return(app_id)
}

peer_plot <- function(worker_mdl, target_worker_id, max_peers = 10,
                      inclusion_thresh = 0.01, period = "day") {
  dates <- seq.Date(from = min(worker_mdl$model$pre.period),
                    to = max(worker_mdl$model$post.period),
                    by = period)
  peer_data <- worker_mdl$model$bsts.model$predictors %>% as_tibble() %>%
    mutate(date = dates) %>%
    bind_cols(tibble(
      target = scale(as.numeric(worker_mdl$series$response)))) %>%
    select(-.data$`(Intercept)`) %>%
    pivot_longer(-date, names_to = "worker_id", values_to = "scaled_count") %>%
    left_join(
      worker_mdl$model$bsts.model$coefficients %>% as_tibble() %>%
        slice(n = bsts::SuggestBurn(0.1, worker_mdl$model$bsts.model):n()) %>%
        pivot_longer(everything(), names_to = "worker_id",
                     values_to = "coef") %>%
        group_by(.data$worker_id) %>%
        summarise(inclusion_prob = mean(.data$coef != 0),
                  coef = mean(.data$coef[.data$coef != 0]),
                  positive_prob = mean(.data$coef > 0),
                  sign = if_else(.data$positive_prob > 0.5, 1, -1),
                  .groups = "drop") %>%
        arrange(-.data$inclusion_prob, -.data$positive_prob),
      by = "worker_id") %>%
    mutate(scaled_count = if_else(!is.na(.data$coef),
                                  .data$scaled_count * .data$coef,
                                  .data$scaled_count),
           inclusion_prob = if_else(.data$worker_id == "target", 1,
                                    .data$inclusion_prob),
           inclusion_rank = dense_rank(-.data$inclusion_prob)) %>%
    mutate(target_worker_id = {{target_worker_id}})

  peer_data %>%
    filter(.data$inclusion_rank %in% 1:max_peers &
             .data$inclusion_prob > inclusion_thresh) %>%
    mutate(type = forcats::fct_rev(if_else(.data$worker_id == "target",
                                           "target", "peers")),
           alpha = scale(if_else(.data$worker_id == "target", 1,
                                 .data$inclusion_prob))) %>%
    ggplot(aes(.data$date, .data$scaled_count, group = .data$worker_id,
               color = .data$type, alpha = .data$alpha, lty = .data$type)) +
    geom_line() +
    geom_vline(xintercept = max(worker_mdl$model$pre.period), lty = 2,
               color = "grey") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(alpha = FALSE)

  return(peer_data)
}
