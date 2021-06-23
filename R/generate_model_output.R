#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import purrr
#' @importFrom fs path path_dir
#' @importFrom readr write_csv
#' @importFrom graphics plot
#' @importFrom utils head
#' @importFrom rlang .data
#' @importFrom stats median sd
#' @importFrom forcats fct_rev
#' @importFrom bsts SuggestBurn
#' @importFrom zoo index

desc_summary_count <- function(app_workers_data, start_date, output_dir) {
  # count summaries
  app_workers_data %>%
    # drop leading zeroes
    group_by(.data$worker_id) %>%
    arrange(.data$date) %>%
    filter(cumsum(.data$count) != 0) %>%
    # sum stats by date
    group_by(date) %>%
    summarise(across(.data$count, list(mean = mean, total = sum)),
              workers_active = list(unique(.data$worker_id[.data$count > 0])),
              .groups = "drop") %>%
    mutate(workers_total =
             accumulate(.data$workers_active,
                        ~ unique(c(unlist(.x), unlist(.y))))) %>%
    mutate(across(starts_with("workers_"), ~ purrr::map_int(.x, length)),
           workers_active_new = .data$workers_total - lag(.data$workers_total),
           workers_inactive_old = .data$workers_total - .data$workers_active,
           workers_active_old = .data$workers_active -
             .data$workers_active_new) %>%
    pivot_longer(-.data$date) %>%
    write_csv(fs::path(output_dir, "desc_summary_count.csv")) %>%
    filter(str_detect(.data$name, "^workers_(in)?active_") |
             .data$name == "count_total") %>%
    filter(!is.na(.data$value)) %>%
    mutate(type = if_else(str_starts(.data$name, "count_"), .data$name,
                                     str_extract(.data$name, "(^[^_]*)"))) %>%
    ggplot(aes(.data$date, .data$value, fill = .data$name)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ .data$type, scales = "free_y", ncol = 1) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_fill_discrete(breaks = c("workers_active_new", "workers_active_old",
                                   "workers_inactive_old")) +
    geom_vline(xintercept = {{start_date}}, lty = 2, color = "grey")
  ggsave(fs::path(output_dir, "desc_summary_count.png"))
}

desc_summary_stats <- function(app_workers_data, start_date, output_dir) {
  # stat summaries
  app_workers_data %>%
    group_by(.data$worker_id) %>%
    arrange(.data$date) %>%
    filter(cumsum(.data$count) != 0) %>%
    group_by(date) %>%
    mutate(across(.data$count, list(log10 = ~ log10(.x + 1)))) %>%
    summarise(across(c(.data$count, .data$count_log10),
                     list(mean =  mean, sd = sd, median = median))) %>%
    pivot_longer(-.data$date) %>%
    write_csv(fs::path(output_dir, "desc_summary_stats.csv")) %>%
    ggplot(aes(.data$date, .data$value, group = .data$name)) +
    facet_wrap(~ .data$name, scales = "free_y") +
    geom_line() +
    geom_vline(xintercept = {{start_date}}, lty = 2, color = "grey")
  ggsave(fs::path(output_dir, "desc_summary_stats.png"))
}

desc_quantile_timeseries <- function(app_workers_data, start_date, output_dir,
                                     n_quantiles = 3) {
  # pre-quantile timeseries
  desc_quantile_ts <- app_workers_data %>%
    group_by(.data$worker_id) %>%
    arrange(.data$date) %>%
    filter(cumsum(.data$count) != 0) %>%
    group_by(period = if_else(.data$date <= {{start_date}}, "pre", "post"),
             .data$worker_id) %>%
    mutate(med_count = median(.data$count),
           mean_count = mean(.data$count),
           num_months = n()) %>%
    group_by(period) %>%
    mutate(med_quantile = ntile(.data$med_count, {{n_quantiles}}),
           mean_quantile = ntile(.data$mean_count, {{n_quantiles}})) %>%
    group_by(.data$date) %>%
    mutate(month_percentile = percent_rank(.data$count),
           quantile = factor(.data$med_quantile, levels = 1:n_quantiles),
           mean_months = mean(num_months)) %>%
    group_by(.data$date, .data$quantile) %>%
    summarise(mean_percentile = mean(.data$month_percentile),
              .groups = "drop") %>%
    write_csv(fs::path(output_dir, "desc_quantile_timeseries.csv"))

  desc_quantile_ts %>%
    ggplot(aes(.data$date, .data$mean_percentile,
               color = .data$quantile, group = .data$quantile)) +
    geom_line() +
    geom_vline(xintercept = {{start_date}}, alpha = 0.3)
  ggsave(fs::path(output_dir, "desc_quantile_timeseries.png"))
  return(desc_quantile_ts)
}

model_summary <- function(app_workers_model, output_dir) {
  sig_p <- app_workers_model[[1]]$model$alpha
  model_summary <- tibble(
    worker_id = str_remove(names(app_workers_model), "worker_"),
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
    mutate(deviance = case_when(
      .data$AbsEffect_Cumulative > 0 & .data$p_Cumulative < sig_p ~ "over",
      .data$AbsEffect_Cumulative < 0 & .data$p_Cumulative < sig_p ~ "under",
      TRUE ~ "average")) %>%
    arrange(.data$p_Cumulative) %>%
    write_csv(fs::path(output_dir, "model_summary.csv"))

  model_summary %>%
    group_by(.data$deviance) %>%
    summarize(workers = n(), .groups = "drop") %>%
    ggplot(aes(.data$deviance, .data$workers, fill = .data$deviance)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    ggtitle("Cumulative model summary")
  ggsave(fs::path(output_dir, "model_cumulative_summary.png"))

  return(model_summary)
}

model_summary_timeseries <- function(app_workers_model, output_dir) {
  start_date <- max(app_workers_model[[1]]$model$pre.period)
  timeseries_summary <- tibble(worker_id = names(app_workers_model),
         app_id = fs::path_dir(output_dir),
         series = purrr::map(app_workers_model, ~ purrr::pluck(.x, "series") %>%
                               as.data.frame() %>%
                               tibble::rownames_to_column("date"))) %>%
    unnest(.data$series) %>%
    mutate(point_perf = case_when(point.effect.lower > 0 ~ "over",
                                  point.effect.upper < 0  ~ "under",
                                  TRUE ~ "average")) %>%
    select(.data$date, .data$app_id, .data$point_perf) %>%
    mutate(date = lubridate::ymd(.data$date)) %>%
    filter(.data$date >= {{start_date}}) %>%
    group_by(.data$date , .data$app_id, .data$point_perf) %>%
    summarise(workers = n(), .groups = "drop") %>%
    arrange(.data$app_id, .data$date) %>%
    write_csv(fs::path(output_dir, "model_summary_timeseries.csv"))

  timeseries_summary %>%
    ggplot(aes(.data$date, .data$workers, fill = .data$point_perf,
               color = .data$point_perf)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "bottom", legend.title = element_blank())
  ggsave(fs::path(output_dir, "model_timeseries_summary.png"))
  return(timeseries_summary)
}

model_group_timeseries <- function(app_workers_model, output_dir) {
  start_date <- max(app_workers_model[[1]]$model$pre.period)
  model_summary <- model_summary(app_workers_model, output_dir)
  group_timeseries <- model_summary %>%
    select(.data$worker_id, .data$p_Cumulative) %>%
    mutate(series = purrr::map(
      .data$worker_id, ~ app_workers_model[[paste0("worker_", .x)]]$series %>%
        as.data.frame() %>% tibble::rownames_to_column("date"))) %>%
    unnest(.data$series) %>%
    mutate(date = lubridate::ymd(.data$date)) %>%
    write_csv(fs::path(output_dir, "model_group_timeseries.csv"))
  group_timeseries %>%
    select(.data$worker_id, .data$date, count = .data$response) %>%
    left_join(
      model_summary %>%
        select(.data$worker_id, .data$deviance), by = "worker_id") %>%
    group_by(.data$date, .data$deviance) %>%
    summarise(across(.data$count, list(mean = mean, sd = sd))) %>%
    ggplot(aes(.data$date, .data$count_mean, color = .data$deviance)) +
    geom_line() +
    geom_vline(xintercept = {{start_date}}, lty = 2, color = "grey")
    # geom_errorbar(aes(ymin = .data$count_mean - 1.96 * .data$count_sd,
    #                   ymax = .data$count_mean + 1.96 * .data$count_sd),
    #               alpha = 0.3)
  ggsave(fs::path(output_dir, "model_group_timeseries.png"))
  return(group_timeseries)
}

model_worker_peers <- function(app_workers_model, output_dir,
                               peer_plots = FALSE) {
  start_date <- max(app_workers_model[[1]]$model$pre.period)
  model_summary <- model_summary(app_workers_model, output_dir)
  purrr::map_dfr(
    pull(filter(model_summary, .data$deviance != "average"),
         .data$worker_id), function(.x) {
      worker_mdl <- app_workers_model[[paste0("worker_", .x)]]
      # worker plots
      plot(worker_mdl, "original")
      ggsave(fs::path(output_dir, paste0("worker_", .x, ".png")))
      # peer plots
      if (peer_plots) {
        peer_data <- peer_plot(worker_mdl, target_worker_id = .x)
        ggsave(fs::path(output_dir, paste0("peers_", .x, ".png")))
      }
      return(peer_data)
      }) %>% bind_rows() %>%
    write_csv(fs::path(output_dir, "model_worker_peers.csv")) %>%
    return()
}

peer_plot <- function(worker_mdl, target_worker_id, max_peers = 10,
                      inclusion_thresh = 0.01) {
  dates <- worker_mdl$series %>% zoo::index()
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

model_worker_rank <- function(app_workers_model, app_workers_data, output_dir,
                              top_workers = 5) {
  model_summary <- model_summary(app_workers_model, output_dir)
  start_date <- max(app_workers_model[[1]]$model$pre.period)
  worker_rank <- app_workers_data %>%
    select(-.data$active) %>%
    # fill in missing months with zeroes
    pivot_wider(names_from = .data$date, values_from = .data$count,
                values_fill = 0) %>%
    pivot_longer(cols = -.data$worker_id,
                 names_to = "date", values_to = "count") %>%
    mutate(date = lubridate::ymd(date)) %>%
    group_by(.data$worker_id) %>%
    arrange(.data$date) %>%
    # drop leading zeroes (replace with NA)
    mutate(count = if_else(cumsum(.data$count) == 0, NA_real_, .data$count)) %>%
    # drop NA months (leading zeroes)
    ungroup() %>%  filter(!is.na(.data$count)) %>%
    # loess pre-post stat
    group_by(.data$worker_id) %>% arrange(.data$worker_id, .data$date) %>%
    mutate(count_smooth = stats::lowess(.data$count, f = 1/3)$y) %>%
    # agg pre/post stats
    group_by(period = if_else(.data$date < {{start_date}}, "pre", "post"),
             .data$worker_id) %>%
    arrange(.data$period, .data$worker_id, .data$date) %>%
    summarise(count_mean = mean(.data$count),
              count_smooth = unique(if_else(.data$period == "pre",
                                            last(.data$count_smooth),
                                            nth(.data$count_smooth, 2))),
              num_periods = n(),
              data = list(tibble(date, count)),
              .groups = "drop_last") %>%
    # calculate percentile differences in pre/post stats
    mutate(count_mean_pctile = percent_rank(.data$count_mean),
           count_smooth_pctile = percent_rank(.data$count_smooth),
           periods_pctile = percent_rank(.data$num_periods)) %>%
    pivot_wider(names_from = .data$period,
                values_from = c(.data$count_mean, .data$count_smooth,
                                .data$num_periods, .data$count_mean_pctile,
                                .data$count_smooth_pctile, .data$periods_pctile,
                                .data$data)) %>%
    # at least one pre-period value
    filter(.data$num_periods_pre > 0) %>%
    rowwise() %>%
    mutate(count_mean_diff = .data$count_mean_post - .data$count_mean_pre,
           count_smooth_diff = .data$count_smooth_post - .data$count_smooth_pre,
           # num_periods_diff = num_periods_post - num_periods_pre,
           count_mean_pctile_diff = .data$count_mean_pctile_post -
             .data$count_mean_pctile_pre,
           count_smooth_pctile_diff = .data$count_smooth_pctile_post -
             .data$count_smooth_pctile_pre,
           # periods_pctile_diff = periods_pctile_post - periods_pctile_pre,
           # periods_rank_diff = periods_pctile_post - periods_pctile_pre,
           data = list(bind_rows(.data$data_pre, .data$data_post))) %>%
    ungroup() %>%
    mutate(count_mean_pctile_rank = rank(-abs(.data$count_mean_pctile_diff)),
           count_smooth_rank = rank(-abs(.data$count_smooth_diff)),
           count_smooth_pctile_rank = rank(-abs(.data$count_smooth_pctile_diff)),
           count_mean_rank = rank(-abs(.data$count_mean_diff))) %>%
    select(.data$worker_id, .data$count_mean_diff, .data$count_mean_pctile_diff,
           .data$count_mean_pctile_rank, .data$count_mean_rank,
           .data$count_smooth_rank, .data$count_smooth_pctile_rank,
           .data$data) %>%
    arrange(.data$count_mean_pctile_rank + .data$count_mean_rank +
              .data$count_smooth_rank + .data$count_smooth_pctile_rank) %>%
    mutate(joined_rank = paste0(str_sub(.data$worker_id, 1, 5),
                                " mean_pctile: ", .data$count_mean_pctile_rank,
                                " mean: ", .data$count_mean_rank,
                                " smooth: ", .data$count_smooth_rank,
                                " smooth_pctile: ",
                                .data$count_smooth_pctile_rank)) %>%
    unnest(.data$data) %>%
    left_join({{model_summary}} %>%
                mutate(tscompare_rank = rank(.data$p_Cumulative)) %>%
                select(.data$worker_id, .data$tscompare_rank),
              by = "worker_id") %>%
    select(.data$worker_id, .data$date, .data$count, contains("rank"),
           -.data$joined_rank) %>%
    write_csv(fs::path(output_dir, "model_worker_rank.csv"))

  worker_rank %>%
    filter(.data$count_mean_pctile_rank <= {{top_workers}} |
             .data$count_mean_rank <= {{top_workers}} |
             .data$count_smooth_rank <= {{top_workers}} |
             .data$count_smooth_pctile_rank <= {{top_workers}} |
             .data$tscompare_rank <= {{top_workers}}) %>%
    mutate(title = paste(str_sub(.data$worker_id, 1, 5),
                         .data$tscompare_rank, .data$count_mean_rank,
                         .data$count_mean_pctile_rank, .data$count_smooth_rank,
                         .data$count_smooth_pctile_rank)) %>%
    ggplot(aes(.data$date, .data$count, group = .data$title)) + geom_line() +
    facet_wrap(~ .data$title, scales = "free_y", ncol = 3) +
    geom_vline(xintercept = {{start_date}}, lty = 2, color = "grey")
  ggsave(fs::path(output_dir, "model_worker_rank.png"), width = 8,
         height = 2 * top_workers)
}

generate_output <- function(app_workers_model, app_workers_data, app_id, sig_p,
                            top_workers = 5) {
  # setup
  output_dir <- fs::path(app_id, "output")
  fs::dir_create(output_dir)
  start_date <- max(app_workers_model[[1]]$model$pre.period)

  # descriptive outputs
  dsc <- desc_summary_count(app_workers_data, start_date, output_dir)
  dss <- desc_summary_stats(app_workers_data, start_date, output_dir)
  dqt <- desc_quantile_timeseries(app_workers_data, start_date, output_dir)

  # model outputs
  ms <- model_summary(app_workers_model, output_dir)
  mst <- model_summary_timeseries(app_workers_model, output_dir)
  mgt <- model_group_timeseries(app_workers_model, output_dir)

  # model worker outputs
  mwp <- model_worker_peers(app_workers_model, output_dir, top_workers)
  mwr <- model_worker_rank(app_workers_model, app_workers_data, output_dir,
                           top_workers)
  return(app_id)
}
