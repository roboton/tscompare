
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tscompare

<!-- badges: start -->
<!-- badges: end -->

The goal of tscompare is to provide an analytics library for Community
Health Worker related data.

## Installation

You can install lhe development version of tscompare from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("roboton/tscompare")
```

## Example

This is a basic example with simulated data with two worker anomalies
(one over and one under).

``` r
library(tscompare)
set.seed(143)
num_dates <- 90
num_workers <- 30
# generate synthetic data
test_data <- setNames(
  merge(as.character(1:num_workers),
        seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
        colnames = c("worker_id", "date")), c("worker_id", "date"))
start_date <- Sys.Date() + floor(num_dates / 2)
test_data$count <- sapply(1:(num_dates*num_workers),
                          function(x) { rpois(1, 50) })
# worker anomalies
test_data[test_data$worker_id == "1" &
            test_data$date > start_date, "count"] <- 100
test_data[test_data$worker_id == "2" &
            test_data$date > start_date, "count"] <- 1

output_dir <- worker_analysis(
  test_data, "test_analysis", start_date = start_date, period = "day",
  sig_p = 0.01)
#> Warning in worker_analysis(test_data, "test_analysis", start_date =
#> start_date, : computing worker models for app_id: test_analysis
#> test_analysis/app_workers_model.rds
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
#> "none")` instead.
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
#> "none")` instead.
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
paste("worker analysis output in directory:", output_dir)
#> [1] "worker analysis output in directory: test_analysis"
```

``` r
output_pngs <- list.files("test_analysis/output", pattern = ".png$")

for(png in output_pngs){
  cat("\n") 
  cat("![", png, "](test_analysis/output/", png, ")", sep = "")
  cat("\n")
}
```

![desc\_quantile\_timeseries.png](test_analysis/output/desc_quantile_timeseries.png)

![desc\_summary\_count.png](test_analysis/output/desc_summary_count.png)

![desc\_summary\_stats.png](test_analysis/output/desc_summary_stats.png)

![model\_cumulative\_summary.png](test_analysis/output/model_cumulative_summary.png)

![model\_group\_timeseries.png](test_analysis/output/model_group_timeseries.png)

![model\_timeseries\_summary.png](test_analysis/output/model_timeseries_summary.png)

![model\_worker\_rank.png](test_analysis/output/model_worker_rank.png)

![peers\_1.png](test_analysis/output/peers_1.png)

![peers\_2.png](test_analysis/output/peers_2.png)

![peers\_24.png](test_analysis/output/peers_24.png)

![peers\_26.png](test_analysis/output/peers_26.png)

![peers\_29.png](test_analysis/output/peers_29.png)

![worker\_1.png](test_analysis/output/worker_1.png)

![worker\_2.png](test_analysis/output/worker_2.png)

![worker\_24.png](test_analysis/output/worker_24.png)

![worker\_26.png](test_analysis/output/worker_26.png)

![worker\_29.png](test_analysis/output/worker_29.png)
