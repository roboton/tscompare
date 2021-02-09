
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chwork

<!-- badges: start -->

<!-- badges: end -->

The goal of chwork is to provide an analytics library for Community
Health Worker related data.

## Installation

You can install lhe development version of chwork from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("roboton/chwork")
```

## Example

This is a basic example with simulated data with two worker anomalies
(one over and one under).

``` r
library(chwork)
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
                          function(x) { rnorm(1, 50, 20) })
# worker anomalies
test_data[test_data$worker_id == "worker_1" &
            test_data$date > start_date, "count"] <- 99
test_data[test_data$worker_id == "worker_2" &
            test_data$date > start_date, "count"] <- 1

output_dir <- worker_analysis(
  test_data, "test_analysis", start_date = start_date, period = "day",
  sig_p = 0.05 / num_workers) # bonferroni correction
#> Warning in worker_analysis(test_data, "test_analysis", start_date =
#> start_date, : computing worker models for app_id: test_analysis
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
#> Inf
#> Saving 7 x 5 in image
#> Warning in self$trans$transform(x): NaNs produced
#> Warning: Transformation introduced infinite values in continuous x-axis
#> Warning: Removed 14 rows containing non-finite values (stat_bin).
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
#> Saving 7 x 5 in image
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

![desc\_count\_dist.png](test_analysis/output/desc_count_dist.png)

![desc\_percentile\_month\_pre.png](test_analysis/output/desc_percentile_month_pre.png)

![desc\_percentile\_ts.png](test_analysis/output/desc_percentile_ts.png)

![desc\_timeseries.png](test_analysis/output/desc_timeseries.png)

![desc\_variance\_boxplot.png](test_analysis/output/desc_variance_boxplot.png)

![desc\_variance\_timeseries.png](test_analysis/output/desc_variance_timeseries.png)

![model\_cumulative\_summary.png](test_analysis/output/model_cumulative_summary.png)

![model\_perf\_groups\_timeseries.png](test_analysis/output/model_perf_groups_timeseries.png)

![model\_timeseries\_summary.png](test_analysis/output/model_timeseries_summary.png)

![peers\_1.png](test_analysis/output/peers_1.png)

![peers\_12.png](test_analysis/output/peers_12.png)

![peers\_13.png](test_analysis/output/peers_13.png)

![peers\_14.png](test_analysis/output/peers_14.png)

![peers\_17.png](test_analysis/output/peers_17.png)

![peers\_18.png](test_analysis/output/peers_18.png)

![peers\_2.png](test_analysis/output/peers_2.png)

![peers\_24.png](test_analysis/output/peers_24.png)

![peers\_27.png](test_analysis/output/peers_27.png)

![peers\_30.png](test_analysis/output/peers_30.png)

![peers\_4.png](test_analysis/output/peers_4.png)

![peers\_6.png](test_analysis/output/peers_6.png)

![perf\_groups\_timeseries.png](test_analysis/output/perf_groups_timeseries.png)

![worker\_1.png](test_analysis/output/worker_1.png)

![worker\_12\_peers.png](test_analysis/output/worker_12_peers.png)

![worker\_12.png](test_analysis/output/worker_12.png)

![worker\_13\_peers.png](test_analysis/output/worker_13_peers.png)

![worker\_13.png](test_analysis/output/worker_13.png)

![worker\_14\_peers.png](test_analysis/output/worker_14_peers.png)

![worker\_14.png](test_analysis/output/worker_14.png)

![worker\_17\_peers.png](test_analysis/output/worker_17_peers.png)

![worker\_17.png](test_analysis/output/worker_17.png)

![worker\_18\_peers.png](test_analysis/output/worker_18_peers.png)

![worker\_18.png](test_analysis/output/worker_18.png)

![worker\_2.png](test_analysis/output/worker_2.png)

![worker\_24\_peers.png](test_analysis/output/worker_24_peers.png)

![worker\_24.png](test_analysis/output/worker_24.png)

![worker\_27\_peers.png](test_analysis/output/worker_27_peers.png)

![worker\_27.png](test_analysis/output/worker_27.png)

![worker\_30\_peers.png](test_analysis/output/worker_30_peers.png)

![worker\_30.png](test_analysis/output/worker_30.png)

![worker\_4\_peers.png](test_analysis/output/worker_4_peers.png)

![worker\_4.png](test_analysis/output/worker_4.png)

![worker\_6\_peers.png](test_analysis/output/worker_6_peers.png)

![worker\_6.png](test_analysis/output/worker_6.png)
