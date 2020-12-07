
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
#> Skipping install of 'chwork' from a github remote, the SHA1 (7d809d05) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

This is a basic example with simulated data with two worker anomalies
(one over and one under).

``` r
library(chwork)
set.seed(143)
num_dates <- 90
num_workers <- 30
test_data <- setNames(
  merge(paste0("worker_", 1:num_workers),
        seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
        colnames = c("worker_id", "date")), c("worker_id", "date"))
start_date <- Sys.Date() + floor(num_dates / 2)
test_data$count <- sapply(1:(num_dates*num_workers),
                          function(x) { rnorm(1, 50, 20) })
test_data[test_data$worker_id == "worker_1" &
            test_data$date > start_date, "count"] <- 99
test_data[test_data$worker_id == "worker_2" &
            test_data$date > start_date, "count"] <- 1
output_dir <- worker_analysis(
  test_data, "test_analysis", start_date = start_date, period = "day",
  sig_p = 0.05 / num_workers) # bonferroni correction
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
worker_pngs <- list.files("test_analysis/output", pattern = ".png$")

for(png in worker_pngs){
  cat("\n") 
  cat("![", png, "](test_analysis/output/", png, ")", sep = "")
  cat("\n")
}
```

![desc\_timeseries.png](test_analysis/output/desc_timeseries.png)

![model\_cumulative\_summary.png](test_analysis/output/model_cumulative_summary.png)

![model\_timeseries\_summary.png](test_analysis/output/model_timeseries_summary.png)

![worker\_1.png](test_analysis/output/worker_1.png)

![worker\_12.png](test_analysis/output/worker_12.png)

![worker\_14.png](test_analysis/output/worker_14.png)

![worker\_17.png](test_analysis/output/worker_17.png)

![worker\_18.png](test_analysis/output/worker_18.png)

![worker\_2.png](test_analysis/output/worker_2.png)

![worker\_24.png](test_analysis/output/worker_24.png)

![worker\_30.png](test_analysis/output/worker_30.png)

![worker\_4.png](test_analysis/output/worker_4.png)

![worker\_6.png](test_analysis/output/worker_6.png)
