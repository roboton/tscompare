
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
#> Skipping install of 'chwork' from a github remote, the SHA1 (6c1b07a3) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

This is a basic example with simulated data:

``` r
library(chwork)
num_dates <- 90
num_workers <- 30
count_values <- 1:100
test_data <- merge(paste0("worker_", 1:num_workers),
      seq(Sys.Date(), Sys.Date() + (num_dates - 1), by = 1),
      colnames = c("foo", "bar"))
start_date <- Sys.Date() + floor(num_dates / 2)
test_data$count <- sapply(1:(num_dates*num_workers),
                          function(x) { sample(count_values, 1) })
test_data <- setNames(test_data, c("worker_id", "date", "count"))
output_dir <- worker_analysis(
  test_data, "test_analysis", start_date = start_date, period = "day")
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
