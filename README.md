
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
#devtools::install_github("roboton/chwork")
devtools::install()
#> 
#>      checking for file ‘/home/ron/src/chwork/DESCRIPTION’ ...  ✓  checking for file ‘/home/ron/src/chwork/DESCRIPTION’
#>   ─  preparing ‘chwork’:
#>      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>        NB: this package now depends on R (>= 3.5.0)
#>        WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'chwork/test_analysis/app_workers_data.rds'  WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'chwork/test_analysis/app_workers_model.rds'
#>   ─  building 'chwork_0.0.0.9000.tar.gz'
#>      
#> Running /usr/lib/R/bin/R CMD INSTALL /tmp/RtmpZYGvnF/chwork_0.0.0.9000.tar.gz \
#>   --install-tests 
#> * installing to library ‘/home/ron/R/x86_64-pc-linux-gnu-library/3.6’
#> * installing *source* package ‘chwork’ ...
#> ** using staged installation
#> ** R
#> ** byte-compile and prepare package for lazy loading
#> ** help
#> *** installing help indices
#> ** building package indices
#> ** testing if installed package can be loaded from temporary location
#> ** testing if installed package can be loaded from final location
#> ** testing if installed package keeps a record of temporary installation path
#> * DONE (chwork)
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
worker_analysis(setNames(test_data, c("worker_id", "date", "count")),
                "test_analysis", start_date = start_date, period = "day")
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
#> [1] "test_analysis"
```
