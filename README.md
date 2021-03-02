
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epgminer

<!-- badges: start -->

<!-- badges: end -->

To facilitate the analysis of EPG data, epgminer contains various
functions for working with voltage time series data as well as ANA
annotation files from manual annotation software.

## Installation

To install, use the following code:

``` r
# install.packages("devtools")
devtools::install_github("LylChun/epgminer")
```

## Example

One major utility of epgminer is to find the frequency of voltage data
waveforms using the Fourier Transform.
