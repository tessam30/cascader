
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cascade

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of cascade is to generate cascade plots of various types.

## Installation

You can install the development version of cascade from
[GitHub](https://github.com/cascade) with:

``` r
# install.packages("devtools")
devtools::install_github("USAID-OHA-SI/cascade")
```

## Example

``` r
library(cascade)
library(glitr)
library(glamr)
library(gophr)
## basic example code

# Set up
genie_path <- file.path(si_path(), "Genie/Genie-PSNUByIMs-Zambia-Daily-2022-05-16.zip")

msd_source <- source_info(genie_path)
curr_pd <- source_info(genie_path, return = "period")
curr_fy <- source_info(genie_path, return = "fiscal_year")
curr_qtr <- source_info(genie_path, return = "quarter")

df <- read_msd(genie_path)

# Plot
create_cascade(df)
# You will then be prompted for which cascade to be returned.
```

\`\`\`

------------------------------------------------------------------------

*Disclaimer: The findings, interpretation, and conclusions expressed
herein are those of the authors and do not necessarily reflect the views
of United States Agency for International Development. All errors remain
our own.*
