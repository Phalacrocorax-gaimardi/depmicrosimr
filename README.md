
<!-- README.md is generated from README.Rmd. Please edit that file -->

# depmicrosimr

<!-- badges: start -->

<!-- badges: end -->

Dynamic electricity pricing is believed to be an important tool to
deliver flexible response by consumers to wholesale electricity prices.
*depmicrosimr* is an agent-based model designed to project the uptake of
dynamic electricity tariffs on Irish consumers and the consequences for
the power system.

## Installation

You can install the development version of depmicrosimr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("phalacrocorax-gaimardi/depmicrosimr")
```

## Example

Simulate load-shifting by a typical household switching from flat
pricing to dynamic pricing. This is based on 2025 wholesale prices in
the dataset *sem_prices_2023_2025* and standard urban load profile LP1
in the dataset *load_profiles*.

``` r
library(depmicrosimr)
#optimise load-shifting behaviour based on 2025 wholesale prices for a household with "natural" load profile LP1
demand <- make_demand_response_data(profile="lp1",mean_daily_load=20,years=2025)
#> Joining with `by = join_by(datetime)`
#assume 50% of load is flexible, behavioural cost parameter is 0.5, loads are shiftable over 24h period.
get_flex(demand,phi=0.5,tau=24,gamma=0.5)
#> [1] "mean load = 0.416628445783263"
#> -----------------------------------------------------------------
#>            OSQP v1.0.0  -  Operator Splitting QP Solver
#>               (c) The OSQP Developer Team
#> -----------------------------------------------------------------
#> problem:  variables n = 8760, constraints m = 8761
#>           nnz(P) + nnz(A) = 1070220
#> settings: algebra = Built-in,
#>           OSQPInt = 4 bytes, OSQPFloat = 8 bytes,
#>           linear system solver = QDLDL v0.1.8,
#>           eps_abs = 1.0e-06, eps_rel = 1.0e-06,
#>           eps_prim_inf = 1.0e-04, eps_dual_inf = 1.0e-04,
#>           rho = 1.00e-01 (adaptive: 50 iterations),
#>           sigma = 1.00e-06, alpha = 1.60, max_iter = 4000
#>           check_termination: on (interval 25, duality gap: on),
#>           time_limit: 1.00e+10 sec,
#>           scaling: on (10 iterations), scaled_termination: off
#>           warm starting: on, polishing: off, 
#> iter  objective    prim res   dual res   gap        rel kkt    rho         time
#>    1  -2.5078e+01   2.38e-01   1.77e-01  -1.23e+00   2.38e-01   1.00e-01    1.10e+00s
#>   50  -2.6175e+01   2.37e-04   1.16e-06  -8.29e-05   2.37e-04   1.17e+00*   1.71e+00s
#>   75  -2.6175e+01   8.18e-09   1.22e-07  -5.70e-07   1.22e-07   1.17e+00    2.25e+00s
#> 
#> status:               solved
#> number of iterations: 75
#> optimal objective:    -26.1748
#> dual objective:       -26.1748
#> duality gap:          -5.6989e-07
#> primal-dual integral: 1.2345e+00
#> run time:             2.26e+00s
#> optimal rho estimate: 2.48e-01
#> # A tibble: 8,760 × 8
#>    datetime              price  load      x load_opt baseload flex_load flex_opt
#>    <dttm>                <dbl> <dbl>  <dbl>    <dbl>    <dbl>     <dbl>    <dbl>
#>  1 2025-01-01 00:00:00 0.00923 0.732 0.293     1.02     0.366     0.366    0.659
#>  2 2025-01-01 01:00:00 0.0112  0.575 0.290     0.865    0.287     0.287    0.577
#>  3 2025-01-01 02:00:00 0.01    0.535 0.288     0.823    0.268     0.268    0.556
#>  4 2025-01-01 03:00:00 0.01    0.530 0.286     0.815    0.265     0.265    0.551
#>  5 2025-01-01 04:00:00 0.01    0.541 0.281     0.822    0.271     0.271    0.551
#>  6 2025-01-01 05:00:00 0.008   0.416 0.267     0.683    0.208     0.208    0.475
#>  7 2025-01-01 06:00:00 0.008   0.458 0.238     0.696    0.229     0.229    0.467
#>  8 2025-01-01 07:00:00 0.0219  0.525 0.182     0.707    0.262     0.262    0.444
#>  9 2025-01-01 08:00:00 0.055   0.595 0.103     0.697    0.297     0.297    0.400
#> 10 2025-01-01 09:00:00 0.091   0.688 0.0207    0.709    0.344     0.344    0.365
#> # ℹ 8,750 more rows
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
