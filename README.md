
<!-- README.md is generated from README.Rmd. Please edit that file -->

# didimputation

<!-- badges: start -->
<!-- badges: end -->

The goal of didimputation is to estimate TWFE models without running
into the problem of staggered treatment adoption.

## Installation

You can install didimputation from github with:

``` r
devtools::install_github("kylebutts/didimputation")
```

### TWFE vs. DID Imputation Example

I will load example data from the package and plot the average outcome
among the groups. Here is one unit’s data:

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.3     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   2.0.0     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(didimputation)
#> Loading required package: fixest
#> From fixest 0.9.0 onward, BREAKING changes! (Permanently remove this message with fixest_startup_msg(FALSE).) 
#> - In i():
#>     + the first two arguments have been swapped! Now it's i(factor_var, continuous_var) for interactions. 
#>     + argument 'drop' has been removed (put everything in 'ref' now).
#> - In feglm(): 
#>     + the default family becomes 'gaussian' to be in line with glm(). Hence, for Poisson estimations, please use fepois() instead.
library(fixest)

# Load theme
source("https://raw.githubusercontent.com/kylebutts/templates/master/ggplot_theme/theme_kyle.R")
#> Loading required package: showtext
#> Loading required package: sysfonts
#> Loading required package: showtextdb

# Load Data from did2s package
data("df_het", package="did2s")
```

Here is a plot of the average outcome variable for each of the groups:

``` r
# Plot Data 
df_avg <- df_het %>% 
  group_by(group, year) %>% 
  summarize(dep_var = mean(dep_var), .groups = 'drop')

# Get treatment years for plotting
gs <- df_het %>% 
  filter(treat == TRUE) %>% 
  pull(g) %>% unique()
    
    
ggplot() + 
    geom_line(data = df_avg, mapping = aes(y = dep_var, x = year, color = group), size = 1.5) +
    geom_vline(xintercept = gs - 0.5, linetype = "dashed") + 
    theme_kyle(base_size = 16) +
    theme(legend.position = "bottom") +
    labs(y = "Outcome", x = "Year", color = "Treatment Cohort") + 
    scale_y_continuous(expand = expansion(add = .5)) + 
    scale_color_manual(values = c("Group 1" = "#d2382c", "Group 2" = "#497eb3", "Group 3" = "#8e549f")) 
```

<div class="figure">

<img src="man/figures/README-plot-df-het-1.png" alt="Example data with heterogeneous treatment effects" width="100%" />
<p class="caption">
Example data with heterogeneous treatment effects
</p>

</div>

### Estimate DID Imputation

First, lets estimate a static did:

``` r
# Static
static <- did_imputation(data = df_het, yname = "dep_var", gname = "g", tname = "year", idname = "unit")

static
#> # A tibble: 1 × 5
#>   term  estimate std.error conf.low conf.high
#>   <chr>    <dbl>     <dbl>    <dbl>     <dbl>
#> 1 treat     2.26    0.0314     2.20      2.32
```

This is very close to the true treatment effect of 2.2384912.

Then, let’s estimate an event study did:

``` r
# Event Study
es <- did_imputation(data = df_het, yname = "dep_var", gname = "g",
               tname = "year", idname = "unit", 
               # event-study
               horizon=TRUE, pretrends = -5:-1)

es
#> # A tibble: 26 × 5
#>    term  estimate std.error conf.low conf.high
#>    <chr>    <dbl>     <dbl>    <dbl>     <dbl>
#>  1 -5     -0.0641    0.0767  -0.214     0.0861
#>  2 -4     -0.0120    0.0753  -0.160     0.136 
#>  3 -3     -0.0139    0.0765  -0.164     0.136 
#>  4 -2      0.0510    0.0770  -0.0999    0.202 
#>  5 -1      0.0202    0.0758  -0.128     0.169 
#>  6 0       1.51      0.0755   1.37      1.66  
#>  7 1       1.66      0.0841   1.50      1.83  
#>  8 2       1.86      0.0829   1.70      2.03  
#>  9 3       1.92      0.0843   1.75      2.08  
#> 10 4       1.87      0.0842   1.71      2.04  
#> # … with 16 more rows
```

And plot the results:

``` r
pts <- es %>%
    select(rel_year = term, estimate, std.error) %>%
    mutate(
        ci_lower = estimate - 1.96 * std.error,
        ci_upper = estimate + 1.96 * std.error,
        group = "DID Imputation Estimate",
        rel_year = as.numeric(rel_year)
    ) %>%
    filter(rel_year >= -8 & rel_year <= 8) %>% 
    mutate(rel_year = rel_year + 0.1)

te_true <- df_het %>%
    # Keep only treated units
    filter(g > 0) %>%
    group_by(rel_year) %>%
    summarize(estimate = mean(te + te_dynamic)) %>%
      mutate(group = "True Effect") %>%
    filter(rel_year >= -8 & rel_year <= 8) %>% 
    mutate(rel_year = rel_year)

pts <- bind_rows(pts, te_true)

max_y <- max(pts$estimate)

ggplot() +
    # 0 effect
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    # Confidence Intervals
    geom_linerange(data = pts, mapping = aes(x = rel_year, ymin = ci_lower, ymax = ci_upper), color = "grey30") +
    # Estimates
    geom_point(data = pts, mapping = aes(x = rel_year, y = estimate, color = group), size = 2) +
    # Label
    geom_label(data = data.frame(x = -0.5 - 0.1, y = max_y + 0.25, label = "Treatment Starts ▶"), label.size=NA,
               mapping = aes(x = x, y = y, label = label), size = 5.5, hjust = 1, fontface = 2, inherit.aes = FALSE) +
    scale_x_continuous(breaks = -8:8, minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    scale_color_manual(values = c("DID Imputation Estimate" = "steelblue", "True Effect" = "#b44682")) +
    labs(x = "Relative Time", y = "Estimate", color = NULL, title = NULL) +
    theme_kyle(base_size = 16) +
    theme(legend.position = "bottom")
#> Warning: Removed 17 rows containing missing values (geom_segment).
```

<div class="figure">

<img src="man/figures/README-plot-es-1.png" alt="Event-study plot with example data" width="100%" />
<p class="caption">
Event-study plot with example data
</p>

</div>

### Comparison to TWFE

``` r
# TWFE
twfe <- fixest::feols(dep_var ~ i(rel_year, ref=c(-1, Inf)) | unit + year, data = df_het) %>%
    broom::tidy() %>%
    filter(str_detect(term, "rel_year::")) %>%
    select(rel_year = term, estimate, std.error) %>%
    mutate(
        rel_year = as.numeric(str_remove(rel_year, "rel_year::")),
        ci_lower = estimate - 1.96 * std.error,
        ci_upper = estimate + 1.96 * std.error,
        group = "TWFE Estimate"
    ) %>%
    filter(rel_year <= 8 & rel_year >= -8) %>% 
    mutate(rel_year = rel_year - 0.1)

# Add TWFE Points
both_pts <- pts %>% mutate(
        group = if_else(group == "Estimated Effect", "DID Imputation Estimate", group)
    ) %>% 
    bind_rows(., twfe)


ggplot() +
    # 0 effect
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    # Confidence Intervals
    geom_linerange(data = both_pts, mapping = aes(x = rel_year, ymin = ci_lower, ymax = ci_upper), color = "grey30") +
    # Estimates
    geom_point(data = both_pts, mapping = aes(x = rel_year, y = estimate, color = group), size = 2) +
    # Label
    geom_label(data = data.frame(x = -0.5 - 0.1, y = max_y + 0.25, label = "Treatment Starts ▶"), label.size=NA,
               mapping = aes(x = x, y = y, label = label), size = 5.5, hjust = 1, fontface = 2, inherit.aes = FALSE) +
    scale_x_continuous(breaks = -8:8, minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    scale_color_manual(values = c("DID Imputation Estimate" = "steelblue", "True Effect" = "#b44682", "TWFE Estimate" = "#82b446")) +
    labs(x = "Relative Time", y = "Estimate", color = NULL, title = NULL) +
    theme_kyle(base_size = 16) +
    theme(legend.position = "bottom")
#> Warning: Removed 17 rows containing missing values (geom_segment).
```

<div class="figure">

<img src="man/figures/README-plot-compare-1.png" alt="TWFE and Two-Stage estimates of Event-Study" width="100%" />
<p class="caption">
TWFE and Two-Stage estimates of Event-Study
</p>

</div>

# References
