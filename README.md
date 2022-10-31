
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modelrecon

The goal of modelrecon is to apply thresholds to predicted probabilities
and calculate net benefit in the presence of resource constraints.

## Installation

You can install the released version of modelrecon from GitHub with:

``` r
remotes::install_github('ML4LHS/modelrecon')
```

## Get started

Let’s load the package and generate and example dataset containing the
probability of an adverse outcome and whether or not that outcome was
experienced (`TRUE`) or not experienced (`FALSE`).

``` r
library(modelrecon)
library(dplyr)
library(tidyr)
library(ggplot2)

example_data = data.frame(probability = c(0.8, 0.7, 0.6, 0.5, 0.3, 0.2, 0.1, 0.1, 0.05, 0.01),
                          outcome = c(T, T, F, T, T, F, F, F, T, F))
```

## What does our example dataset look like?

``` r
example_data
#>    probability outcome
#> 1         0.80    TRUE
#> 2         0.70    TRUE
#> 3         0.60   FALSE
#> 4         0.50    TRUE
#> 5         0.30    TRUE
#> 6         0.20   FALSE
#> 7         0.10   FALSE
#> 8         0.10   FALSE
#> 9         0.05    TRUE
#> 10        0.01   FALSE
```

## Let’s apply a threshold of 0.2

This means we will call all predictions with a probability \>= 0.2 as
`TRUE`.

``` r
example_data %>% 
  apply_threshold(0.2)
#>    probability outcome prediction met_threshold
#> 1         0.80    TRUE       TRUE           0.2
#> 2         0.70    TRUE       TRUE           0.2
#> 3         0.60   FALSE       TRUE           0.2
#> 4         0.50    TRUE       TRUE           0.2
#> 5         0.30    TRUE       TRUE           0.2
#> 6         0.20   FALSE       TRUE           0.2
#> 7         0.10   FALSE      FALSE            NA
#> 8         0.10   FALSE      FALSE            NA
#> 9         0.05    TRUE      FALSE            NA
#> 10        0.01   FALSE      FALSE            NA
```

## Let’s calculate a net benefit with a threshold of 0.2

``` r
example_data %>%
  apply_threshold(0.2) %>%
  calculate_net_benefit()
#> [1] 0.35
```

## What is going on behind the scenes?

Behind the scenes, the `calculate_net_benefit()` function is calculating
the number of true and false positives, and then using that along with
the previously applied threshold to calculate the net benefit.

### How did `calculate_net_benefit()` know about the threshold?

This information is captured in the `thresholds` attribute.

``` r
example_data %>%
  apply_threshold(0.2) %>%
  attributes() %>% 
  .$thresholds
#> [1] 0.2
```

### Want more information about the number of true and false positives?

Set the `verbose` argument of `calculate_net_benefit()` to `TRUE`. This
will print, *not* return, a data frame with the information it used to
calculate the net benefit. The value returned is still the net benefit.

``` r
example_data %>%
  apply_threshold(0.2) %>%
  calculate_net_benefit(verbose = TRUE)
#> # A tibble: 1 x 5
#>   met_threshold     n true_positives false_positives    nb
#>           <dbl> <int>          <int>           <int> <dbl>
#> 1           0.2    10              4               2  0.35
#> [1] 0.35
```

## What happens when you apply an absolute constraint?

Two of the five predicted `TRUE` values are converted to `FALSE` because
only the first 3 `TRUE` values (those with the highest predicted
probability) are able to be acted upon.

``` r
example_data %>%
  apply_threshold(0.2) %>%
  apply_constraint(3)
#>    probability outcome prediction met_threshold
#> 1         0.80    TRUE       TRUE           0.2
#> 2         0.70    TRUE       TRUE           0.2
#> 3         0.60   FALSE       TRUE           0.2
#> 4         0.50    TRUE      FALSE            NA
#> 5         0.30    TRUE      FALSE            NA
#> 6         0.20   FALSE      FALSE            NA
#> 7         0.10   FALSE      FALSE            NA
#> 8         0.10   FALSE      FALSE            NA
#> 9         0.05    TRUE      FALSE            NA
#> 10        0.01   FALSE      FALSE            NA
```

## Calculate a realized net benefit with a threshold of 0.2 and an capacity of 3

This is an example of an *absolute* constraint.

``` r
example_data %>%
  apply_threshold(0.2) %>%
  apply_constraint(3) %>%
  calculate_net_benefit(verbose = TRUE)
#> # A tibble: 1 x 5
#>   met_threshold     n true_positives false_positives    nb
#>           <dbl> <int>          <int>           <int> <dbl>
#> 1           0.2    10              2               1 0.175
#> [1] 0.175
```

## Calculate a realized net benefit with an absolute threshold of 0.2 and capacity of 3, and *then* a relative constraint of 0.5:

``` r
example_data %>%
  apply_threshold(0.2) %>%
  apply_constraint(3) %>%
  apply_threshold(0.5) %>%
  calculate_net_benefit(verbose = TRUE)
#> # A tibble: 2 x 5
#>   met_threshold     n true_positives false_positives    nb
#>           <dbl> <int>          <int>           <int> <dbl>
#> 1           0.2    10              2               1 0.175
#> 2           0.5    10              1               0 0.1
#> [1] 0.275
```

## The default assumption when we set a threshold without a subsequent constraint is that the capacity is infinite.

You can also explicitly note the infinite capacity, which will be
applied only to the immediate prior threshold.

``` r
example_data %>%
  apply_threshold(0.2) %>%
  apply_constraint(3) %>%
  apply_threshold(0.5) %>%
  apply_constraint(Inf) %>% 
  calculate_net_benefit()
#> [1] 0.275
```

Using this mechanism, you can construct multiple layers of absolute and
relative constraints as the piped functions retain metadata about prior
constraints and thus know that each constraint applies to only the prior
threshold.

You *cannot* apply a threshold that is *lower* than a prior threshold
because it would make no sense to apply a permissive criterion *before*
a more restrictive one.

## Setting a new threshold that is lower than the prior one will generate an error.

``` r
example_data %>%
  apply_threshold(0.5) %>%
  apply_constraint(3) %>%
  apply_threshold(0.2) %>%
  calculate_net_benefit()
#> Error in apply_threshold(., 0.2): New threshold must be greater than previous maximum threshold of 0.5
```

## Setting a new threshold that is the same as a prior one will generate a warning.

In a future version, this may be upgraded to an error.

``` r
example_data %>%
  apply_threshold(0.2) %>%
  apply_constraint(3) %>%
  apply_threshold(0.2) %>%
  calculate_net_benefit()
#> Warning in apply_threshold(., 0.2): New threshold must be greater than previous maximum threshold of 0.2. Because the current threshold
#> is equal set threshold, previously added constraints at this threshold have been overwritten.
#> [1] 0.35
```

# Let’s plot a decision curve for an absolute constraint, and an absolute + relative constraint

``` r
plot_data =
  expand_grid(constraint = c(0, 1, 3, 5, 7, Inf),
              threshold = seq(from = 0, to = 1, by = 0.05)) %>%
  group_by(constraint, threshold) %>%
  mutate(net_benefit = example_data %>%
           apply_threshold(threshold) %>%
           apply_constraint(constraint) %>%
           calculate_net_benefit()) %>%
  ungroup()

# Vary absolute constraint and add relative constraint (up to threshold of 0.5)

plot_data_2 =
  expand_grid(constraint = c(0, 1, 3, 5, 7, Inf),
              threshold = seq(from = 0, to = 0.5, by = 0.05)) %>%
  group_by(constraint, threshold) %>%
  mutate(net_benefit = example_data %>%
           apply_threshold(threshold) %>%
           apply_constraint(constraint) %>%
           apply_threshold(pmax(0.5, threshold)) %>%
           calculate_net_benefit()) %>%
  ungroup()

bind_rows(
  plot_data %>% mutate(constraint_type = 'Absolute constraint'),
  plot_data_2 %>% mutate(constraint_type = paste0('Absolute constraint\n',
                                                  'relaxed by relative\n',
                                                  'constraint at threshold\n',
                                                  'of 0.5'))
) %>%
  mutate(constraint = if_else(constraint == Inf, 'Infinity', as.character(constraint))) %>%
  mutate(constraint = as.factor(paste('Capacity =',constraint))) %>%
  filter((constraint == 'Capacity = 3' & threshold == 0.2) |
           (constraint == 'Capacity = Infinity' & threshold == 0.2)) %>% 
  slice(1:3) %>% 
  mutate(text = c('Case study 2', 'Case study 1', 'Case study 3')) %>% 
  mutate(x = c(0.3, 0.4, 0.3), y = c(0.05, 0.4, 0.33)) -> 
  point_data

bind_rows(
  plot_data %>% mutate(constraint_type = 'Absolute constraint'),
  plot_data_2 %>% mutate(constraint_type = paste0('Absolute constraint\n',
                                                  'relaxed by relative\n',
                                                  'constraint at threshold\n',
                                                  'of 0.5'))
  ) %>%
  mutate(constraint = if_else(constraint == Inf, 'Infinity', as.character(constraint))) %>%
  mutate(constraint = as.factor(paste('Capacity =',constraint))) %>%
  ggplot(aes(x = threshold, y = net_benefit,
             linetype = constraint_type)) +
  geom_line() +
  geom_point(data = point_data) +
  geom_text(data = point_data,
            aes(label = text, x = x, y = y), size = 3) +
  facet_wrap(~constraint) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 6)) +
  labs(x = 'Threshold probability',
       y = 'Realized net benefit',
       linetype = 'Constraint')
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

``` r
# ggsave('Figure 2.pdf',
#        width = 6.5, height = 4, units = 'in')
```
