hw5
================
sze pui
11/20/2021

``` r
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(purrr)
```

#Problem 1

``` r
#drop the cells with unknown or blank
homicide_df = read_csv("homicide_data.csv", na = c("","Unknown"))
```

    ## Rows: 52179 Columns: 12

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
 homicide_df = homicide_df %>%
   mutate(      #combind city and state together for easy reading 
     city_state = str_c(city, state),
      resolution = case_when(
      disposition == "Closed without arrest" ~ "unsolved",
      disposition == "Open/No arrest" ~ "unsolved",
      disposition == "Closed by arrest" ~ "solved")) %>% 
   relocate(city_state)%>% 
  filter(city_state != "TulsaAL")
```

Lets focus on baltimore first…

``` r
baltimore_df = 
  homicide_df %>% 
  filter(city_state == "BaltimoreMD") 
baltimore_summary = 
baltimore_df %>% 
  summarize(
    unsolved = sum(resolution == "unsolved"), 
    n = n()
  )
baltimore_test = 
prop.test(
  x = baltimore_summary %>% pull(unsolved),
  n = baltimore_summary %>% pull(n)
)
baltimore_test %>% 
  broom::tidy()
```

    ## # A tibble: 1 × 8
    ##   estimate statistic  p.value parameter conf.low conf.high method    alternative
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
    ## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample… two.sided

We’ll iterate across cities. First, we write a function.

``` r
prop_test_function = function(city_df){
city_summary = 
city_df %>% 
  summarize(
    unsolved = sum(resolution == "unsolved"), 
    n = n()
  )
city_test = 
prop.test(
  x = city_summary %>% pull(unsolved),
  n = city_summary %>% pull(n)
)
return(city_test)
}
```

Next, let’s iterate across all cities.

``` r
results_df = 
  homicide_df %>% 
  nest(data = uid:resolution) %>% 
  mutate(test_results = map(data, prop_test_function), 
         tidy_results = map(test_results, broom::tidy)
         ) %>% 
  select(city_state, tidy_results) %>% 
  unnest(tidy_results) %>%
  select(city_state, estimate, starts_with("conf"))
```

We’ll make a plot showing estimates and confidence intervals.

``` r
results_df %>% 
  mutate(city_state = fct_reorder(city_state, estimate)) %>% 
  ggplot(aes(x = city_state, y = estimate)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![](hw5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
