linear models
================
Shihui Peng
2023-11-24

## load and clean the Airbnb data

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  select(price, stars, borough = neighbourhood_group, neighbourhood, room_type) |> 
    filter(borough != "Staten Island")
```

# let’s fit a model

## fit a model

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)

# or we can use this:
fit =
  nyc_airbnb |> 
  lm(price ~ stars + borough, data = _)

# let's look at the 'fit'
fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

- no need to create dummy variables by ourselves, r will recognize the
  categorical variables and create for us.
- The `lm` function begins with the formula specification – outcome on
  the left of the ~ and predictors separated by + on the right.
  - interactions between variables can be specified using `*`.
  - You can also specify an intercept-only model (`outcome ~ 1`)
  - a model with no intercept (`outcome ~ 0 + ...`) (default is with
    intercept)
  - a model using all available predictors (`outcome ~ .`).

## we can use these common functions, but usually we don’t

``` r
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

``` r
summary(fit)$coef
```

    ##                   Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)      -70.41446  14.020697 -5.022180 5.137589e-07
    ## stars             31.98989   2.527500 12.656733 1.269392e-36
    ## boroughBrooklyn   40.50030   8.558724  4.732049 2.232595e-06
    ## boroughManhattan  90.25393   8.567490 10.534465 6.638618e-26
    ## boroughQueens     13.20617   9.064879  1.456850 1.451682e-01

``` r
coef(fit)
```

    ##      (Intercept)            stars  boroughBrooklyn boroughManhattan 
    ##        -70.41446         31.98989         40.50030         90.25393 
    ##    boroughQueens 
    ##         13.20617

``` r
# fitted.values(fit) --> pull out the fitted values
```

- The reason that we omit the output
  - it’s a huge pain to deal with. `summary` produces an object of class
    `summary.lm`, which is also a list – that’s how we extracted the
    coefficients using `summary(fit)$coef`. `coef` produces a vector of
    coefficient values, and `fitted.values` is a vector of fitted
    values. None of this is tidy.

## instead, we want tidy up the output (`broom::glance()`) and tidy up the coefficients (`broom::tidy()`):

``` r
fit |> 
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
fit |> 
  broom::tidy()
```

    ## # A tibble: 5 × 5
    ##   term             estimate std.error statistic  p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)         -70.4     14.0      -5.02 5.14e- 7
    ## 2 stars                32.0      2.53     12.7  1.27e-36
    ## 3 boroughBrooklyn      40.5      8.56      4.73 2.23e- 6
    ## 4 boroughManhattan     90.3      8.57     10.5  6.64e-26
    ## 5 boroughQueens        13.2      9.06      1.46 1.45e- 1

``` r
# broom::tidy() outputs a table but this outputs a matrix (found based on...)
summary(fit)$coef |> class()
```

    ## [1] "matrix" "array"

- `broom::tidy()` outputs a table vs `summary(fit)$coef` outputs a
  matrix. so broom::tidy() is better b/c a table is easier to be used.

### why we say a table is better… e.g.:

``` r
fit |> 
  broom::tidy() |> 
  mutate(
    term = str_replace(term, "^borough", "Borough: ")
  ) |> 
  select(term, estimate, p.value) |> 
  knitr::kable(digits = 3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Borough: Brooklyn  |   40.500 |   0.000 |
| Borough: Manhattan |   90.254 |   0.000 |
| Borough: Queens    |   13.206 |   0.145 |

- a good thing is everything after b::t is familiar tidyverse codes.
- As an aside, broom::tidy works with lots of things, including most of
  the functions for model fitting you’re likely to run into (survival,
  mixed models, additive models, …).

### when r creating indicator variables for categorical vars:

``` r
fit_1 = 
  nyc_airbnb |> 
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)
  ) |> 
  lm(price ~ stars + borough, data = _)

fit_1 |> 
  broom::tidy()
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

- it takes whatever cat var I has started with (e.g. stars before
  borough here)
- it assumes the same factor order as ggplot does (which is,
  alphabetical order). but we can mutate it if we need. as below:

### let’s fit another model

``` r
fit = lm(price ~ stars + borough + room_type, data = nyc_airbnb)

fit |> 
  broom::tidy()
```

    ## # A tibble: 7 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              49.5      13.6      3.64  2.76e- 4
    ## 2 stars                    21.9       2.43     9.01  2.09e-19
    ## 3 boroughBrooklyn          22.7       8.20     2.77  5.60e- 3
    ## 4 boroughManhattan         63.0       8.22     7.67  1.76e-14
    ## 5 boroughQueens             7.58      8.68     0.873 3.82e- 1
    ## 6 room_typePrivate room  -105.        2.05   -51.2   0       
    ## 7 room_typeShared room   -129.        6.15   -21.0   2.24e-97