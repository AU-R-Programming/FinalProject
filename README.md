
# Final Project-Group 1

[Package Repository:
FinalProject](https://github.com/AU-R-Programming/FinalProject)

The package is called “lmG1” under the “AU-R-Programming/FinalProject”
repository. The package is designed to perform linear regression where
the parameter vector beta is estimated using optimization. The package
can be download by using
“devtools::install\_github(”AU-R-Programming/FinalProject“)”function.
Learn more about the lmG1 function in the vignette below.

To start off, there are two variables that need to be initialized by the
user:

    `x`: predictor variables, which is represented by a matrix of dimension n × p.

    `y`: response variable of interest, a vector that we want to predict.

We are using the `crop.data.csv` as our example data set.

``` r
library(devtools)
devtools::install_github("AU-R-Programming/FinalProject")

library(lmG1)
crop_data <- read.csv("crop.data.csv", header = TRUE)

set.seed(1)
crop_data <- na.omit(crop_data)
density <- as.factor(crop_data$density)
block <- as.factor(crop_data$block)
fertilizer <- as.factor(crop_data$fertilizer)
X <- cbind(density, block, fertilizer)
y <- crop_data$yield
```

``` r
result <- lmG1(y, X, alpha = 0.05, plot = TRUE)
```

![](README_files/figure-gfm/linear_regression_plots-1.png)<!-- -->

``` r
result$beta
```

    ## [1] 175.79435564   0.53307154  -0.07108954   0.29961801

``` r
result$y_pred
```

    ##           [,1]
    ##  [1,] 176.5560
    ##  [2,] 177.0179
    ##  [3,] 176.4138
    ##  [4,] 176.8758
    ##  [5,] 176.5560
    ##  [6,] 177.0179
    ##  [7,] 176.4138
    ##  [8,] 176.8758
    ##  [9,] 176.5560
    ## [10,] 177.0179
    ## [11,] 176.4138
    ## [12,] 176.8758
    ## [13,] 176.5560
    ## [14,] 177.0179
    ## [15,] 176.4138
    ## [16,] 176.8758
    ## [17,] 176.5560
    ## [18,] 177.0179
    ## [19,] 176.4138
    ## [20,] 176.8758
    ## [21,] 176.5560
    ## [22,] 177.0179
    ## [23,] 176.4138
    ## [24,] 176.8758
    ## [25,] 176.5560
    ## [26,] 177.0179
    ## [27,] 176.4138
    ## [28,] 176.8758
    ## [29,] 176.5560
    ## [30,] 177.0179
    ## [31,] 176.4138
    ## [32,] 176.8758
    ## [33,] 176.8556
    ## [34,] 177.3176
    ## [35,] 176.7134
    ## [36,] 177.1754
    ## [37,] 176.8556
    ## [38,] 177.3176
    ## [39,] 176.7134
    ## [40,] 177.1754
    ## [41,] 176.8556
    ## [42,] 177.3176
    ## [43,] 176.7134
    ## [44,] 177.1754
    ## [45,] 176.8556
    ## [46,] 177.3176
    ## [47,] 176.7134
    ## [48,] 177.1754
    ## [49,] 176.8556
    ## [50,] 177.3176
    ## [51,] 176.7134
    ## [52,] 177.1754
    ## [53,] 176.8556
    ## [54,] 177.3176
    ## [55,] 176.7134
    ## [56,] 177.1754
    ## [57,] 176.8556
    ## [58,] 177.3176
    ## [59,] 176.7134
    ## [60,] 177.1754
    ## [61,] 176.8556
    ## [62,] 177.3176
    ## [63,] 176.7134
    ## [64,] 177.1754
    ## [65,] 177.1552
    ## [66,] 177.6172
    ## [67,] 177.0130
    ## [68,] 177.4750
    ## [69,] 177.1552
    ## [70,] 177.6172
    ## [71,] 177.0130
    ## [72,] 177.4750
    ## [73,] 177.1552
    ## [74,] 177.6172
    ## [75,] 177.0130
    ## [76,] 177.4750
    ## [77,] 177.1552
    ## [78,] 177.6172
    ## [79,] 177.0130
    ## [80,] 177.4750
    ## [81,] 177.1552
    ## [82,] 177.6172
    ## [83,] 177.0130
    ## [84,] 177.4750
    ## [85,] 177.1552
    ## [86,] 177.6172
    ## [87,] 177.0130
    ## [88,] 177.4750
    ## [89,] 177.1552
    ## [90,] 177.6172
    ## [91,] 177.0130
    ## [92,] 177.4750
    ## [93,] 177.1552
    ## [94,] 177.6172
    ## [95,] 177.0130
    ## [96,] 177.4750

``` r
result$sigma2
```

    ##           [,1]
    ## [1,] 0.3326439

``` r
result$variance_beta
```

    ##        ones     density       block  fertilizer 
    ## 0.058905695 0.017325204 0.003465041 0.005197561

``` r
result$confint
```

    ##         ones      density        block   fertilizer         ones      density 
    ## 175.31866267   0.27509081  -0.18646203   0.15831614 176.27004861   0.79105228 
    ##        block   fertilizer 
    ##   0.04428296   0.44091988

``` r
result$r_sqrd
```

    ## [1] 0.2705565

``` r
result$mallow_cp
```

    ##          [,1]
    ## [1,] 33.26439

``` r
result$f_stat
```

    ## [1] 11.37628

``` r
result$pf_value
```

    ## [1] 2.052307e-06
