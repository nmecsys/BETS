
<!-- README.md is generated from README.Rmd. Please edit that file -->
BETS
====

[![Build Status](https://travis-ci.org/nmecsys/BETS.svg?branch=master)](https://travis-ci.org/nmecsys/BETS) 
![Build Status](https://ci.appveyor.com/api/projects/status/github/nmecsys/BETS?branch=master&svg=true) 
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/BETS)](https://CRAN.R-project.org/package=BETS) 
[![downloads](http://cranlogs.r-pkg.org/badges/BETS)](http://cran.rstudio.com/web/packages/BETS/index.html) 
![](http://cranlogs.r-pkg.org/badges/last-week/BETS?color=blue)
[![Github Stars](https://img.shields.io/github/stars/nmecsys/BETS.svg?style=flat&label=Github)](https://github.com/nmecsys/BETS)
![GitHub repo size in bytes](https://img.shields.io/github/repo-size/nmecsys/BETS.svg)
![CRAN](https://img.shields.io/cran/l/BETS.svg)



![Screenshot](https://gist.githubusercontent.com/GreedBlink/e47bce2c1f31ca138885eab5a704a98f/raw/2364ac84350c54fff9acb923877c2f2b88babc08/BETS.png)


:exclamation: **Please read this carefully before using the latest BETS version (0.4.4)**

*The package went through considerable changes.*

BETS - Brazilian Economic Times Series
======================================

Installation
------------

``` r
# cran version
install.packages("BETS") 
# dev version
devtools::install_github("nmecsys/BETS")
```

Usage
-----

``` r
library(BETS)
```

:exclamation: Important (update 0.4.2)
--------------------------------------

1.  BETS package underwent major changes in response to R Journal's reccomendations:
    -   New function names (see table below)
    -   Database onnection credentials are now encrypted
    -   Sample data was included in `/data`, to allow the user to run examples even when offline, or when our server is down.

| Old name          | New name    |
|:------------------|:------------|
| BETS.search       | BETSsearch  |
| BETS.get          | BETSget     |
| BETS.chart        | chart       |
| BETS.save.sas     | saveSas     |
| BETS.save.stata   | saveStata   |
| BETS.save.spss    | saveSpss    |
| BETS.corrgram     | corrgram    |
| BETS.dashboard    | dashboard   |
| BETS.deflate      | deflate     |
| BETS.dummy        | dummy       |
| BETS.grnn.test    | grnn.test   |
| BETS.grnn.train   | grnn.train  |
| BETS.normalize    | normalize   |
| BETS.predict      | predict     |
| BETS.report       | report      |
| BETS.sidra.get    | sidraGet    |
| BETS.sidra.search | sidraSearch |
| BETS.std\_resid   | std\_resid  |
| BETS.t\_test      | t\_test     |
| BETS.ur\_test     | ur\_test    |

1.  Package `forecast`'s newest version (8.3) contains a bug in `ndiffs`. An error arises when trying to run Augmented Dickey-Fuller (ADF) tests. Therefore, BETS' `report` function does not work properly if the user opt for SARIMA analysis with ADF tests. A solution is to install `forecast 8.2`:

``` r
remove.packages("forecast")
install.packages("devtools")
devtools::install_version("forecast", version = "8.2", type = "source")
```

