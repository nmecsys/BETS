[![Build Status](https://travis-ci.org/nmecsys/BETS.svg?branch=master)](https://travis-ci.org/nmecsys/BETS) 
![Build Status](https://ci.appveyor.com/api/projects/status/github/nmecsys/BETS?branch=master&svg=true)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BETS)](https://CRAN.R-project.org/package=BETS) 
[![downloads](http://cranlogs.r-pkg.org/badges/BETS)](http://cran.rstudio.com/web/packages/BETS/index.html)
![](http://cranlogs.r-pkg.org/badges/last-week/BETS?color=blue)

:exclamation: **Please read this carefully before using the latest BETS version (0.4.2).**

*The package went through considerable changes.*

# BETS - Brazilian Economic Times Series

## Installation

```R
install.packages("BETS") 
```
## Usage

```R
library(BETS)
```

## :exclamation: Important (update 0.4.2)

1. BETS package underwent major changes in response to R Journal's reccomendations:
    - New function names (see table below)
    - Database onnection credentials are now encrypted
    - Sample data was included in `/data`, to allow the user to run examples even when offline, or when our server is down.

  |Old name  | New name   |  
  |:---------|:----------|
  |BETS.search|BETSsearch|
  |BETS.get|BETSget|
  |BETS.chart|chart|
  |BETS.save.sas|saveSas|
  |BETS.save.stata|saveStata|
  |BETS.save.spss|saveSpss|
  |BETS.corrgram|corrgram|
  |BETS.dashboard|dashboard|
  |BETS.deflate|deflate|
  |BETS.dummy|dummy|
  |BETS.grnn.test|grnn.test|
  |BETS.grnn.train|grnn.train|
  |BETS.normalize|normalize|
  |BETS.predict|predict|
  |BETS.report|report|
  |BETS.sidra.get|sidra.get|
  |BETS.sidra.search|sidra.search|
  |BETS.std_resid|std_resid|
  |BETS.t_test|t_test|
  |BETS.ur_test|ur_test|
    
2. Package `forecast`'s newest version (8.3) contains a bug in `ndiffs`. An error arises when trying to run augmented Dickey-Fuller testes. Therefore, BETS' `report` function does not work properly if the user opt for SARIMA analysis with ADF tests. A solution is to install `forecast 8.2`:

```R
remove.packages("forecast")
install.packages("devtools")
devtools::install_version("forecast", version = "8.2", type = "source")
```

### Information
 
 The Brazilian Economic Uncertainty Indicator (IIE-Br) is standardized in a new window
 
   ```R
   # To get the IIEBR and components, run:
   iiebr <- BETS.get(code = "ST_100.0")
   market <- BETS.get(code = "ST_100.1")
   expectations <- BETS.get(code = "ST_100.2")
   media <- BETS.get(code = "ST_100.3")
   ```




