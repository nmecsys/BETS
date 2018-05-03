[![Build Status](https://travis-ci.org/nmecsys/BETS.svg?branch=master)](https://travis-ci.org/nmecsys/BETS) 
![Build Status](https://ci.appveyor.com/api/projects/status/github/nmecsys/BETS?branch=master&svg=true)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BETS)](https://CRAN.R-project.org/package=BETS) 
[![downloads](http://cranlogs.r-pkg.org/badges/BETS)](http://cran.rstudio.com/web/packages/BETS/index.html)
![](http://cranlogs.r-pkg.org/badges/last-week/BETS?color=blue)

# BETS - Brazilian Economic Times Series

## Installation

```R
install.packages("BETS") 
```
## Usage

```R
library(BETS)
```

## :exclamation: Importante (update 0.4.2)

The BETS package underwent a major update, such as:
  - New function names
  - Connection credentials to the database are now encrypted
  - In cases of non-internet connection, a sample of data was added for offline use


|Olde name  | New name   |  
|:---------:|:----------:|
|BETS.search|BETSsearch|
|BETS.get|BETSget|
|BETS.chart|chart|
|BETS.save.sas|saveSas|
|BETS.save.stata|saveStata|
|BETS.save.spss|saveSpss|
|...|...|






### Information
 
 The Brazilian Economic Uncertainty Indicator (IIE-Br) is standardized in a new window
 
   ```R
   # To get the IIEBR and components, run:
   iiebr <- BETS.get(code = "ST_100.0")
   market <- BETS.get(code = "ST_100.1")
   expectations <- BETS.get(code = "ST_100.2")
   media <- BETS.get(code = "ST_100.3")
   ```




