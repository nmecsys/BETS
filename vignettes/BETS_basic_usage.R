## ----echo = F,message = F,warning=FALSE----------------------------------
require(BETS)

## ----eval = F------------------------------------------------------------
#  BETSsearch(description, src, periodicity, unit, code, view = TRUE, lang = "en")

## ----echo = F------------------------------------------------------------
library(BETS)

## ----eval = F------------------------------------------------------------
#  # Some examples
#  BETSsearch(description = "sales ~ retail",view = F)
#  BETSsearch(description = "'sales volume index' ~ vehicles",view = F)
#  BETSsearch(description = "'distrito federal'", periodicity = 'A', src = 'IBGE',view = F)

## ----eval = F------------------------------------------------------------
#  # Search for accumulated GDP series
#  BETSsearch(description = "gdp accumulated", unit = "US$", view = F)

## ----echo = F, results='hide'--------------------------------------------
#results <- BETSsearch(description = "gdp accumulated", unit = "US$", view = F)

## ----echo = F------------------------------------------------------------
#results

## ----echo = F, results='hide'--------------------------------------------
#results <- BETSsearch(description = "consumption ~ 'seasonally adjusted' ~ private", view = F)

## ----echo = F------------------------------------------------------------
#head(results)

## ----eval = F------------------------------------------------------------
#  BETSget(code, data.frame = FALSE)

## ----eval = F------------------------------------------------------------
#  # Get the 12-month cumulative GDP series in dollars
#  gdp_accum <- BETSget(4192)
#  window(gdp_accum, start = c(2014,1))

## ----eval = F------------------------------------------------------------
#  #Get the series for the GDP of the Federal District at market prices
#  gdp_df <- BETSget(23992, data.frame = T)
#  head(gdp_df)

## ----eval = F------------------------------------------------------------
#  saveSas(code, data = NULL, file.name = "series")
#  saveSpss(code, data = NULL, file.name = "series")
#  saveStata(code, data = NULL, file.name = "series")

## ----eval = F------------------------------------------------------------
#  # Save the series for the net public debt in the default Excel format
#  saveStata(code = 2078, file.name = "series_stata.dta")
#  
#  # Save the series for the net public debt in the default Excel format
#  saveStata(code = 2078, file.name = "series_stata.dta")
#  
#  # Save any series in SPSS format
#  my.series <- BETSget(4447)
#  saveSpss(data = my.series, file.name = "series_spss")

## ----eval = F------------------------------------------------------------
#  chart(ts, file = NULL, open = TRUE, lang = "en", params = NULL)

## ----eval = F------------------------------------------------------------
#  # Uncertainty Index chart
#  chart(ts = 'iie_br', file = "iie_br", open = TRUE)
#  
#  # Leading and Coincident Labor Indicators charts
#  chart(ts = "lab_mrkt", file = "lab_mrkt.png", open = TRUE)

## ----eval = F------------------------------------------------------------
#  dashboard(type = "business_cycle", charts = "all", saveas = NA, parameters = NULL)

## ----eval = F------------------------------------------------------------
#  dashboard(type = "business_cycle", saveas = "survey.pdf")

## ----eval = F------------------------------------------------------------
#  parameters = list(author = "FGV/IBRE",
#                    url = "http://portalibre.fgv.br/",
#                    text = "text.txt",
#                    logo = "logo_ibre.png")
#  
#  dashboard(type = "macro_situation", parameters = parameters)

