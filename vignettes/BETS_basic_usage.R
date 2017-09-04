## ----eval = F------------------------------------------------------------
#  BETS.search(description, src, periodicity, unit, code, view = TRUE, lang = "en")

## ----echo = F------------------------------------------------------------
library(BETS)

## ----eval = F------------------------------------------------------------
#  # Some examples
#  BETS.search(description = "sales ~ retail")
#  BETS.search(description = "'sales volume index' ~ vehicles")
#  BETS.search(description = "'distrito federal'", periodicity = 'A', src = 'IBGE')

## ----eval = F------------------------------------------------------------
#  # Search for accumulated GDP series
#  BETS.search(description = "gdp accumulated", unit = "US$", view = F)

## ----echo = F, results='hide'--------------------------------------------
#results <- BETS.search(description = "gdp accumulated", unit = "US$", view = F)

## ----echo = F------------------------------------------------------------
#results

## ----eval = F------------------------------------------------------------
#  # Search for consumption series, not seasonally adjusted, not private
#  BETS.search(description = "consumption ~ 'seasonally adjusted' ~ private", view = F)

## ----echo = F, results='hide'--------------------------------------------
results <- BETS.search(description = "consumption ~ 'seasonally adjusted' ~ private", view = F)

## ----echo = F------------------------------------------------------------
head(results)

## ----eval = F------------------------------------------------------------
#  BETS.get(code, data.frame = FALSE)

## ------------------------------------------------------------------------
# Get the 12-month cumulative GDP series in dollars
gdp_accum <- BETS.get(4192)
window(gdp_accum, start = c(2014,1))

## ------------------------------------------------------------------------
#Get the series for the GDP of the Federal District at market prices
gdp_df <- BETS.get(23992, data.frame = T)
head(gdp_df)

## ----eval = F------------------------------------------------------------
#  BETS.save.sas(code, data = NULL, file.name = "series")
#  BETS.save.spss(code, data = NULL, file.name = "series")
#  BETS.save.stata(code, data = NULL, file.name = "series")

## ----eval = F------------------------------------------------------------
#  # Save the series for the net public debt in the default Excel format
#  BETS.save.stata(code = 2078, file.name = "series_stata.dta")
#  
#  # Save the series for the net public debt in the default Excel format
#  BETS.save.stata(code = 2078, file.name = "series_stata.dta")
#  
#  # Save any series in SPSS format
#  my.series <- BETS.get(4447)
#  BETS.save.spss(data = my.series, file.name = "series_spss")

## ----eval = F------------------------------------------------------------
#  BETS.chart(ts, file = NULL, open = TRUE, lang = "en", params = NULL)

## ----eval = F------------------------------------------------------------
#  # Uncertainty Index chart
#  BETS.chart(ts = 'iie_br', file = "iie_br", open = TRUE)
#  
#  # Leading and Coincident Labor Indicators charts
#  BETS.chart(ts = "lab_mrkt", file = "lab_mrkt.png", open = TRUE)

## ----eval = F------------------------------------------------------------
#  BETS.dashboard(type = "business_cycle", charts = "all", saveas = NA, parameters = NULL)

## ----eval = F------------------------------------------------------------
#  BETS.dashboard(type = "business_cycle", saveas = "survey.pdf")

## ----eval = F------------------------------------------------------------
#  parameters = list(author = "FGV/IBRE",
#                    url = "http://portalibre.fgv.br/",
#                    text = "text.txt",
#                    logo = "logo_ibre.png")
#  
#  BETS.dashboard(type = "macro_situation", parameters = parameters)

