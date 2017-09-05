##-- SARIMA

# Caso base, default

BETS.report()

# Caso do artigo 

dum <- BETS.dummy(start= c(2002,1) , end = c(2016,4) , from = c(2008,7) , to = c(2008,11))

params = list( 
  af.lags = 48,
  n.ahead = 12,
  ur.test =  list(mode = "ADF", type = "drift", lags = 11, selectlags = "BIC", level = "5pct"),
  box.test = list(lag = 2),
  dummy = dum
)

BETS.report(ts = window(BETS.get(21864), start= c(2002,1) , end = c(2015,10)), 
            parameters = params,
            series.saveas = "csv")

# Caso base, com codigo e parametros 

parameters = list(
  af.lags= 48,
  n.ahead = 12 ) 

BETS.report(ts = 21864, parameters = parameters)

# Caso base, com dummy, objeto do tipo ts
dum <- BETS.dummy(start= c(2002,1) , end = c(2017,1) , from = c(2008,9) , to = c(2008,11))

parameters = list( 
    af.lags = 25,
    n.ahead = 15,
    dummy = dum
)

BETS.report(ts = window(BETS.get(21864), start= c(2002,1) , end = c(2015,10)), parameters = parameters)

# Caso base, objeto do tipo ts, mudando teste ARCH e teste Box

parameters = list( 
    af.lags = 25,
    n.ahead = 15,
    dummy = dum,
    arch.test = list(lags = 12, alpha = 0.01),
    box.test = list(type = "Box-Pierce")
)

BETS.report(ts = window(BETS.get(21864), start= c(2002,1) , end = c(2015,10)), parameters = parameters)

# Outra serie

params = list( 
  af.lags = 36,
  n.ahead = 6,
  dummy = BETS.dummy(start= c(2002,1) , end = c(2017,12) , from = c(2008,7) , to = c(2008,11)),
  arch.test = list(lags = 12, alpha = 0.025),
  box.test = list(type = "Box-Pierce")
)

BETS.report(ts = 21863, parameters = params)



# Caso base, mudando teste de raiz unitaria

parameters = list( 
    af.lags = 25,
    n.ahead = 15,
    ur.test = list(mode = "KPSS", level = "2.5pct")
)

BETS.report(ts = 21864, parameters = parameters)

# Caso base, outro codigo

BETS.report(ts = 4447)

# Caso base, salvando a serie e as previsoes num csv

BETS.report(ts = 4447, series.saveas = "csv")

# Mais de uma serie, apenas IDs

series = list(4447, 21864)

parameters = list(
  af.lags= 48,
  n.ahead = 12 ) 


BETS.report(ts = series, parameters = parameters)

# Mais de uma serie, apenas objetos do tipo ts

series = list(BETS.get(4447), BETS.get(21864))

parameters = list(
  af.lags= 25,
  n.ahead = 15 ) 


BETS.report(ts = series, parameters = parameters)


# Uma serie, codigo, salvar no Desktop

BETS.report(ts = 4447, parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE")

# Uma serie, objeto ts, salvar no Desktop

BETS.report(ts = BETS.get(4447), parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE2")

# Duas series, lista mista, salvar no Desktop

series = list(4447, BETS.get(21864))

BETS.report(ts = series, parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE")

# Salvar previsoes

BETS.report(ts = 4447, parameters = parameters, series.saveas = "csv")

##-- GRNN

# Caso base, default

BETS.report(mode = "GRNN")
params = list(regs = 4382)
BETS.report(mode = "GRNN", ts = 13522, parameters = params)

# Mistura de regressores 

target = BETS.get(13521)

target_monthly = vector(mode = "numeric")

for(t in target){
  target_monthly = c(target_monthly, rep(t,12))
}

target_monthly = c(target_monthly, rep(4.5,12))
target = ts(target_monthly, start = c(1999,1), end = c(2016,11), frequency = 12)

regs = list(4382,target)

params = list(regs = regs)

BETS.report(mode = "GRNN", ts = 13522, parameters = params)

params = list(regs = regs, var.names = c("ipca","gdp","target"))

BETS.report(mode = "GRNN", ts = 13522, parameters = params)

# Regressores customizados

gdp = BETS.get(4382) 
ipca = BETS.get(13522)

gdp_real = BETS.deflate(gdp, ipca, type = "point.perc")

require(mFilter)

trend =  fitted(hpfilter(gdp_real))
h_gdp_real = gdp_real - trend


regs = list(h_gdp_real,target)

options(digits = 3)
params = list(regs = regs, var.names = c("ipca","gdp","target"), sigma.interval = c(0.06,0.065), sigma.step = 0.005)

BETS.report(mode = "GRNN", ts = 13522, parameters = params, series.saveas = "csv")

##-- HOLT-WINTERS

# Default

BETS.report(mode = "HOLT-WINTERS")

# Other series

BETS.report(mode = "HOLT-WINTERS", ts = 4447)

# Save forecasts

BETS.report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "sas")

# Change parameters

library(knitr)

params = list(alpha = 0.5, gamma = T)

BETS.report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "csv", parameters = params)


params = list(gamma = T, beta = T)

BETS.report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "csv", parameters = params)

