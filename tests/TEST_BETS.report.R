##-- SARIMA

# Caso base, default

report()

# Caso do artigo 

dum <- dummy(start= c(2002,1) , end = c(2016,4) , from = c(2008,7) , to = c(2008,11))

params = list( 
  cf.lags = 48,
  n.ahead = 12,
  ur.test =  list(mode = "ADF", type = "drift", lags = 11, selectlags = "BIC", level = "5pct"),
  box.test = list(lag = 2),
  dummy = dum
)

report(ts = window(BETSget(21864), start= c(2002,1) , end = c(2015,10)), 
            parameters = params,
            series.saveas = "csv")

# Caso base, com codigo e parametros 

parameters = list(
  cf.lags= 48,
  n.ahead = 12 ) 

report(ts = 21864, parameters = parameters)

# Caso base, com dummy, objeto do tipo ts
dum <- dummy(start= c(2002,1) , end = c(2017,1) , from = c(2008,9) , to = c(2008,11))

parameters = list( 
    cf.lags = 25,
    n.ahead = 15,
    dummy = dum
)

report(ts = window(BETSget(21864), start= c(2002,1) , end = c(2015,10)), parameters = parameters)

# Caso base, objeto do tipo ts, mudando teste ARCH e teste Box

parameters = list( 
    cf.lags = 25,
    n.ahead = 15,
    dummy = dum,
    arch.test = list(lags = 12, alpha = 0.01),
    box.test = list(type = "Box-Pierce")
)

report(ts = window(BETSget(21864), start= c(2002,1) , end = c(2015,10)), parameters = parameters)

# Outra serie

params = list( 
  cf.lags = 36,
  n.ahead = 6,
  dummy = dummy(start= c(2002,1) , end = c(2017,12) , from = c(2008,7) , to = c(2008,11)),
  arch.test = list(lags = 12, alpha = 0.025),
  box.test = list(type = "Box-Pierce", lag = 5)
)

report(ts = 21863, parameters = params)



# Caso base, mudando teste de raiz unitaria

parameters = list( 
    cf.lags = 25,
    n.ahead = 15,
    ur.test = list(mode = "KPSS", level = "2.5pct")
)

report(ts = 21864, parameters = parameters)

# Caso base, outro codigo

report(ts = 4447)

# Caso base, salvando a serie e as previsoes num csv

report(ts = 4447, series.saveas = "csv")

# Mais de uma serie, apenas IDs

series = list(4447, 21864)

parameters = list(
  cf.lags= 48,
  n.ahead = 12 ) 


report(ts = series, parameters = parameters)

# Mais de uma serie, apenas objetos do tipo ts

series = list(BETS.get(4447), BETS.get(21864))

parameters = list(
  cf.lags= 25,
  n.ahead = 15 ) 


report(ts = series, parameters = parameters)


# Uma serie, codigo, salvar no Desktop

report(ts = 4447, parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE")

# Uma serie, objeto ts, salvar no Desktop

report(ts = get(4447), parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE2")

# Duas series, lista mista, salvar no Desktop

series = list(4447, BETSget(21864))

report(ts = series, parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE")

# Salvar previsoes

report(ts = 4447, parameters = parameters, series.saveas = "csv")

##-- GRNN

# Caso base, default

report(mode = "GRNN")
params = list(regs = 4382)
report(mode = "GRNN", ts = 13522, parameters = params)

# Mistura de regressores 

target = BETSget(13521)

target_monthly = vector(mode = "numeric")

for(t in target){
  target_monthly = c(target_monthly, rep(t,12))
}

target_monthly = c(target_monthly, rep(4.5,12))
target = ts(target_monthly, start = c(1999,1), end = c(2016,11), frequency = 12)

regs = list(4382,target)

params = list(regs = regs)

report(mode = "GRNN", ts = 13522, parameters = params)

params = list(regs = regs, var.names = c("ipca","gdp","target"))

report(mode = "GRNN", ts = 13522, parameters = params)

# Regressores customizados

gdp = BETSget(4382) 
ipca = BETSget(13522)

gdp_real = deflate(gdp, ipca, type = "point.perc")

require(mFilter)

trend =  fitted(hpfilter(gdp_real))
h_gdp_real = gdp_real - trend


regs = list(h_gdp_real,target)

options(digits = 3)
params = list(regs = regs, var.names = c("ipca","gdp","target"), sigma.interval = c(0.06,0.065), sigma.step = 0.005)

report(mode = "GRNN", ts = 13522, parameters = params, series.saveas = "csv")

##-- HOLT-WINTERS

# Default

report(mode = "HOLT-WINTERS")

# Other series

report(mode = "HOLT-WINTERS", ts = 4447)

# Save forecasts

report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "sas")

# Change parameters

library(knitr)

params = list(alpha = 0.5, gamma = T)

report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "csv", parameters = params)


params = list(gamma = T, beta = T)

Breport(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "csv", parameters = params)

