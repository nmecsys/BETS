library(BETS)

## Business Cycle Dashboard

BETS.chart(ts = "animal_spirits", file = "animal_spirits", open = T)
BETS.chart(ts = "animal_spirits", file = "animal_spirits.png", open = F)
BETS.chart(ts = "animal_spirits")

BETS.chart(ts = "ei_vars", file = "ei_vars", open = T)
BETS.chart(ts = "ei_vars", file = "ei_vars.png", open = F)
BETS.chart(ts = "ei_vars")

BETS.chart(ts = "gdp_comps", file = "gdp_comps", open = T)
BETS.chart(ts = "gdp_comps", file = "gdp_comps.png", open = F)
BETS.chart(ts = "gdp_comps")

BETS.chart(ts = "gdp_unemp", file = "gdp_unemp", open = T)
BETS.chart(ts = "gdp_unemp", file = "gdp_unemp.png", open = F)
BETS.chart(ts = "gdp_unemp")

BETS.chart(ts = "gdp_vars", file = "gdp_vars", open = T)
BETS.chart(ts = "gdp_vars", file = "gdp_vars.png", open = F)
BETS.chart(ts = "gdp_vars")

BETS.chart(ts = "iie_br", file = "iie_br", open = T)
BETS.chart(ts = "iie_br", file = "iie_br.png", open = F)
BETS.chart(ts = "iie_br")

BETS.chart(ts = "misery_index", file = "misery_index", open = T)
BETS.chart(ts = "misery_index", file = "misery_index.png", open = F)
BETS.chart(ts = "misery_index")

BETS.chart(ts = "transf_ind", file = "transf_ind.png", open = T)
BETS.chart(ts = "transf_ind", file = "transf_ind", open = F)
BETS.chart(ts = "transf_ind")

BETS.chart(ts = "servc", file = "servc.png", open = T)
BETS.chart(ts = "servc", file = "servc", open = F)
BETS.chart(ts = "servc")

BETS.chart(ts = "retail", file = "retail.png", open = T)
BETS.chart(ts = "retail", file = "retail", open = F)
BETS.chart(ts = "retail")

BETS.chart(ts = "constr", file = "constr.png", open = T)
BETS.chart(ts = "constr", file = "constr", open = F)
BETS.chart(ts = "constr")

BETS.chart(ts = "consm", file = "consm.png", open = T)
BETS.chart(ts = "consm", file = "consm", open = F)
BETS.chart(ts = "consm")

BETS.chart(ts = "lab_mrkt", file = "lab_mrkt.png", open = T)
BETS.chart(ts = "lab_mrkt", file = "lab_mrkt", open = F)
BETS.chart(ts = "lab_mrkt")

BETS.chart(ts = "cap_utl", file = "cap_utl.png", open = T)
BETS.chart(ts = "cap_utl", file = "cap_utl", open = F)
BETS.chart(ts = "cap_utl")

BETS.chart("sent_ind", file = "sent_ind.png", open = T)
BETS.chart("gdp_mon", file = "gdp_mon.png", open = T)
BETS.chart("lei", file = "lei.png", open = T)

## Macro Situation Dashboard

BETS.chart("ipca_with_core", file = "ipca_with_core.png", open = F)
BETS.chart("ipca_with_core", file = "ipca_with_core.pdf", open = F)
BETS.chart("ipca_with_core", open = T)

BETS.chart("ulc", file = "ulc.png", open = F)
BETS.chart("ulc", file = "ulc.pdf", open = F)

BETS.chart("unemp", file = "unemp.png", open = F)
BETS.chart("unemp",  file = "unemp.pdf", open = F)

BETS.chart("vargdp", file = "vargdp.png", open = F)
BETS.chart("vargdp", file = "vargdp.pdf", open = F)

BETS.chart("indprod", file = "indprod.png", open = F)
BETS.chart("indprod", file = "indprod.pdf", open = F)

BETS.chart("eap", file = "eap.png", open = F)
BETS.chart("eap", file = "eap.pdf", open = F)

BETS.chart("cdb", file = "cdb.png", open = F)
BETS.chart("cdb", file = "cdb.pdf", open = F)

BETS.chart("selic", file = "selic.png", open = F)
BETS.chart("selic", file = "selic.pdf", open = F)


##-- Custom Charts

## NORMAL STYLE

# General Government Debt

ts <- window(BETS.get(4537),start = c(2006,1))
    
params <- list(
  type = "lines",
  title = "General Government Debt",
  arr.len = 6,
  ylim = c(30,82),
  subtitle = "% GDP",
  legend = c("Gross", "Net"),
  extra = window(BETS.get(4536),start = c(2006,1)),
  extra.arr.ort = 'h',
  extra.arr.len = 1 
)

BETS.chart(ts = ts, style = "normal", file = "debt.png", open = T, params = params)

# International Reserves

ts <- window(BETS.get(3545),start = 2006)/100

params <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    colors = 'chocolate1'
)

BETS.chart(ts = ts, style = "normal", file = "int_res.pdf", open = T, params = params)

# Current Acccount x Direct Foreing Investment

ts <- window(BETS.get(23461), start = 2006)/100

params <- list(
    type = "bar",
    title = "Current Account vs Direct Foreign Investment",
    subtitle = "Net, Anual, US$ Billions",
    colors = c("royalblue","deepskyblue2"),
    extra = window(BETS.get(23645), start = 2006)/100,
    legend.pos = "bottomleft",
    legend = c("Current Account","Direct Foreign Investment"),
    extra.arr.ort = 'v',
    extra.arr.len = 200
)

BETS.chart(ts = ts, style = "normal", file = "ca_di.pdf", open = T, params = params)

# External Debt

df <- BETS.get(11407, data.frame = T)
df <- df[-(1:30),2]
ts <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

df <- BETS.get(11409, data.frame = T)
df <- df[-(1:30),2]
extra <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

params <- list(
    type = "lines",
    title = "External Debt",
    subtitle = "% GDP",
    colors = c("aquamarine4","aquamarine3"),
    legend = c("Gross", "Net"),
    extra = extra,
    arr.len = 5,
    extra.arr.ort = 'v',
    extra.arr.len = 5,
    legend.pos = "bottomleft"
)

BETS.chart(ts = ts, style = "normal", file = "ext_debt.pdf", open = T, params = params)

## PLOTLY STYLE

# General Government Debt

ts <- window(BETS.get(4537),start = c(2006,1))

params <- list(
    type = "lines",
    title = "General Government Debt",
    subtitle = "% GDP",
    legend = c("Gross", "Net"),
    extra = window(BETS.get(4536),start = c(2006,1))
)

BETS.chart(ts = ts, style = "plotly", file = "debt.png", open = T, params = params)

# International Reserves

ts <- window(BETS.get(3545),start = 2006)/100

params <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    colors = '#FF7F24',
    arr.len = 10
)

BETS.chart(ts = ts, style = "plotly", file = "int_res.png", open = T, params = params)

# Current Acccount x Direct Foreing Investment

ts <- window(BETS.get(23461), start = 2006)/100

params <- list(
    type = "bar",
    title = "Current Account vs Direct Foreign Investment",
    subtitle = "Net, Anual, US$ Billions",
    colors = c("#4169E1","#00BFFF"),
    extra = window(BETS.get(23645), start = 2006)/100,
    legend = c("Current Account","Direct Foreign Investment"),
    extra.arr.ort = 'v'
)

BETS.chart(ts = ts, style = "plotly", file = "ca_di.png", open = T, params = params)

# External Debt

df <- BETS.get(11407, data.frame = T)
df <- df[-(1:30),2]
ts <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

df <- BETS.get(11409, data.frame = T)
df <- df[-(1:30),2]
extra <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

params <- list(
    type = "lines",
    title = "External Debt",
    subtitle = "% GDP",
    colors = c("#458B74","#66CDAA"),
    legend = c("Gross", "Net"),
    extra = extra
)

BETS.chart(ts = ts, style = "plotly", file = "ext_debt.png", open = T, params = params)

# par(mar = c(5.1,4.1,4.1,2.1))
# series = ts(1:20, start = 2000, end = 2019, frequency = 1)
# b = barplot(as.vector(series), names.arg = as.vector(time(series)), xlab = "", ylab = "",  xpd = FALSE)
# lines(y = as.vector(series), lwd = 2.5, lty = 2)

data <- BETS.get(21864)

params <- list(
    title = "Intermediate Goods Production",
    subtitle = "Index",
    colors = c("royalblue")
)

BETS.chart(ts = data, style = "normal", open = T, params = params, file = "igp.png")


params <- list(
    type = "lines",
    title = "External Debt",
    subtitle = "% GDP",
    colors = c("aquamarine4","aquamarine3"),
    legend = c("Gross", "Net"),
    extra = extra,
    arr.len = 5,
    extra.arr.ort = 'v',
    extra.arr.len = 5,
    legend.pos = "bottomleft"
)

BETS.chart(ts = ts, style = "normal", file = "ext_debt.pdf", open = T, params = params)


