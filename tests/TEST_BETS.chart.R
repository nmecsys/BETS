library(BETS)

## Business Cycle Dashboard

# chart(ts = "animal_spirits", file = "animal_spirits", open = T)
# chart(ts = "animal_spirits", file = "animal_spirits.png", open = F)
# chart(ts = "animal_spirits")

chart(ts = "ei_vars", file = "ei_vars", open = T)
chart(ts = "ei_vars", file = "ei_vars.png", open = F)
chart(ts = "ei_vars")

chart(ts = "gdp_comps", file = "gdp_comps", open = T)
chart(ts = "gdp_comps", file = "gdp_comps.png", open = F)
chart(ts = "gdp_comps")

chart(ts = "gdp_unemp", file = "gdp_unemp", open = T)
chart(ts = "gdp_unemp", file = "gdp_unemp.png", open = F)
chart(ts = "gdp_unemp")

chart(ts = "gdp_vars", file = "gdp_vars", open = T)
chart(ts = "gdp_vars", file = "gdp_vars.png", open = F)
chart(ts = "gdp_vars")

chart(ts = "iie_br", file = "iie_br", open = T)
chart(ts = "iie_br", file = "iie_br.png", open = F)
chart(ts = "iie_br")

chart(ts = "misery_index", file = "misery_index", open = T)
chart(ts = "misery_index", file = "misery_index.png", open = F)
chart(ts = "misery_index")

chart(ts = "transf_ind", file = "transf_ind.png", open = T)
chart(ts = "transf_ind", file = "transf_ind", open = F)
chart(ts = "transf_ind")

chart(ts = "servc", file = "servc.png", open = T)
chart(ts = "servc", file = "servc", open = F)
chart(ts = "servc")

chart(ts = "retail", file = "retail.png", open = T)
chart(ts = "retail", file = "retail", open = F)
chart(ts = "retail")

chart(ts = "constr", file = "constr.png", open = T)
chart(ts = "constr", file = "constr", open = F)
chart(ts = "constr")

chart(ts = "consm", file = "consm.png", open = T)
chart(ts = "consm", file = "consm", open = F)
chart(ts = "consm")

chart(ts = "lab_mrkt", file = "lab_mrkt.png", open = T)
chart(ts = "lab_mrkt", file = "lab_mrkt", open = F)
chart(ts = "lab_mrkt")

chart(ts = "cap_utl", file = "cap_utl.png", open = T)
chart(ts = "cap_utl", file = "cap_utl", open = F)
chart(ts = "cap_utl")

chart("sent_ind", file = "sent_ind.png", open = T)
chart("gdp_mon", file = "gdp_mon.png", open = T)
chart("lei", file = "lei.png", open = T)

## Macro Situation Dashboard

chart("ipca_with_core", file = "ipca_with_core.png", open = F)
chart("ipca_with_core", file = "ipca_with_core.pdf", open = F)
chart("ipca_with_core", open = T)

chart("ulc", file = "ulc.png", open = F)
chart("ulc", file = "ulc.pdf", open = F)

chart("unemp", file = "unemp.png", open = F)
chart("unemp",  file = "unemp.pdf", open = F)

chart("vargdp", file = "vargdp.png", open = F)
chart("vargdp", file = "vargdp.pdf", open = F)

chart("indprod", file = "indprod.png", open = F)
chart("indprod", file = "indprod.pdf", open = F)

chart("eap", file = "eap.png", open = F)
chart("eap", file = "eap.pdf", open = F)

chart("cdb", file = "cdb.png", open = F)
chart("cdb", file = "cdb.pdf", open = F)

chart("selic", file = "selic.png", open = F)
chart("selic", file = "selic.pdf", open = F)


##-- Custom Charts

## NORMAL STYLE

# General Government Debt

ts <- window(BETSget(4537),start = c(2006,1))
    
params <- list(
  type = "lines",
  title = "General Government Debt",
  arr.len = 6,
  ylim = c(30,82),
  subtitle = "% GDP",
  legend = c("Gross", "Net"),
  extra = window(BETSget(4536),start = c(2006,1)),
  extra.arr.ort = 'h',
  extra.arr.len = 1 
)

chart(ts = ts, style = "normal", file = "debt.png", open = T, params = params)

# International Reserves

ts <- window(BETSget(3545),start = 2006)/100

params <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    colors = 'chocolate1'
)

chart(ts = ts, style = "normal", file = "int_res.pdf", open = T, params = params)

# Current Acccount x Direct Foreing Investment

ts <- window(BETSget(23461), start = 2006)/100

params <- list(
    type = "bar",
    title = "Current Account vs Direct Foreign Investment",
    subtitle = "Net, Anual, US$ Billions",
    colors = c("royalblue","deepskyblue2"),
    extra = window(BETSget(23645), start = 2006)/100,
    legend.pos = "bottomleft",
    legend = c("Current Account","Direct Foreign Investment"),
    extra.arr.ort = 'v',
    extra.arr.len = 200
)

chart(ts = ts, style = "normal", file = "ca_di.pdf", open = T, params = params)

# External Debt

df <- BETSget(11407, data.frame = T)
df <- df[-(1:30),2]
ts <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

df <- BETSget(11409, data.frame = T)
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

chart(ts = ts, style = "normal", file = "ext_debt.pdf", open = T, params = params)

## PLOTLY STYLE

# General Government Debt

ts <- window(BETSget(4537),start = c(2006,1))

params <- list(
    type = "lines",
    title = "General Government Debt",
    subtitle = "% GDP",
    legend = c("Gross", "Net"),
    extra = window(get(4536),start = c(2006,1))
)

chart(ts = ts, style = "plotly", file = "debt.png", open = T, params = params)

# International Reserves

ts <- window(BETSget(3545),start = 2006)/100

params <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    colors = '#FF7F24',
    arr.len = 10
)

chart(ts = ts, style = "plotly", file = "int_res.png", open = T, params = params)

# Current Acccount x Direct Foreing Investment

ts <- window(BETSget(23461), start = 2006)/100

params <- list(
    type = "bar",
    title = "Current Account vs Direct Foreign Investment",
    subtitle = "Net, Anual, US$ Billions",
    colors = c("#4169E1","#00BFFF"),
    extra = window(get(23645), start = 2006)/100,
    legend = c("Current Account","Direct Foreign Investment"),
    extra.arr.ort = 'v'
)

chart(ts = ts, style = "plotly", file = "ca_di.png", open = T, params = params)

# External Debt

df <- BETSget(11407, data.frame = T)
df <- df[-(1:30),2]
ts <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

df <- BETSget(11409, data.frame = T)
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

chart(ts = ts, style = "plotly", file = "ext_debt.png", open = T, params = params)

# par(mar = c(5.1,4.1,4.1,2.1))
# series = ts(1:20, start = 2000, end = 2019, frequency = 1)
# b = barplot(as.vector(series), names.arg = as.vector(time(series)), xlab = "", ylab = "",  xpd = FALSE)
# lines(y = as.vector(series), lwd = 2.5, lty = 2)

data <- BETSget(21864)

params <- list(
    title = "Intermediate Goods Production",
    subtitle = "Index",
    colors = c("royalblue")
)

chart(ts = data, style = "normal", open = T, params = params, file = "igp.png")


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

chart(ts = ts, style = "normal", file = "ext_debt.pdf", open = T, params = params)


