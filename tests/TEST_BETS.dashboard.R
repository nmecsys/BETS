library(BETS)

BETS.dashboard()
BETS.dashboard(saveas = "survey.pdf")

# É pra dar erro
BETS.dashboard(type = "none")

# With text, without logo

parameters = list(author = "FGV/IBRE",
                  text = "text.txt",
                  url = "http://portalibre.fgv.br/")

BETS.dashboard(type = "macro_situation", parameters = parameters, saveas = "inst/macro_dashboard.pdf")

# Without text, with logo

parameters = list(author = "FGV/IBRE",
                  url = "http://portalibre.fgv.br/",
                  logo = "logo_ibre.png")

BETS.dashboard(type = "macro_situation", parameters = parameters, saveas = "inst/macro_situation_dashboard.pdf")

# With text, with logo

parameters = list(author = "FGV/IBRE",
                  url = "http://portalibre.fgv.br/",
                  text = "text.txt",
                  logo = "logo_ibre.png")

BETS.dashboard(type = "macro_situation", parameters = parameters)

##-- Custom, normal style

style = 'normal'
charts <- list()
charts.opts <- list()

# General Government Debt

charts[[1]] <- window(BETS.get(4537),start = c(2006,1))

charts.opts[[1]] <- list(
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

BETS.chart(ts = charts[[1]], style = "normal", file = "debt", open = T, params = charts.opts[[1]])


# International Reserves

charts[[2]] <- window(BETS.get(3545),start = 2006)/100

charts.opts[[2]] <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    colors = 'chocolate1',
    trend = T
)

BETS.chart(ts = charts[[2]], style = "normal", file = "int_res.pdf", open = T, params = charts.opts[[2]])


# Current Account vc Direct Foreign Investment

charts[[3]] <- window(BETS.get(23461), start = 2006)/100

charts.opts[[3]] <- list(
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

BETS.chart(ts = charts[[3]], style = "normal", file = "ca_di.pdf", open = T, params = charts.opts[[3]])

# External Debt

df <- BETS.get(11407, data.frame = T)
df <- df[-(1:30),2]
charts[[4]] <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

df <- BETS.get(11409, data.frame = T)
df <- df[-(1:30),2]
extra <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

charts.opts[[4]] <- list(
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

BETS.chart(ts = charts[[4]], style = "normal", file = "debt", open = T, params = charts.opts[[4]])

parameters <- list(
    style = style,
    charts.opts = charts.opts,
    author = "FGV/IBRE",
    url = "http://portalibre.fgv.br/",
    text = "text2.txt",
    logo = "logo_ibre.png"
)

BETS.dashboard(type = "custom", charts = charts, saveas = "custom_dashboard.pdf", parameters = parameters)

##-- Custom, plotly style

style = 'plotly'
charts <- list()
charts.opts <- list()

# General Government Debt

charts[[1]] <- window(BETS.get(4537),start = c(2006,1))

charts.opts[[1]] <- list(
    type = "lines",
    title = "General Government Debt",
    subtitle = "% GDP",
    legend = c("Gross", "Net"),
    extra = window(BETS.get(4536),start = c(2006,1))
)

BETS.chart(ts = charts[[1]], style = "plotly", file = "debt", open = T, params = charts.opts[[1]])


# International Reserves

charts[[2]] <- window(BETS.get(3545),start = 2006)/100

charts.opts[[2]] <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    colors = '#FF7F24',
    arr.len = 10
)

BETS.chart(ts = charts[[2]], style = "plotly", file = "int_res", open = T, params = charts.opts[[2]])


# Current Account vc Direct Foreign Investment

charts[[3]] <- window(BETS.get(23461), start = 2006)/100

charts.opts[[3]] <- list(
    type = "bar",
    title = "Current Account vs Direct Foreign Investment",
    subtitle = "Net, Anual, US$ Billions",
    colors = c("#4169E1","#00BFFF"),
    extra = window(BETS.get(23645), start = 2006)/100,
    legend = c("Current Account","Direct Foreign Investment"),
    extra.arr.ort = 'v'
)

BETS.chart(ts = charts[[3]], style = "plotly", file = "ca_di", open = T, params = charts.opts[[3]])

# External Debt

df <- BETS.get(11407, data.frame = T)
df <- df[-(1:30),2]
charts[[4]] <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

df <- BETS.get(11409, data.frame = T)
df <- df[-(1:30),2]
extra <- window(ts(df, start = c(2000,1), frequency = 4),start = c(2006,1))

charts.opts[[4]] <- list(
    type = "lines",
    title = "External Debt",
    subtitle = "% GDP",
    colors = c("#458B74","#66CDAA"),
    legend = c("Gross", "Net"),
    extra = extra
)

BETS.chart(ts = charts[[4]], style = "plotly", file = "debt", open = T, params = charts.opts[[4]])

parameters <- list(
    style = style,
    charts.opts = charts.opts)

BETS.dashboard(type = "custom", charts = charts, saveas = "custom_dashboard.pdf", parameters = parameters)

