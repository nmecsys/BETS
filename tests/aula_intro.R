par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)

## AR(1), estacionário

set.seed(12)
n = 500

y <- vector(mode = "numeric", length = 500)
y[1] <- 0

for(i in 2:500){
  y[i] <- 0.5*y[i-1] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "AR(1), phi = 0.5", col = "royalblue")

## AR(1), não-estacionário

for(i in 2:500){
  y[i] <- y[i-1] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "AR(1), phi = 1", col = "royalblue")

## AR(1), média diferente de 0

y[1] <- 5

for(i in 2:500){
  y[i] <- 5 + 0.5*y[i-1] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "AR(1), phi = 0.5, média não-nula", col = "royalblue")


## MA(1)

u <- vector(mode = "numeric", length = 500)
u[1] <- rnorm(1,0,1)
y[1] <- 0

for(i in 2:500){
  u[i] <- rnorm(1,0,1)
  y[i] <- u[i] - 0.5*u[i-1]
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "MA(1), theta = 0.5", col = "royalblue")

## AR(2), estacionário

y <- vector(mode = "numeric", length = 500)
y[1] <- 0
y[2] <- 0

for(i in 3:500){
  y[i] <- 0.4*y[i-1] + 0.5*y[i-2] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "AR(2), phi1 = 0.4, phi2 = 0.5", col = "royalblue")

## AR(2), não-estacionário

y <- vector(mode = "numeric", length = 500)
y[1] <- 0
y[2] <- 0

for(i in 3:500){
  y[i] <- 0.5*y[i-1] + 0.5*y[i-2] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "AR(2), phi1 = 0.4, phi2 = 0.5", col = "royalblue")

## MA(2)

u <- vector(mode = "numeric", length = 500)
u[1] <- rnorm(1,0,1)
u[2] <- rnorm(1,0,1)
y[1] <- 0

for(i in 3:500){
  u[i] <- rnorm(1,0,1)
  y[i] <- u[i] - 0.5*u[i-1] - 1.2*u[i-2]
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "MA(2), theta1 = 0.5, theta2 = 1.2", col = "royalblue")

## ARMA(1,1), estacionário

u <- vector(mode = "numeric", length = 500)
u[1] <- rnorm(1,0,1)
y[1] <- 0

for(i in 2:500){
  u[i] <- rnorm(1,0,1)
  y[i] <- 0.5*y[i-1] - 0.5*u[i-1] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "MA(2), theta1 = 0.5, theta2 = 1.2", col = "royalblue")

## SARIMA(1,0,0)(1,0,0)

y <- vector(mode = "numeric", length = 500)
y[1:13] <- 0

for(i in 14:500){
  y[i] <- 0.5*y[i-1] + 0.5*y[i-12] - 0.25*y[i-13] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "SARIMA(1,0,0)(1,0,0)[12], phi1 = 0.5, Phi1 = 0.5", col = "royalblue")

## SARIMA(1,0,0)(0,1,0)

for(i in 14:500){
  y[i] <- 0.5*y[i-1] + y[i-12] - 0.5*y[i-13] + rnorm(1,0,1)
}

plot.ts(y, lwd = 1.5, lty = 1, xlab = "", ylab = "", main = "SARIMA(1,0,0)(0,1,0)[12], phi1 = 0.5, Phi1 = 1", col = "royalblue")



