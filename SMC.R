library(tidyverse)
library(reshape2)

set.seed(12345)
target_fcn <- function(x, T) {
  f = (0.6 * dbeta(x, 70, 50) + 0.4 * dbeta(x, 40, 160))^T
}

# Problem specificiation
M <- 50             # Number of particles
nMut <- 5           # Number of repetitions of mutation step (can be zero)
doS <- TRUE         # Flag: do S phase (TRUE) or not
nT <- 5             # Number of temperature increments
T <- seq(1, nT)/nT  # Vector of temperatures

# Reserve space
w <- array(NA, dim = c(M, 1+(2+nMut)*nT))
x <- array(NA, dim = c(M, 1+(2+nMut)*nT))
k <- array(0, dim = c(M, 1+(2+nMut)*nT))
W <- array(0, dim = c(M, 5))
X <- array(0, dim = c(M, 5))

# Intial x and w
step_i = 1
x[, step_i] <- rbeta(M, 1, 1)
w[, step_i] <- w_eq <- rep(1/M, M)
k[, step_i] <- rep(0, M)

i = 0
for (t in T) {
  i = i + 1
  W[, i] = target_fcn(x[, 1], t)
  W[, i] = W[, i] / sum(W[, i])
  X[, i] = x[, 1]
}

sigma = c(0.1, 0.08, 0.05, 0.02, 0.01)
for (t in T) {

  # C (correction) step
  step_i = step_i + 1
  w[, step_i] = target_fcn(x[, step_i-1], T[1])
  w[, step_i] = w[, step_i] / sum(w[, step_i])
  x[, step_i] = x[, step_i-1]
  k[, step_i] <- rep(1, M)
  
  # S (selection) step
  step_i = step_i + 1
  w[, step_i] <- w_eq
  x[, step_i] <- sample(x[, step_i-1], M, replace=TRUE, prob=w[, step_i-1])
  k[, step_i] <- rep(2, M)

  # M (mutation step)
  for (m in 1:nMut) {
    step_i = step_i + 1
    w[, step_i] = w_eq
    lnf_den <- target_fcn(x[, step_i-1], t)
    x_star <- (x[, step_i-1] + rnorm(M, 0, sigma[m])) %% 1
    lnf_num <- target_fcn(x_star, t)
    x[, step_i] <- ifelse(runif(M) < lnf_num/lnf_den, x_star, x[, step_i-1])
    k[, step_i] <- rep(3, M)
  }
}

x_table = as_tibble(melt(x, value.name = "x", varnames=c('m', 't')))
w_table = as_tibble(melt(w, value.name = "w", varnames=c('m', 't')))
k_table = as_tibble(melt(k, value.name = "k", varnames=c('m', 't')))
W_table = as_tibble(melt(W, value.name = "W", varnames=c('m', 't')))
X_table = as_tibble(melt(X, value.name = "X", varnames=c('m', 't')))

wx = left_join(w_table, x_table)
wx = left_join(wx, k_table)
wx = mutate(wx, xs=ifelse(k==1 | k==0, NA, x))
WX = left_join(W_table, X_table)

save(wx, file="~/Library/Mobile Documents/com~apple~CloudDocs/ECN 6338/wx.RData")
save(WX, file="~/Library/Mobile Documents/com~apple~CloudDocs/ECN 6338/W.RData")