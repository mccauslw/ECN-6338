library(wooldridge)
library(truncnorm)

# Charger les données de l'exemple 17.1 de
# Wooldridge (2016) Introductory Econometrics
data(mroz, package='wooldridge')

# Construire le vecteur y et la matrice X
y <- as.vector(mroz$inlf)
n <- length(y)
X_tbl <- mroz[, c('nwifeinc', 'educ', 'exper', 'expersq', 'age', 'kidslt6', 'kidsge6')]
X_tbl$constant = 1
X = as.matrix(X_tbl)

# Fonction de log vraisemblance
logL <- function(beta, y, X) {
  Phi <- pnorm(X %*% beta)    # Probabilité d'être dans la population active
  return(sum(y * log(Phi) + (1-y) * log(1-Phi)))
}

# Gradient de la log vraisemblance
logL_grad <- function(beta, y, X) {
  Xbeta <- X %*% beta
  Phi <- pnorm(Xbeta)
  phi <- dnorm(Xbeta)
  return(t(y * (phi/Phi) + (1-y) * (-phi/(1-Phi))) %*% X)
}

# Log densité a priori
logf_apriori <- function(beta, beta_, H_) {
  return(-0.5 * t(beta-beta_) %*% H_ %*% (beta-beta_))
}

# Log densité conjointe
logf_conj <- function(beta, beta_, H_, y, X) {
  return(logf_apriori(beta, beta_, H_) + logL(beta, y, X))
}

Metropolis <- function(y, X, beta_, H_, M) {
  K <- length(beta_)
  n <- length(y)
  
  # Valeurs initials
  beta <- qr.solve(X, y)
  ln_post <- logf_conj(beta, beta_, H_, y, X)
  
  # Stockage
  Beta <- matrix(0, M, K)
  lnf <- vector('numeric', M)
  
  for (i in 1:M) {
    R <- 1 * chol(t(X) %*% X)
    u <- rnorm(K)
    beta_et <- beta + backsolve(R, u)
    ln_post_et <- logf_conj(beta_et, beta_, H_, y, X)
    if (runif(1) <= exp(ln_post_et - ln_post)) {
      beta <- beta_et
      ln_post <- ln_post_et      
    }
    Beta[i,] = beta
    lnf[i] = ln_post
  }
  return(list(Beta = Beta, lnf = lnf))
}

Gibbs <- function(y, X, beta_, H_, M) {
  K <- length(beta_)
  n <- length(y)
  
  # Quantités constantes
  H__ <- H_ + t(X) %*% X   # Précision a posteriori
  R__ <- chol(H__)         # Facteur de Cholesky triangulaire supérieur
  a = ifelse(y, 0, -Inf)
  b = ifelse(y, Inf, 0)
  
  # Valeurs initiales
  beta <- qr.solve(X, y)
  y_et <- rtruncnorm(n, a, b, X[i,] %*% beta)
  
  # Stockage
  Beta <- matrix(0, M, K)
  Y_et <- matrix(0, M, n)
  
  for (i in 1:M) {
    y_et <- rtruncnorm(n, a, b, X %*% beta)
    R__beta <- rnorm(K) + forwardsolve(R__, t(X) %*% y_et, upper.tri=T, transpose=T)
    beta <- backsolve(R__, R__beta)
    
    Beta[i,] = beta
    Y_et[i,] = y_et
  }
  
  return(list(Beta = Beta, Y_et = Y_et))
}

K <- ncol(X)
beta_ <- rep(0, K)
H_ <- diag(1, K, K)
res_Gibbs <- Gibbs(y, X, beta_, H_, 10000)
res_Metropolis <- Metropolis(y, X, beta_, H_, 10000)
