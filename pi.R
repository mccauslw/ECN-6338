library(optimx)
library(lattice)

# Valeur, gradient, hessien
pi_val_grad_hess <- function(x, C, alpha, eta) {
  eta_sur_al <- eta/alpha; eta2 <- eta*eta
  X <- exp(x)           # Vecteur de quantités
  X_al <- exp(alpha*x)  # Vecteur de quantités X_i^alpha
  Q <- sum(X_al)
  Q_m2<-Q^(eta_sur_al-2); Q_m1<-Q_m2*Q; Q_m0<-Q_m1*Q
  
  # Valeur v, gradient g, hessienne h du profit pi
  v <- eta*Q_m0 - t(C) %*% X
  g <- (eta2*Q_m1) * X_al - C*X
  h <- (alpha*eta2*(eta_sur_al-1)*Q_m2) * X_al %*% t(X_al)
  + (alpha*eta2*Q_m1) * diag(X_al) - diag(C*X)
  list(valeur=v, gradient=g, hessien=h)
}

# Valeur seulement
pi_val <- function(x, C, alpha, eta) {
  eta_sur_al <- eta/alpha
  X <- exp(x)           # Vecteur de quantités
  X_al <- exp(alpha*x)  # Vecteur de quantités X_i^alpha
  Q = sum(X_al)
  Q_m0 = Q^eta_sur_al

  # Valeur v
  v = drop(eta*Q_m0 - X %*% C)
}

pi_minus <- function(x, C, alpha, eta) {-pi_val(x, C, alpha, eta)}

# Gradient seulement
pi_grad <- function(x, C, alpha, eta) {
  eta_sur_al <- eta/alpha
  X <- exp(x)           # Vecteur de quantités
  X_al <- exp(alpha*x)  # Vecteur de quantités X_i^alpha
  Q = sum(X_al)
  Q_m1=Q^(eta_sur_al-1)
  
  # Valeur v, gradient g, hessienne h du profit pi
  g = (eta*eta*Q_m1) * X_al - C*X
}

pi_grad_minus <- function(x, C, alpha, eta) {-pi_grad(x, C, alpha, eta)}
