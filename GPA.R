library(foreign)
library(wooldridge)

# Charger les données de l'exemple 3.1 de
# Wooldridge (2016) Introductory Econometrics
data(gpa1, package='wooldridge')

# Considérez le modèle linéaire
#   colGPA_i = beta_1 + beta_2 hsGPA_i + beta_3 ACT_i + epsilon_i
#
# colGPA : college GPA
# hsGPA  : high school GPA
# ACT    : ACT score

# Faite l'analyse avec la fonction lm (linear model)
GPA_res <- lm(colGPA ~ hsGPA + ACT, data=gpa1)
summary(GPA_res)

# Construire le vecteur y et la matrice X
y <- gpa1$colGPA
n <- length(y)
X <- cbind(rep(1, n), gpa1$hsGPA, gpa1$ACT)

# Calculer l'estimation MCO b avec la fonction solve, qui utilise
# la décomposition LU
XX <- t(X) %*% X
Xy <- t(X) %*% y
b_LU <- solve(XX, Xy)        # MCO b, avec LU

# Calculer l'estimation MCO b avec la décomposition de Cholesky
# Les fonctions pertinentes sont chol et backsolve
R <- chol(XX)
Rb <- forwardsolve(R, Xy, upper.tri=T, transpose=T)
b_Chol <- backsolve(R, Rb)   # MCO b, avec Cholesky

# Une exercice est de calcular l'estimation MCO b avec la décomposition QR
