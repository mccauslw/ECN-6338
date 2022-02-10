# Éléments 1 et 2 de la fonction
f1 <- function(x) {x[1]^2 + x[2]^2 - 1}; f11 <- function(x) {2*x[1]}
f2 <- function(x) {2*x[1]^2 - x[2] - 1}

# Mise à jour de Seidel pour x_1 avec l'équation f1=0
# Il y a deux racines, pour s=-1, s=1
seidel_1 <- function(x, s) {
  if (abs(x[2] > 1))
    x1 <- 0
  else
    x1 <- s*sqrt(1 - x[2]^2)
  x <- c(x1, x[2])
}

# Version linéaire de Seidel pour x_1 avec équation f1=0
seidel_1_lineaire <- function(x, s) {
  x1 <- x[1] - f1(x)/f11(x)
  x <- c(x1, x[2])
}

# Mise à jour de Seidel pour x_2 avec l'équation f2=0
seidel_2 <- function(x, s) {
  x2 <- 2*x[1]^2-1
  x <- c(x[1], x2)
}

# Mise à jour de Seidel pour x_1 avec l'équation f2=0
# Il y a deux racines, pour s=-1, s=1
perm_seidel_1 <- function(x, s) {
  if (x[2] < -1)
    x1 <- 0
  else
    x1 <- s*sqrt(0.5*(1+x[2]))
  x <- c(x1, x[2])
}

# Mise à jour de Seidel pour x_2 avec l'équation f1=0
# Il y a deux racines, pour s=-1, s=1
perm_seidel_2 <- function(x, s) {
  if (abs(x[1] > 1))
    x2 <- 0
  else
    x2 <- s*sqrt(1 - x[1]^2)
  x <- c(x[1], x2)
}

n_steps <- 10
X <- matrix(0, 2*n_steps + 1, 2)

# Par défaut, les signes s1 et s2 sont 1 (positives)
s1 <- 1; s2 <- 1;

# Plusieurs cas, selon la linéarité, les signes, la permutation
if (name == 'seidel')
  {u1 <- seidel_1; u2 <- seidel_2}
if (name == 'seidel-')
  {u1 <- seidel_1; s1 <- -1; u2 <- seidel_2}
if (name == 'seidel_lin')
  {u1 <- seidel_1_lineaire; u2 <- seidel_2}
if (name == 'perm_seidel')
  {u1 <- perm_seidel_1; u2 <- perm_seidel_2}
if (name == 'perm_seidel+-')
  {u1 <- perm_seidel_1; s1 = 1; u2 <- perm_seidel_2; s2 <- -1}
if (name == 'perm_seidel-+')
  {u1 <- perm_seidel_1; s1 = -1; u2 <- perm_seidel_2; s2 <- 1}
if (name == 'perm_seidel--')
  {u1 <- perm_seidel_1; s1 = -1; u2 <- perm_seidel_2; s2 <- -1}

# Itération Gauss-Seidel
X[1,] = x0
for (i in 1:n_steps) {
  X[2*i,] = u1(X[2*i-1,], s1)
  X[2*i+1,] = u2(X[2*i,], s2)
}

# Graphique de courbes f1 = 0 et f2 = 0
x = seq(-2, 2, length=201)
plot(x, 2*x^2 - 1, type='l', ylim=c(-1.0, 1.0), xlab='x1', ylab='x2', asp=1)
th = seq(0, 2*pi, length=401)
lines(cos(th), sin(th))

# Graphique de la suite de 10 itérations (20 points)
lines(X, col='grey', lty='dashed')
points(X, col='blue', pch=20)