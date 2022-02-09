# Illustration des deux premières itérations du pas
# Newton-Raphson pour trouver une racine d'une fonction
# f(x) dans l'intervalle [0,1]

# La définition de la fonction f et de fp (f prime) de sa dérivée
f <- function(x) {(1-x)^3 - log(1+x)}
fp <- function(x) {-3*(1-x)^2 - 1/(1+x)}

# Graphique de la fonction avec l'axe des abscisses
x = seq(0, 1, by=0.001)
plot(x, f(x), type='l', col='blue')
abline(h=0)

# Point initial x0
x0 <- 0;
f0 <- f(x0); fp0 <- fp(x0)          # Evaluations à x0
points(x0, f0, col='blue', pch=20)  # (x0, f0) sur la graphique
text(x0 + 0.05, f0, labels='(x0,f0)', col='blue')

# Première itération, par Newton-Raphson

# Droite de tangente à x0
abline(a=f0, b=fp0, col='red')
# x1 par Newton-Raphson
x1 <- x0-f0/fp0              
# Un point rouge à (x1, 0), l'intersection de
# la droite de tangente à x0 et l'axe des abscisses
points(x1, 0, col='red', pch=20)
# Evaluations à x1
f1 = f(x1); fp1 = fp(x1)
# Un point bleu à (x1, f1)
points(x1, f1, col='blue', pch=20)
text(x1 + 0.05, f1, labels='(x1,f1)', col='blue')


# Deuxième itération, par Newton-Raphson

# Droite de tangente à x1
abline(a=f1-x1*fp1, b=fp1, col='red')
# x2 par Newton-Raphson 
x2 <- x1-f1/fp1
# Un point rouge à (x2, 0), l'intersection de
# la droite de tangente à x1 et l'axe des abscisses
points(x2, 0, col='red', pch=20)
# Evaluations à x2
f2 <- f(x2);
# Un point bleu à (x2, f2)
points(x2, f(x2), col='blue', pch=20)
text(x2 + 0.05, f2 + 0.02, labels='(x2,f2)', col='blue')

# Deuxième itération alternative, par interpolation linéaire

# Droite de sécante entre x0 et x1
abline(a=f0, b=(f1-f0)/(x1-x0), col='green')
# s par interpolation linéaire (droite sécante)
s <- -f0*(x1-x0)/(f1-f0)
# Un point vert à (s, 0), l'intersection de
# la droite de sécante entre x0 et x1 et l'axe des abscisses
points(s, 0, col='green', pch=20)
text(s, -0.05, col='green', labels='s')


# Tentation d'une première itération alternative, par Newton-Raphson
# (On commence par x0_alt = 1)

# Évaluations à x0_alt
x0_alt <- 1;
f0_alt <- f(x0_alt);
f0p_alt <- fp(x0_alt);
# Un point bleu à (x0_alt, f0_alt)
points(x0_alt, f0_alt, col='blue', pch=20)
# Droite de tangente à f0_alt
abline(a=f0_alt-x0_alt*f0p_alt, b=f0p_alt, col='red')

