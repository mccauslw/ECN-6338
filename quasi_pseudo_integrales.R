# Fonction indicatrice pour le simplexe Delta^(n-1)
# (d=2 pour un triangle, d=3 pour un tétraèdre)
f_simp <- function(x) { # x est M par d; la fonction est évaluée à chaque rangée
  ifelse(rowSums(x) <= 1, 1, 0)
}

# Fonction pour la surface d'une hyperpyramide avec un hypercube [0,1]^d
# comme base. (d=1 pour un triangle, d=2 pour les pyramides égyptiennes)
f_pyr <- function(x) { # x est un d-vecteur; la fonction n'est pas vectorisée
  # Le facteur multiplicatif 2 fait en sorte que la hauteur de la
  # hyperpyramide est 1, atteint à (0.5,...,0.5).
  2 * min(min(x), min(1-x))
}

M = 1000 # Nombre de points
d = 3    # Dimension du problème 
suite_halton <- halton(M, d)
suite_pseudo <- matrix(runif(M*d), nrow=M, ncol=d)

# Approximation numérique du volume du simplexe.
# Volume exacte : I = 1/d! (pour comparaison)
I_simp_halton <- mean(f_simp(suite_halton))    # Approximation avec la suite Halton
f_simp_pseudo <- f_simp(suite_pseudo)          # Valeur de f_simp aux M tirages
I_simp_pseudo <- mean(f_simp_pseudo)           # Estimation pseudo-aléatoire de I
I_simp_pseudo_et <- sqrt(var(f_simp_pseudo)/M) # Écart-type de l'erreur (M tirages iid)
I_simp_exacte <- 1/factorial(d)

# Le deuxième argument de 'apply' veut dire évaluer la fonction rangée par rangée
# J'utilise apply pour f_pyr, parce qu'il est difficile de faire la vectorisation
# à l'intérieure de f_pry

# Approximation numérique du volume de la hyperpyramide avec base [0,1]^d
# et hauteur 1.
# Volume exacte : I = 1/(d+1) (pour comparaison)
I_pyr_halton <- mean(apply(suite_halton, 1, f_pyr)) # Approximation avec la suite Halton
f_pyr_pseudo <- apply(suite_pseudo, 1, f_pyr)       # Valeur de f_pyr aux M tirages
I_pyr_pseudo <- mean(f_pyr_pseudo)                  # Estimation pseudo-aléatoire de I
I_pyr_pseudo_et <- sqrt(var(f_pyr_pseudo)/M)        # Écart-type de l'erreur (M tirages iid)
I_pyr_exacte <- 1/(d+1)
