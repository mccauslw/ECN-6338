library(conics)
library(fields)

# Construire une matrice hessienne H = QDQ' à partir d'une décomposition
# en éléments propres
Newton_conic <- function(gradient, theta, lambda) {
  
  # Matrice orthogonale pour une rotation de theta
  Q <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
              nrow=2, ncol=2)
  
  # Matrice diagonale avec les valeurs propres réelles
  D <- diag(lambda)
  
  # Matrice hessienne
  H <- Q %*% D %*% t(Q)
  
  # Paramétrisation de la section conique utilisée par conicPlot
  a <- c(0.5*H[1,1], H[1,2], 0.5*H[2,2], gradient[1], gradient[2], 0)
  
  list(H = H, a = a)
}

# Faire une graphique de la courbe de niveau de l'expansion Taylor
# quadratique.
Newton_plot <- function(gradient, nc)
{
  # En noir :
  # - prochain point Newton
  # - la courbe de niveau (section de conique) qui comprend le point actuel,
  # - les axes majeurs (vecteurs propres) de la courbe de niveau
  conicPlot(nc$a, center=T, sym.axes=T, xlim=c(-1, 2), ylim=c(-1,2), asp=1)
  cc <- conicCenter(nc$a)
  
  points(0, 0, col='green')          # Point actuel (vert)
  
  # Direction du gradient (vert) 
  abline(0, gradient[2]/gradient[1], col='green')

  # Direction Newton (rouge)
  abline(0, cc[2]/cc[1], col='red')
  
  # Direction des axes des ordonnèes (bleu)
  abline(h=0, col='blue')
  abline(v=0, col='blue')
  
  #quiver.plot(x, y, U, V, scale = 0.1, col = "steelblue")
}
