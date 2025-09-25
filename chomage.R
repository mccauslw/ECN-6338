M = 5000
x <- seq(0, 500, length.out=M)
colours <- c('red', 'green', 'blue')
beta <- c(7/9, 24/25, 1/2) # Le paramètre beta pour 3 cas
x_et <- c(50, 18, 75)      # x étoile pour les 3 cas
y_et <- c(200, 216, 150)   # y étoile pour les 3 cas
op <- par(pch=16, bty='l')
plot(x_et, y_et,
     xlab='x (blé)', ylab='y (boeuf)', xlim=c(0,500),ylim=c(0,400))
abline(a=300, b=-2, lty='dashed')    # Contrainte L
abline(a=225, b=-0.5, lty='dashed')  # Contrainte T
polygon(c(0, 0, 50, 150), c(0, 225, 200, 0), col='grey', lty='dotted')
CI = matrix(0, nrow=3, ncol=M)
for (i in 1:3) {
  CI[i,] <- exp( log(y_et[i])
                 + (log(x_et[i])-log(x)) * (1-beta[i])/beta[i]
  )
  lines(x, CI[i,], col=colours[i])
  points(x_et[i], y_et[i], col=colours[i])
}
text(200, 160, 'beta = 7/9', col=colours[1])
text(200, 220, 'beta = 24/25', col=colours[2])
text(200, 80, 'beta = 1/2', col=colours[3])
text(350, 75, 'contrainte T (terre)')
text(25, 325, 'contrainte L (travail)')
par(op)
