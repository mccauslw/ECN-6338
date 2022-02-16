phi_poly <- function(t, p0, m0, p1, m1, a, b)
{
  phi_1 <- 2*t^3 - 3*t^2 + 1
  phi_2 <- t^3 - 2*t^2 + t
  phi_3 <- -2*t^3 + 3*t^2
  phi_4 <- t^3 - t^2
  list(phi_1 = phi_1, phi_2 = phi_2, phi_3 = phi_3, phi_4 = phi_4)
}

t <- seq(0, 1, by=0.001)
phi <- phi_poly(t)
plot(t, phi$phi_1, type='l', ylim=c(-0.2, 1.0))
lines(t, phi$phi_2, col='red')
lines(t, phi$phi_3, col='black')
lines(t, phi$phi_4, col='red')
abline(a=0, b=1, lty='dashed', col='grey')
abline(a=1, b=-1, lty='dashed', col='grey')