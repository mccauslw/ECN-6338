x <- seq(-3, 3, length=601)
T_3 <- 1 + x + x^2/2 + x^3/6 
T_4 <- T_3 + x^4/24
T_5 <- T_4 + x^5/120
Pade_2_1 = (1 + (2/3) * x + (1/6) * x^2) / (1 - (1/3)*x)
plot(x, exp(x), 'l')
lines(x, T_3, col='red')
lines(x, T_4, col='red')
lines(x, T_5, col='red')
lines(x, Pade_2_1, col='green')
