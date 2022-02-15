x <- seq(0, 3, length=301)
T_3 <- (x-1) - (x-1)^2/2 + (x-1)^3/3
T_4 <- T_3 - (x-1)^4/4
T_5 <- T_4 + (x-1)^5/5
Pade_2_1 = (-2.5 + 2*x + 0.5*x^2) / (1 + 2*x)
plot(x, log(x), 'l')
lines(x, T_3, col='red')
lines(x, T_4, col='red')
lines(x, T_5, col='red')
lines(x, Pade_2_1, col='green')
