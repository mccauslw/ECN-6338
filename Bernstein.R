library(pracma)
x = seq(0, 1, by=0.001); n=10
plot(NULL, xlim=c(0,1), ylim=c(0,1))
for(i in 0:n) {
  lines(x, bernsteinb(i, n, x))
}
