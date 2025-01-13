N <- c(24, 7, 9)
al <- c(0.5, 0.5, 0.5)
M = 10000

lnf_seq_multinom <- function(p, N_x) {
  sum(N_x[N_x != 0] * log(p[N_x != 0]))
}

p = rdirichlet(M, al+N)
lnf = apply(p, MARGIN=1, lnf_seq_multinom, N)
