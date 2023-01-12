
f_de_k <- f(k)
u_de_f <- u(f_de_k)



M = 10
Vseq = matrix(0, nrow=N, ncol=M)
Useq = matrix(0, nrow=N, ncol=M)
VU = VU_next(V_z)
Vseq[,1] = VU$V
Useq[,1] = VU$U
for (m in 2:M) {
  VU = VU_next(VU$V)
  Vseq[,m] = VU$V
  Useq[,m] = VU$U
}
