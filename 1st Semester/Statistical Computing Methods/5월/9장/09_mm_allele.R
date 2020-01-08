rm(list = ls())
set.seed(1)

n <- 521
na <- 186
nb <- 38
nab <- 13
no  <- 284

max.iter <- 100
eps <- 1.0e-5

# initialization
p.new <- p <- rep(1/3, 3)

for (m in 1:max.iter) {
   pa <- p[1] # P(A)
   pb <- p[2] # P(B)
   po <- p[3] # P(O)

   naa <- na * pa^2/(pa^2 + 2 * pa * po)  
   nao <- na * 2 * pa * po /(pa^2 + 2 * pa * po)  
   nbb <- nb * pb^2/(pb^2 + 2 * pb * po)  
   nbo <- nb * 2 * pb * po /(pb^2 + 2 * pb * po)  

   p.new[1] <- (2 * naa + nao + nab)/(2*n)
   p.new[2] <- (2 * nbb + nbo + nab)/(2*n)
   p.new[3] <- (nao + nbo + 2 * no)/(2*n)

   if (max(abs(c(p - p.new))) < eps) break
   p <- p.new
}

print(p)