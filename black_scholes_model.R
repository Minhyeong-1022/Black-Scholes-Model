library(plot3D)

s.v = seq(1,100,1)
sig.v = seq(0.01,1,0.01)
k = 50
r = 0.01
t = 0.25

rpt = 100
Premium.c <-  matrix(rep(0,rpt*rpt), nrow = rpt) 
Premium.p <-  matrix(rep(0,rpt*rpt), nrow = rpt) 

for (i in 1:100) {
  s = s.v[i]
  for (j in 1:100) {
    sig = sig.v[j]
    d1 = (log(s/k)+(r+sig^2/2)*t)/sig*sqrt(t)
    d2 = (log(s/k)+(r-sig^2/2)*t)/sig*sqrt(t)
    
    n.d1 = pnorm(d1, mean=0, sd=1)
    n.d2 = pnorm(d2, mean=0, sd=1)
    
    n.nd1 = pnorm(-d1, mean=0, sd=1)
    n.nd2 = pnorm(-d2, mean=0, sd=1)
    
    pv = k*exp(-r*t)
    c = s*n.d1 - pv*n.d2
    p = pv*n.nd2 - s*n.nd1
    Premium.c[i,j] = c
    Premium.p[i,j] = p
}}

persp3D(sig.v, s.v, Premium.c, 
        border = 'black',
        facets = TRUE,  
        colkey = TRUE,  
        bty = 'b2',     
        ticktype = 'detailed', theta = 0, main = "Call option Premium")

persp3D(sig.v, s.v, Premium.p, 
        border = 'black',
        facets = TRUE,  
        colkey = TRUE,  
        bty = 'b2',     
        ticktype = 'detailed', theta = 0, main = "Put option Premium")


