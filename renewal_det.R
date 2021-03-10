renewal_det <- function(Rfun=function(t) 2.5,
                        theta=1.5,
                        N=40000,
                        dt=0.025,
                        genfun1=function(x) dgamma(x, 5, 1),
                        genfun2=function(x) dgamma(x, 5, 1),
                        genfun3=function(x) dgamma(x, 5, 1),
                        I01=0.1,
                        I02=0.001,
                        tmax=100,
                        genmax=2000) {
  gen1 <- genfun1(0:genmax*dt+dt)
  gen1 <- gen1/sum(gen1)
  gen2 <- genfun2(0:genmax*dt+dt)
  gen2 <- gen2/sum(gen2)
  gen3 <- genfun3(0:genmax*dt+dt)
  gen3 <- gen3/sum(gen3)
  tvec <- seq(0, tmax, by=dt)
  Ivec1 <- rep(0, tmax/dt)
  Ivec2 <- rep(0, tmax/dt)
  
  Ivec1[1] <- I01
  Ivec2[1] <- I02
  Svec <- rep(0, tmax/dt)
  Svec[1] <- N - I01 - I02
  
  for (i in 2:length(tvec)) {
    Ivec1[i] <- Svec[i-1] * Rfun(tvec[i]) * sum(Ivec1[max(1, i-genmax):(i-1)] * gen1[min(i, genmax+1):2])/N
    Ivec2[i] <- Svec[i-1] * theta * Rfun(tvec[i]) * sum(Ivec2[max(1, i-genmax):(i-1)] * gen2[min(i, genmax+1):2])/N
    Svec[i] <- Svec[i-1] - Ivec1[i] - Ivec2[i]
  }
  
  Rt1 <- tail(sapply(tvec, Rfun), -1) * head(Svec, -1)/N
  Rt2 <- tail(sapply(tvec, Rfun), -1) * head(Svec, -1)/N * theta
  
  Rtest1 <- tail(Ivec1, -1)/sapply(2:(length(Ivec1)), function(x) sum(Ivec1[max(1, x-genmax):(x-1)] * gen3[min(x, genmax+1):2]))
  Rtest2 <- tail(Ivec2, -1)/sapply(2:(length(Ivec2)), function(x) sum(Ivec2[max(1, x-genmax):(x-1)] * gen3[min(x, genmax+1):2]))
  
  data.frame(
    tvec=tvec[-1],
    Ivec1=Ivec1[-1]/dt,
    Ivec2=Ivec2[-1]/dt,
    Svec=Svec[-1],
    Rt1=Rt1,
    Rt2=Rt2,
    Rtest1=Rtest1,
    Rtest2=Rtest2
  )
}
