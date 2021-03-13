library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

delta <- 0.1

rwvec <- seq(-0.13, 0.13, length.out=31)
kappavec <- c(0, 0.2, 0.5, 1)
kappabase <- 0.2

pardata <- expand.grid(kappavec, rwvec)

Gw <- 5

Gratio <- seq(0.6, 1.4, length.out=31)

pardata2 <- expand.grid(rwvec, Gratio)

thetadata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  rw <- x[[2]]
  
  if (kappa==0) {
    thetahat <- exp(Gw * (rw + delta))/exp(Gw * rw)
  } else {
    thetahat <- ((1 + kappa * (rw + delta) * Gw)^(1/kappa))/
      ((1 + kappa * rw * Gw)^(1/kappa))
  }
  
  data.frame(
    delta=delta,
    rw=rw,
    thetahat=thetahat,
    kappa=kappa
  )
}) %>%
  bind_rows

thetadata2 <- apply(pardata2, 1, function(x) {
  rw <- x[[1]]
  Gratio <- x[[2]] ## Gv/Gw
  Gv <- Gratio * Gw
  
  theta <- ((1 + kappabase * (rw + delta) * Gv)^(1/kappabase))/
    ((1 + kappabase * rw * Gw)^(1/kappabase))
  
  thetahat <- ((1 + kappabase * (rw + delta) * Gw)^(1/kappabase))/
    ((1 + kappabase * rw * Gw)^(1/kappabase))
  
  data.frame(
    delta=delta,
    Gratio=Gratio,
    rw=rw,
    theta=theta,
    thetahat=thetahat,
    bias=theta/thetahat
  )
}) %>%
  bind_rows

g1 <- ggplot(thetadata) +
  geom_line(aes(rw, thetahat, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Wild type speed, $r_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("Estimated relative strength, $\\hat{\\theta}$", limits=c(1, 2.7), expand=c(0, 0)) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  ggtitle("A. Equal generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.75)
  )

g2 <- ggplot(thetadata2) +
  geom_tile(aes(Gratio, rw, fill=theta)) +
  geom_vline(xintercept=1, lty=2, col="white") +
  geom_hline(yintercept=-delta, lty=1, col="white") +
  annotate("text", x=1.04, y=0, label=c("$\\bar{G}_v=\\bar{G}_w$"), col="white", angle=-90) +
  annotate("text", x=0.8, y=-delta+0.01, label=c("$r_v=0$"), col="white") +
  scale_x_continuous("Generation interval ratio, $\\bar{G}_v/\\bar{G}_w$", expand=c(0, 0)) +
  scale_y_continuous("Wild type speed, $r_w$ (1/days)", expand=c(0, 0)) +
  scale_fill_viridis_c("$\\theta$") +
  ggtitle("B. Different generation intervals") +
  theme(
    panel.grid = element_blank()
  )
  
g3 <- ggplot(thetadata2) +
  geom_tile(aes(Gratio, rw, fill=bias)) +
  geom_vline(xintercept=1, lty=2, col="white") +
  geom_hline(yintercept=-delta, lty=1, col="white") +
  annotate("text", x=1.04, y=0, label=c("$\\bar{G}_v=\\bar{G}_w$"), col="white", angle=-90) +
  annotate("text", x=0.8, y=-delta+0.01, label=c("$r_v=0$"), col="white") +
  scale_x_continuous("Generation interval ratio, $\\bar{G}_v/\\bar{G}_w$", expand=c(0, 0)) +
  scale_y_continuous("Wild type speed, $r_w$ (1/days)", expand=c(0, 0)) +
  scale_fill_viridis_c("$\\theta/\\hat{\\theta}$", option="A") +
  ggtitle("C. Bias") +
  theme(
    panel.grid = element_blank(),
    plot.margin = unit(c(0,-2.5,0,0), "cm")
    
  )

gtot <- ggarrange(g1, g2, g3, nrow=1, draw=FALSE)

tikz(file = "relstrength.tex", width = 12, height = 4, standAlone = T)
gtot
dev.off()
tools::texi2dvi('relstrength.tex', pdf = T, clean = T)
