library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

theta <- 1.8

Rwvec <- seq(0.3, 1.7, length.out=21)
kappavec <- c(0, 0.2, 0.5, 1)

pardata <- expand.grid(kappavec, Rwvec)

Gw <- 5
Gv <- 7

deltadata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  Rw <- x[[2]]
  
  if (kappa == 0) {
    delta <- log(theta * Rw)/Gv - log(Rw)/Gw
    
    deltahat <- log(theta * Rw)/Gw - log(Rw)/Gw
  } else {
    delta <- ((theta * Rw)^kappa - 1)/(kappa*Gv) -  ((Rw)^kappa - 1)/(kappa*Gw)
    
    deltahat <- ((theta * Rw)^kappa - 1)/(kappa*Gw) -  ((Rw)^kappa - 1)/(kappa*Gw)
    
  }
  
  data.frame(
    theta=theta,
    Rw=Rw,
    delta=delta,
    deltahat=deltahat,
    bias=delta-deltahat,
    kappa=kappa
  )
}) %>%
  bind_rows

g1 <- ggplot(deltadata) +
  geom_line(aes(Rw, deltahat, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Wild type strength, $\\mathcal{R}_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("Estimated relative speed, $\\hat{\\delta}$ (1/days)", limits=c(0, 0.33), expand=c(0, 0)) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  ggtitle("A. Equal generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.2, 0.75)
  )

g2 <- ggplot(deltadata) +
  geom_line(aes(Rw, delta, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Wild type strength, $\\mathcal{R}_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("True relative speed, $\\delta$ (1/days)", limits=c(0, 0.33), expand=c(0, 0)) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  ggtitle("B. Longer generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(deltadata) +
  geom_line(aes(Rw, bias, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Wild type strength, $\\mathcal{R}_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("Changes in estimates, $\\delta-\\hat{\\delta}$ (1/days)", limits=c(-0.13, 0.05), expand=c(0, 0)) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  ggtitle("C. Bias") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

gtot <- ggarrange(g1, g2, g3, nrow=1, draw=FALSE)

tikz(file = "relspeed.tex", width = 12, height = 4, standAlone = T)
gtot
dev.off()
tools::texi2dvi('relspeed.tex', pdf = T, clean = T)
