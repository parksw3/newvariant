library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

delta <- 0.1

rwvec <- seq(-0.13, 0.13, length.out=21)
kappavec <- c(0, 0.2, 0.5, 1)

pardata <- expand.grid(kappavec, rwvec)

Gw <- 5
Gv <- 7

thetadata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  rw <- x[[2]]
  
  if (kappa==0) {
    
    thetahat <- exp(Gv * (rw + delta))/exp(Gw * rw)
    
    theta <- exp(Gw * (rw + delta))/exp(Gw * rw)
  } else {
    thetahat <- ((1 + kappa * (rw + delta) * Gv)^(1/kappa))/
      ((1 + kappa * rw * Gw)^(1/kappa))
    
    theta <- ((1 + kappa * (rw + delta) * Gw)^(1/kappa))/
      ((1 + kappa * rw * Gw)^(1/kappa))
  }
  
  data.frame(
    delta=delta,
    rw=rw,
    theta=theta,
    thetahat=thetahat,
    bias=thetahat/theta,
    kappa=kappa
  )
}) %>%
  bind_rows

g1 <- ggplot(thetadata) +
  geom_line(aes(rw, theta, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Relative speed, $r_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("Relative strength, $\\theta$", limits=c(1, 2.7), expand=c(0, 0)) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  ggtitle("A. Equal generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.75)
  )

g2 <- ggplot(thetadata) +
  geom_line(aes(rw, thetahat, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Relative speed, $r_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("Relative strength, $\\hat{\\theta}$", limits=c(1, 2.7), expand=c(0, 0)) +
  scale_color_colorblind() +
  ggtitle("B. Longer generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(thetadata) +
  geom_line(aes(rw, bias, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Relative speed, $r_w$ (1/days)", expand=c(0, 0)) +
  scale_y_continuous("Changes in estimates, $\\hat{\\theta}/\\theta$", limits=c(0.9, 1.61), expand=c(0, 0)) +
  scale_color_colorblind() +
  ggtitle("C. Bias") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

gtot <- ggarrange(g1, g2, g3, nrow=1, draw=FALSE)

tikz(file = "relstrength.tex", width = 12, height = 4, standAlone = T)
gtot
dev.off()
tools::texi2dvi('relstrength.tex', pdf = T, clean = T)
