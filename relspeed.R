library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

theta <- 1.5

Rwvec <- seq(0.3, 1.7, length.out=31)
kappavec <- c(0, 0.2, 0.5, 1)
kappabase <- 0.2

pardata <- expand.grid(kappavec, Rwvec)

Gw <- 5

Gratio <- seq(0.6, 1.4, length.out=31)

pardata2 <- expand.grid(Rwvec, Gratio)

deltadata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  Rw <- x[[2]]
  
  if (kappa == 0) {
    deltahat <- log(theta * Rw)/Gw - log(Rw)/Gw
  } else {
    deltahat <- ((theta * Rw)^kappa - 1)/(kappa*Gw) -  ((Rw)^kappa - 1)/(kappa*Gw)
  }
  
  data.frame(
    theta=theta,
    Rw=Rw,
    deltahat=deltahat,
    kappa=kappa
  )
}) %>%
  bind_rows

deltadata2 <- apply(pardata2, 1, function(x) {
  Rw <- x[[1]]
  Gratio <- x[[2]] ## Gv/Gw
  Gv <- Gratio * Gw
  
  delta <- ((theta * Rw)^kappabase - 1)/(kappabase*Gv) -  ((Rw)^kappabase - 1)/(kappabase*Gw)
  
  deltahat <- ((theta * Rw)^kappabase - 1)/(kappabase*Gw) -  ((Rw)^kappabase - 1)/(kappabase*Gw)
  
  data.frame(
    theta=theta,
    Gratio=Gratio,
    Rw=Rw,
    delta=delta,
    deltahat=deltahat,
    bias=delta-deltahat
  )
}) %>%
  bind_rows

g1 <- ggplot(deltadata) +
  geom_line(aes(Rw, deltahat, lty=as.factor(kappa), col=as.factor(kappa))) +
  scale_x_continuous("Wild type strength, $\\mathcal{R}_w$", expand=c(0, 0)) +
  scale_y_continuous("Estimated relative speed, $\\hat{\\delta}$ (1/days)", limits=c(0, 0.33), expand=c(0, 0)) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  ggtitle("A. Equal generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.2, 0.75)
  )

g2 <- ggplot(deltadata2) +
  geom_tile(aes(Gratio, Rw, fill=delta)) +
  geom_vline(xintercept=1, lty=2, col="white") +
  geom_hline(yintercept=1/theta, lty=1, col="white") +
  annotate("text", x=1.04, y=1.1, label=c("$\\bar{G}_v=\\bar{G}_w$"), col="white", angle=-90) +
  annotate("text", x=0.8, y=1/theta+0.05, label=c("$\\mathcal{R}_v=1$"), col="white") +
  scale_x_continuous("Generation interval ratio, $\\bar{G}_v/\\bar{G}_w$", expand=c(0, 0)) +
  scale_y_continuous("Wild type strength, $\\mathcal{R}_w$", expand=c(0, 0)) +
  scale_fill_viridis_c("$\\delta$") +
  ggtitle("B. Different generation intervals") +
  theme(
    panel.grid = element_blank()
  )

g3 <- ggplot(deltadata2) +
  geom_tile(aes(Gratio, Rw, fill=bias)) +
  geom_vline(xintercept=1, lty=2, col="white") +
  geom_hline(yintercept=1/theta, lty=1, col="white") +
  annotate("text", x=1.04, y=1.1, label=c("$\\bar{G}_v=\\bar{G}_w$"), col="white", angle=-90) +
  annotate("text", x=0.8, y=1/theta+0.05, label=c("$\\mathcal{R}_v=1$"), col="white") +
  scale_x_continuous("Generation interval ratio, $\\bar{G}_v/\\bar{G}_w$", expand=c(0, 0)) +
  scale_y_continuous("Wild type strength, $\\mathcal{R}_w$", expand=c(0, 0)) +
  scale_fill_viridis_c("$\\delta-\\hat{\\delta}$", option="A") +
  ggtitle("C. Bias") +
  theme(
    panel.grid = element_blank(),
    plot.margin = unit(c(0,-2.5,0,0), "cm")
  )

gtot <- ggarrange(g1, g2, g3, nrow=1, draw=FALSE)

tikz(file = "relspeed.tex", width = 12, height = 4, standAlone = T)
gtot
dev.off()
tools::texi2dvi('relspeed.tex', pdf = T, clean = T)
