library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=18, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

rw <- 0
delta <- 0.1
kappa_base <- 1/5
Gw <- 5

rho <- ((1 + kappa_base * (rw+delta) * Gw)^(1/kappa_base))

Rwvec <- exp(seq(log(1/rho^{3/2}), log(sqrt(rho)), length.out=31))
kappavec <- c(0, 1)

Gratiovec <- c(1/1.5, 1, 1.5)

pardata <- expand.grid(kappavec, Rwvec, Gratiovec)

deltadata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  Rw <- x[[2]]
  Gratio <- x[[3]]
  Gv <- Gratio * Gw
  
  if (kappa == 0) {
    delta <- log(rho * Rw)/Gv - log(Rw)/Gw
  } else {
    delta <- ((rho * Rw)^kappa - 1)/(kappa*Gv) -  ((Rw)^kappa - 1)/(kappa*Gw)
  }
  
  data.frame(
    rho=rho,
    Rw=Rw,
    delta=delta,
    kappa=kappa,
    Gratio=Gratio
  )
}) %>%
  bind_rows %>%
  mutate(
    kappa=factor(kappa, levels=c(kappavec),
                 labels=paste0("$\\kappa=", kappavec, "$")),
    Gratio2=factor(Gratio, levels=c(Gratiovec),
                   labels=c("$\\bar{G}_{\\textrm{var}} = 2/3 \\bar{G}_{\\textrm{wt}}$",
                            "$\\bar{G}_{\\textrm{var}} = \\bar{G}_{\\textrm{wt}}$",
                            "$\\bar{G}_{\\textrm{var}} = 3/2 \\bar{G}_{\\textrm{wt}}$"))
  )

g1 <- ggplot(deltadata) +
  geom_line(aes(Rw, delta, lty=Gratio2, col=Gratio2), lwd=2)  +
  scale_x_log10("Wild type strength, $\\mathcal{R}_{\\textrm{wt}}$",
                breaks=c(2/3, 1, 3/2),
                labels=c("2/3", 1, "3/2"),
                expand=c(0, 0.03)) +
  scale_y_continuous("Relative speed, $\\delta$ (1/days)", limits=c(0, 0.3), expand=c(0, 0),
                     breaks=c(0, 0.1, 0.2, 0.3),
                     labels=c(0, 0.1, 0.2, 0.3)) +
  facet_wrap(~kappa) +
  scale_color_colorblind() +
  scale_linetype() +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.88),
    legend.background = element_rect(fill=NA),
    legend.key.size = unit(0.8, "cm")
  )

tikz(file = "relspeed_new.tex", width = 8, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('relspeed_new.tex', pdf = T, clean = T)
