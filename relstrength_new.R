library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=18, base_family = "Times"))
library(ggthemes)
library(egg)
library(directlabels)
library(tikzDevice)

deltavec <- c(0.1)

rwvec <- seq(-0.15, 0.05, length.out=31)
kappavec <- c(0, 1)

Gw <- 5

Gratiovec <- c(1/1.5, 1, 1.5)

pardata <- expand.grid(kappavec, rwvec, Gratiovec, deltavec)

rhodata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  rw <- x[[2]]
  Gratio <- x[[3]]
  delta <- x[[4]]
  
  Gv <- Gratio * Gw
  
  if (kappa==0) {
    rho <- exp(Gv * (rw + delta))/exp(Gw * rw)
  } else {
    rho <- ((1 + kappa * (rw + delta) * Gv)^(1/kappa))/
      ((1 + kappa * rw * Gw)^(1/kappa))
  }
  
  data.frame(
    delta=delta,
    rw=rw,
    rho=rho,
    Gratio=Gratio,
    kappa=kappa
  )
}) %>%
  bind_rows %>%
  mutate(
    kappa=factor(kappa, levels=c(kappavec),
                 labels=paste0("$\\kappa=", kappavec, "$")),
    Gratio2=factor(Gratio, levels=c(Gratiovec),
                   labels=c("$\\bar{G}_{\\textrm{var}} = 2/3 \\bar{G}_{\\textrm{wt}}$",
                            "$\\bar{G}_{\\textrm{var}} = \\bar{G}_{\\textrm{wt}}$",
                            "$\\bar{G}_{\\textrm{var}} = 3/2 \\bar{G}_{\\textrm{wt}}$")),
    delta2=factor(delta, levels=c(deltavec),
                  labels=paste0("$\\delta=", deltavec, "/\\textrm{day}$"))
  )

g1 <- ggplot(rhodata) +
  geom_line(aes(rw, rho, col=Gratio2, lty=Gratio2), lwd=2) +
  facet_grid(~kappa) +
  scale_x_continuous("Wild type speed, $r_{\\textrm{wt}}$ (1/day)", breaks=c(-0.1, 0)) +
  scale_y_log10("Relative strength, $\\rho$", breaks=c(1, 2, 4), limits=c(1, 4)) +
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

tikz(file = "relstrength_new.tex", width = 8, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('relstrength_new.tex', pdf = T, clean = T)
