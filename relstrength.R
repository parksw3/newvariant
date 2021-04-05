library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

delta <- 0.1

rwvec <- c(-0.15, -0.1, 0, 0.1)
kappavec <- c(0, 0.2, 0.5, 1)

Gw <- 5

Gratio <- exp(seq(log(1/1.5), log(1.5), length.out=31))

pardata <- expand.grid(kappavec, rwvec, Gratio)

rhodata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  rw <- x[[2]]
  Gratio <- x[[3]]
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
    kappa=factor(kappa, levels=c(0, 0.2, 0.5, 1)),
    rw=factor(rw, levels=rwvec,
              labels=c("$r_{\\textrm{wt}} < r_{\\textrm{var}} < 0$",
                       "$r_{\\textrm{wt}} < r_{\\textrm{var}} = 0$",
                       "$0 = r_{\\textrm{wt}} < r_{\\textrm{var}}$",
                       "$0 < r_{\\textrm{wt}} < r_{\\textrm{var}}$"))
  )

range(filter(rhodata, !kappa %in% c(0, 1))$rho)

g1 <- ggplot(rhodata) +
  geom_line(aes(Gratio, rho, lty=kappa, col=kappa)) +
  scale_x_log10("Relative mean generation interval, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(0.67, 1, 1.5)) +
  scale_y_log10("Relative strength, $\\rho$", breaks=c(1, 2, 4), limits=c(1, 4)) +
  facet_wrap(~rw) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  theme(
    panel.grid = element_blank()
  )

tikz(file = "relstrength.tex", width = 6*1.2, height = 4*1.2, standAlone = T)
g1
dev.off()
tools::texi2dvi('relstrength.tex', pdf = T, clean = T)
