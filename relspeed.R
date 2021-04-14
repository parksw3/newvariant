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

Rwvec <- c(1/rho^{3/2}, 1/rho, 1/sqrt(rho), 1, sqrt(rho))
kappavec <- c(0, 0.2, 0.5, 1)

Gratio <- exp(seq(log(1/1.5), log(1.5), length.out=31))

pardata <- expand.grid(kappavec, Rwvec, Gratio)

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
    kappa=factor(kappa, levels=c(0, 0.2, 0.5, 1)),
    Rw=factor(Rw, levels=c(Rwvec[3:5], Rwvec[1:2]),
              labels=c("$\\mathcal{R}_{\\textrm{wt}} < 1 < \\mathcal{R}_{\\textrm{var}}$",
                       "$1 = \\mathcal{R}_{\\textrm{wt}} < \\mathcal{R}_{\\textrm{var}}$",
                       "$1 < \\mathcal{R}_{\\textrm{wt}} < \\mathcal{R}_{\\textrm{var}}$",
                       "$\\mathcal{R}_{\\textrm{wt}} < \\mathcal{R}_{\\textrm{var}} < 1$",
                       "$\\mathcal{R}_{\\textrm{wt}} < \\mathcal{R}_{\\textrm{var}} = 1$"))
  )

deltadata %>%
  filter(Gratio==1, kappa==0.5)

kappa_title1 <- data.frame(
  x=c(1.3, 1.3),
  y=c(0.06, 0.12),
  labels=c("$\\kappa=1$", "$\\kappa=0$"),
  Rw="$\\mathcal{R}_{\\textrm{wt}} < \\mathcal{R}_{\\textrm{var}} = 1$"
)

kappa_title2 <- data.frame(
  x=c(1.3, 1.3),
  y=c(0.04, 0.15),
  labels=c("$\\kappa=0$", "$\\kappa=1$"),
  Rw="$1 < \\mathcal{R}_{\\textrm{wt}} < \\mathcal{R}_{\\textrm{var}}$"
)

g1 <- ggplot(deltadata) +
  geom_line(aes(Gratio, delta, lty=kappa, col=kappa), lwd=2) +
  geom_text(data=kappa_title1, aes(x, y, label=labels), col=(colorblind_pal()(4))[c(4, 1)], size=5) +
  geom_text(data=kappa_title2, aes(x, y, label=labels), col=(colorblind_pal()(4))[c(1, 4)], size=5) +
  scale_x_log10("Relative mean generation interval, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(2/3, 1, 3/2),
                labels=c("2/3", 1, "3/2"),
                expand=c(0, 0.03)) +
  scale_y_continuous("Relative speed, $\\delta$ (1/day)", limits=c(0, 0.3), expand=c(0, 0),
                     breaks=c(0, 0.1, 0.2, 0.3),
                     labels=c(0, 0.1, 0.2, 0.3)) +
  facet_wrap(~Rw, as.table=F) +
  scale_color_colorblind("$\\kappa$") +
  scale_linetype_discrete("$\\kappa$") +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.position = c(0.8, 0.8)
  )

tikz(file = "relspeed.tex", width = 10, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('relspeed.tex', pdf = T, clean = T)
