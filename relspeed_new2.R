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
kappavec <- c(0, 0.2, 1)

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

textdata1 <- data.frame(
  x=sqrt(rho),
  y=0.105,
  Gratio2="$\\bar{G}_{\\textrm{var}} = \\bar{G}_{\\textrm{wt}}$",
  kappa=c("$\\kappa=0$")
)

textdata2 <- data.frame(
  x=sqrt(rho),
  y=0.056,
  Gratio2="$\\bar{G}_{\\textrm{var}} = 3/2 \\bar{G}_{\\textrm{wt}}$",
  kappa=c("$\\kappa=0$")
)

textdata3 <- data.frame(
  x=sqrt(rho),
  y=0.175,
  Gratio2="$\\bar{G}_{\\textrm{var}} = 2/3 \\bar{G}_{\\textrm{wt}}$",
  kappa=c("$\\kappa=0$")
)

arrow_right <- deltadata %>%
  filter(Rw==sqrt(rho), Gratio != 1) %>%
  select(Rw, delta, Gratio, kappa) %>%
  group_by(kappa) %>%
  mutate(
    Gratio=ifelse(Gratio < 1, "lower", "upper")
  ) %>%
  spread(Gratio, delta)

arrow_left <- deltadata %>%
  filter(Rw==1/rho^{3/2}, Gratio != 1) %>%
  select(Rw, delta, Gratio, kappa) %>%
  group_by(kappa) %>%
  mutate(
    Gratio=ifelse(Gratio < 1, "lower", "upper")
  ) %>%
  spread(Gratio, delta)

earlytext <- deltadata %>%
  filter(Rw==1/rho^{3/2}, Gratio == 1, kappa=="$\\kappa=0$") %>%
  mutate(
    label1="More early transmission"
  )

g1 <- ggplot(deltadata) +
  geom_line(aes(Rw, delta, lty=Gratio2, col=Gratio2), lwd=2) +
  geom_text(data=textdata1, aes(x, y, label=Gratio2, col=Gratio2), hjust=1) +
  geom_text(data=textdata2, aes(x, y, label=Gratio2, col=Gratio2), hjust=1, angle=-18) +
  geom_text(data=textdata3, aes(x, y, label=Gratio2, col=Gratio2), hjust=1, angle=27) +
  geom_vline(xintercept=1/rho, lty=2) +
  annotate("text", x=1/rho/1.05, y=0.25, label="$\\mathcal{R}_{\\textrm{var}} = 1$", angle=90) +
  geom_segment(data=arrow_right, aes(x=Rw*1.05, xend=Rw*1.05, y=upper, yend=lower),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(data=arrow_left, aes(x=Rw/1.05, xend=Rw/1.05, y=upper, yend=lower),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data=earlytext, aes(Rw/1.05^2, delta, label=label1), angle=90) +
  scale_x_log10("Wild type strength, $\\mathcal{R}_{\\textrm{wt}}$",
                breaks=c(1/rho^{3/2}, 1/rho, 1/sqrt(rho), 1, sqrt(rho)),
                labels=c("$\\rho^{-3/2}$", "$\\rho^{-1}$", "$\\rho^{-1/2}$", "$1$", "$\\rho^{1/2}$")) +
  scale_y_continuous("Relative speed, $\\delta$ (1/day)", limits=c(0, 0.3), expand=c(0, 0),
                     breaks=c(0, 0.1, 0.2, 0.3),
                     labels=c(0, 0.1, 0.2, 0.3)) +
  facet_wrap(~kappa, scales="free_x") +
  scale_color_colorblind() +
  scale_linetype() +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(fill=NA),
    legend.key.size = unit(0.8, "cm")
  )

tikz(file = "relspeed_new2.tex", width = 12, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('relspeed_new2.tex', pdf = T, clean = T)
