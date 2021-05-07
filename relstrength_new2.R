library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=18, base_family = "Times"))
library(ggthemes)
library(egg)
library(directlabels)
library(tikzDevice)

deltavec <- c(0.1)

rwvec <- seq(-0.15, 0.05, length.out=31)
kappavec <- c(0, 0.2, 1)

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

textdata1 <- data.frame(
  x=0.05,
  y=1.71,
  Gratio2="$\\bar{G}_{\\textrm{var}} = \\bar{G}_{\\textrm{wt}}$",
  kappa=c("$\\kappa=0$")
)

textdata2 <- data.frame(
  x=0.05,
  y=2.5,
  Gratio2="$\\bar{G}_{\\textrm{var}} = 3/2 \\bar{G}_{\\textrm{wt}}$",
  kappa=c("$\\kappa=0$")
)

textdata3 <- data.frame(
  x=0.05,
  y=1.34,
  Gratio2="$\\bar{G}_{\\textrm{var}} = 2/3 \\bar{G}_{\\textrm{wt}}$",
  kappa=c("$\\kappa=0$")
)

arrow_right <- rhodata %>%
  filter(rw==0.05, Gratio != 1) %>%
  select(rw, rho, Gratio, kappa) %>%
  group_by(kappa) %>%
  mutate(
    Gratio=ifelse(Gratio < 1, "lower", "upper")
  ) %>%
  spread(Gratio, rho)

arrow_left <- rhodata %>%
  filter(rw==-0.15, Gratio != 1) %>%
  select(rw, rho, Gratio, kappa) %>%
  group_by(kappa) %>%
  mutate(
    Gratio=ifelse(Gratio < 1, "lower", "upper")
  ) %>%
  spread(Gratio, rho)

earlytext <- rhodata %>%
  filter(rw==-0.15, Gratio == 1, kappa=="$\\kappa=0$") %>%
  mutate(
    label1="More early transmission"
  )

g1 <- ggplot(rhodata) +
  geom_line(aes(rw, rho, col=Gratio2, lty=Gratio2), lwd=2) +
  geom_text(data=textdata1, aes(x, y, label=Gratio2, col=Gratio2), hjust=1) +
  geom_text(data=textdata2, aes(x, y, label=Gratio2, col=Gratio2), hjust=1, angle=29) +
  geom_text(data=textdata3, aes(x, y, label=Gratio2, col=Gratio2), hjust=1, angle=-20) +
  geom_vline(xintercept=-deltavec, lty=2) +
  annotate("text", x=-0.108, y=3.5, label="$r_{\\textrm{var}} = 0$", angle=90) +
  geom_segment(data=arrow_right, aes(x=rw+0.01, xend=rw+0.01, y=upper, yend=lower),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(data=arrow_left, aes(x=rw-0.01, xend=rw-0.01, y=upper, yend=lower),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data=earlytext, aes(rw-0.02, rho, label=label1), angle=90) +
  facet_wrap(~kappa, scale="free_x") +
  scale_x_continuous("Wild type speed, $r_{\\textrm{wt}}$ (1/day)", breaks=c(-0.15, -0.1, -0.05, 0, 0.05)) +
  scale_y_log10("Relative strength, $\\rho$", breaks=c(1, 2, 4), limits=c(1, 4)) +
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

tikz(file = "relstrength_new2.tex", width = 12, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('relstrength_new2.tex', pdf = T, clean = T)
