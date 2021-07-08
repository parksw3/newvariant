library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)
source("renewal_det.R")

delta <- 0.1

rw <- 0

kappa <- 0.2

Gw <- 5

Gratio <- c(1/1.5, 1, 1.5)

Rv_base <- ((1 + kappa * (rw + delta) * Gw)^(1/kappa))
target <- 0.9

tmax <- 80

constant_strength <- Rv_base/target

constant_speed <- optim(0.1,
                        function(r) {
                          (integrate(function(y) Rv_base * dgamma(y, shape=1/kappa, rate=1/kappa/Gw) * exp(-r * y),
                                     0,
                                     Inf)[[1]] - target)^2
                        },
                        lower=0,
                        upper=0.2,
                        method="Brent")[[1]]

thetavec <- ((1 + kappa * (rw + delta) * Gw*Gratio)^(1/kappa))

genfun_short <- function(x) dgamma(x, 5, 5/(Gw*Gratio[1]))
genfun_medium <- function(x) dgamma(x, 5, 5/(Gw*Gratio[2]))
genfun_long <- function(x) dgamma(x, 5, 5/(Gw*Gratio[3]))

Rfun <- function(t) {
  if (t < 30) {
    return(1) 
  } else {
    return(1/constant_strength)
  }
}
  
hfun <- function(t) {
  if (t < 30) {
    return(0) 
  } else {
    return(constant_speed)
  }
}

simstrength1 <- renewal_det(
  Rfun=Rfun,
  theta=thetavec[1],
  genfun1=genfun_medium,
  genfun2=genfun_short,
  tmax=tmax
) %>%
  mutate(
    type="Shorter"
  )

simstrength2 <- renewal_det(
  Rfun=Rfun,
  theta=thetavec[2],
  genfun1=genfun_medium,
  genfun2=genfun_medium,
  tmax=tmax
) %>%
  mutate(
    type="Equal"
  )

simstrength3 <- renewal_det(
  Rfun=Rfun,
  theta=thetavec[3],
  genfun1=genfun_medium,
  genfun2=genfun_long,
  tmax=tmax
) %>%
  mutate(
    type="Longer"
  )

simspeed1 <- renewal_det_speed(
  R=1,
  hfun=hfun,
  theta=thetavec[1],
  genfun1=genfun_medium,
  genfun2=genfun_short,
  tmax=tmax
) %>%
  mutate(
    type="Shorter"
  )

simspeed2 <- renewal_det_speed(
  R=1,
  hfun=hfun,
  theta=thetavec[2],
  genfun1=genfun_medium,
  genfun2=genfun_medium,
  tmax=tmax
) %>%
  mutate(
    type="Equal"
  )

simspeed3 <- renewal_det_speed(
  R=1,
  hfun=hfun,
  theta=thetavec[3],
  genfun1=genfun_medium,
  genfun2=genfun_long,
  tmax=tmax
) %>%
  mutate(
    type="Longer"
  )

simstrengthall <- bind_rows(
  simstrength1,
  simstrength2,
  simstrength3
) %>%
  mutate(
    type=factor(type, levels=c("Longer", "Equal", "Shorter")),
    prop=Ivec2/(Ivec1+Ivec2)
  )

simspeedall <- bind_rows(
  simspeed1,
  simspeed2,
  simspeed3
) %>%
  mutate(
    type=factor(type, levels=c("Longer", "Equal", "Shorter")),
    prop=Ivec2/(Ivec1+Ivec2)
  )

g1 <- ggplot(simstrengthall) +
  geom_line(aes(tvec, Ivec1, lty=type, col=type), col="gray", lty=1, lwd=3) +
  geom_line(aes(tvec, Ivec2, lty=type, col=type), lwd=3) +
  annotate("text", x=1, y=0.05, label="$i_{\\textrm{wt}}(t)$", col="gray", hjust=0) +
  annotate("text", x=1, y=0.5e-3, label="$i_{\\textrm{var}}(t)$", col="black", hjust=0) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_log10("Daily incidence", limits=c(1e-6, 0.11), expand=c(0, 0)) +
  scale_color_viridis_d("Variant GI", end=0.9) +
  scale_linetype_manual("Variant GI", values=1:3) +
  ggtitle("A. Constant strength") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.5, 0.3),
    legend.background = element_rect(fill=NA),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

g2 <- ggplot(simstrengthall) +
  geom_line(aes(tvec, Rt2, lty=type, col=type), lwd=3) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_log10("Reproduction number, $\\mathcal{R}_{\\textrm{var}}(t)$", limits=c(0.5, 2.5), expand=c(0, 0)) +
  scale_color_viridis_d("Variant GI", end=0.9) +
  scale_linetype_manual("Variant GI", values=1:3) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

g3 <- ggplot(simstrengthall) +
  geom_line(aes(tvec, prop, lty=type, col=type), lwd=3) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_continuous("Proportion new variant", limits=c(0.01, 0.99), expand=c(0, 0),
                     breaks=c(0.01, 0.1, 0.5, 0.9, 0.99),
                     trans="logit") +
  scale_color_viridis_d("Variant GI", end=0.9) +
  scale_linetype_manual("Variant GI", values=1:3) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

g4 <- ggplot(simspeedall) +
  geom_line(aes(tvec, Ivec1, lty=type, col=type), col="gray", lty=1, lwd=3) +
  geom_line(aes(tvec, Ivec2, lty=type, col=type), lwd=3) +
  annotate("text", x=1, y=0.05, label="$i_{\\textrm{wt}}(t)$", col="gray", hjust=0) +
  annotate("text", x=1, y=0.5e-3, label="$i_{\\textrm{var}}(t)$", col="black", hjust=0) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_log10("Daily incidence", limits=c(1e-6, 0.11), expand=c(0, 0)) +
  scale_color_viridis_d("Variant GI", end=0.9) +
  scale_linetype_manual("Variant GI", values=1:3) +
  ggtitle("D. Constant speed") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g5 <- ggplot(simspeedall) +
  geom_line(aes(tvec, Rt2, lty=type, col=type), lwd=3) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_log10("Reproduction number, $\\mathcal{R}_{\\textrm{var}}(t)$", limits=c(0.5, 2.5), expand=c(0, 0)) +
  scale_color_viridis_d("Variant GI", end=0.9) +
  scale_linetype_manual("Variant GI", values=1:3) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g6 <- ggplot(simspeedall) +
  geom_line(aes(tvec, prop, lty=type, col=type), lwd=3) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_continuous("Proportion new variant", limits=c(0.01, 0.99), expand=c(0, 0),
                     breaks=c(0.01, 0.1, 0.5, 0.9, 0.99),
                     trans="logit") +
  scale_color_viridis_d("Variant GI", end=0.9) +
  scale_linetype_manual("Variant GI", values=1:3) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gtot <- egg::ggarrange(g1, g2, g3, 
                  g4, g5, g6,
                  nrow=2,
                  draw=FALSE)

tikz(file = "control_sim.tex", width = 10, height = 6, standAlone = T)
gtot
dev.off()
tools::texi2dvi('control_sim.tex', pdf = T, clean = T)
