library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=18, base_family = "Times"))
library(ggthemes)
library(egg)
library(tikzDevice)

delta <- 0.1

rw <- 0

kappa <- 0.2

Gw <- 5

Gratio <- exp(seq(log(1/1.5), log(1.5), length.out=31))

Rv_base <- ((1 + kappa * (rw + delta) * Gw)^(1/kappa))
target <- 0.9

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

intervention <- sapply(Gratio, function(x) {
  Gr <- x
  Gv <- Gw * Gr
  
  Rv <- ((1 + kappa * (rw + delta) * Gv)^(1/kappa))
  
  rv <- rw + delta
  
  strength_post_strength <- Rv/constant_strength
  strength_post_speed <- ((strength_post_strength)^kappa-1)/kappa/Gv
  
  speed_post_strength <- integrate(function(y) Rv * dgamma(y, shape=1/kappa, rate=1/kappa/Gv) * exp(-constant_speed * y),
                                 0,
                                 Inf)[[1]]
  
  speed_post_speed <- optim(-0.05,
        function(r) {
          (integrate(function(y) Rv * dgamma(y, shape=1/kappa, rate=1/kappa/Gv) * exp(-constant_speed * y) * exp(-r * y),
                     0,
                     Inf)[[1]] - 1)^2
        },
        lower=-0.1,
        upper=0.1,
        method="Brent")[[1]]
  
  data.frame(
    Gratio=Gr,
    rw=rw,
    Rv=Rv,
    rv=rv,
    strength_post_strength=strength_post_strength,
    strength_post_speed=strength_post_speed,
    speed_post_strength=speed_post_strength,
    speed_post_speed=speed_post_speed
  )
}, simplify = FALSE) %>%
  bind_rows

gendata_base <- data_frame(
  tvec=seq(0, 21, by=0.1),
  density=Rv_base*dgamma(tvec, shape=1/kappa, rate=1/kappa/Gw),
  strength=density/constant_strength,
  speed=density*exp(-constant_speed*tvec)
)

Gpost_base <- integrate(function(x) x*Rv_base*dgamma(x, shape=1/kappa, rate=1/kappa/Gw)*exp(-constant_speed*x),
                        0,
                        Inf)[[1]]/target

gendata_long <- data_frame(
  tvec=seq(0, 21, by=0.1),
  density=filter(intervention, Gratio==tail(Gratio,1))$Rv*dgamma(tvec, shape=1/kappa, rate=1/kappa/(Gw*tail(Gratio,1))),
  strength=density/constant_strength,
  speed=density*exp(-constant_speed*tvec)
)

Gpost_long <- integrate(function(x) x*Rv_base*dgamma(x, shape=1/kappa, rate=1/kappa/(Gw*tail(Gratio,1)))*exp(-constant_speed*x),
                        0,
                        Inf)[[1]]/filter(intervention, Gratio==tail(Gratio,1))$speed_post_strength

g1 <- ggplot(gendata_base) +
  geom_line(aes(tvec, density, col="Pre-intervention", lty="Pre-intervention"), lwd=2) +
  geom_line(aes(tvec, strength, col="Post-intervention (constant-strength)", lty="Post-intervention (constant-strength)"), lwd=2) +
  geom_vline(xintercept = Gw, lwd=2) +
  geom_vline(xintercept = Gw, lwd=2, col=colorblind_pal()(3)[3], lty=3) +
  scale_x_continuous("Generation intervals (days)") +
  scale_y_continuous("Kernel density", limits=c(0, 0.4)) +
  scale_color_manual("", values=(colorblind_pal()(3)[c(3, 1)])) +
  scale_linetype_manual("", values=c(3, 1)) +
  ggtitle("A. Equal generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.6, 0.95),
    legend.background = element_rect(fill=NA)
  )

g2 <- ggplot(gendata_base) +
  geom_line(aes(tvec, density, col="Pre-intervention", lty="Pre-intervention"), lwd=2) +
  geom_line(aes(tvec, speed, col="Post-intervention (constant-strength)", lty="Post-intervention (constant-strength)"), lwd=2) +
  geom_vline(xintercept = Gw, lwd=2) +
  geom_vline(xintercept = Gpost_base, col=colorblind_pal()(3)[2], lwd=2, lty=2) +
  scale_x_continuous("Generation intervals (days)") +
  scale_y_continuous("Kernel density", limits=c(0, 0.4)) +
  scale_color_manual("", values=(colorblind_pal()(3)[c(2, 1)])) +
  scale_linetype_manual("", values=c(2, 1)) +
  ggtitle("B. Equal generation intervals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.6, 0.95),
    legend.background = element_rect(fill=NA)
  )

g3 <- ggplot(gendata_long) +
  geom_line(aes(tvec, density), lwd=2, col=colorblind_pal()(3)[1]) +
  geom_line(aes(tvec, strength), lwd=2, col=colorblind_pal()(3)[3], lty=3) +
  geom_vline(xintercept = (Gw*tail(Gratio,1)), lwd=2) +
  geom_vline(xintercept = (Gw*tail(Gratio,1)), lwd=2, col=colorblind_pal()(3)[3], lty=3) +
  scale_x_continuous("Generation intervals (days)") +
  scale_y_continuous("Kernel density", limits=c(0, 0.4)) +
  ggtitle("C. Longer generation intervals") +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(gendata_long) +
  geom_line(aes(tvec, density), lwd=2, col=colorblind_pal()(3)[1]) +
  geom_line(aes(tvec, speed), lwd=2, col=colorblind_pal()(3)[2], lty=2) +
  geom_vline(xintercept = (Gw*tail(Gratio,1)), lwd=2) +
  geom_vline(xintercept = Gpost_long, lwd=2, col=colorblind_pal()(3)[2], lty=2) +
  scale_x_continuous("Generation intervals (days)") +
  scale_y_continuous("Kernel density", limits=c(0, 0.4)) +
  ggtitle("D. Longer generation intervals") +
  theme(
    panel.grid = element_blank()
  )


g5 <- ggplot(intervention) +
  geom_hline(yintercept=1, col="gray") +
  geom_line(aes(Gratio, Rv, col="Pre-intervention", lty="Pre-intervention"), lwd=2) +
  geom_line(aes(Gratio, strength_post_strength, col="Post-intervention (constant-strength)", lty="Post-intervention (constant-strength)"), lwd=2) +
  geom_line(aes(Gratio, speed_post_strength, col="Post-intervention (constant-speed)", lty="Post-intervention (constant-speed)"), lwd=2) +
  scale_x_log10("Generation interval ratio, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(2/3, 1, 3/2),
                labels=c("2/3", 1, "3/2")) +
  scale_y_log10("Strength", breaks=c(0.5, 1, 2, 4), limits=c(0.5, 4)) +
  scale_linetype_manual("", values=c(2, 3, 1)) +
  scale_color_manual("", values=(colorblind_pal()(3)[c(2, 3, 1)])) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.35, 0.85),
    legend.background = element_rect(fill=NA)
  )

g6 <- ggplot(intervention) +
  geom_hline(yintercept=0, col="gray") +
  geom_line(aes(Gratio, rv, col="Epidemic", lty="Epidemic"), lwd=2) +
  geom_line(aes(Gratio, strength_post_speed, col="Post-intervention (constant-strength)", lty="Post-intervention (constant-strength)"), lwd=2) +
  geom_line(aes(Gratio, speed_post_speed, col="Post-intervention (constant-speed)", lty="Post-intervention (constant-speed)"), lwd=2) +
  scale_x_log10("Generation interval ratio, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(2/3, 1, 3/2),
                labels=c("2/3", 1, "3/2")) +
  scale_y_continuous("Speed (1/days)") +
  scale_linetype_manual("", values=1:3) +
  scale_color_manual("", values=(colorblind_pal()(3))) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )


gtot <- ggarrange(g1, g2, g3, g4, g5, g6, nrow=3, ncol=2)

tikz(file = "control.tex", width = 12, height = 10, standAlone = T)
gtot
dev.off()
tools::texi2dvi('control.tex', pdf = T, clean = T)
