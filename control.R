library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
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
  
  strength_strength <- constant_strength
  strength_speed <- (Rv^kappa-1)/kappa/Gv - ((Rv/constant_strength)^kappa-1)/kappa/Gv
  
  speed_speed <- constant_speed 
  
  speed_strength <- Rv/integrate(function(y) Rv * dgamma(y, shape=1/kappa, rate=1/kappa/Gv) * exp(-constant_speed * y),
                                 0,
                                 Inf)[[1]]
  
  data.frame(
    Gratio=Gr,
    rw=rw,
    Rv=Rv,
    rv=rv,
    strength_strength=strength_strength,
    strength_speed=strength_speed,
    speed_strength=speed_strength,
    speed_speed=speed_speed
  )
}, simplify = FALSE) %>%
  bind_rows

g1 <- ggplot(intervention) +
  geom_line(aes(Gratio, Rv, col="Epidemic", lty="Epidemic"), lwd=1) +
  geom_line(aes(Gratio, strength_strength, col="Constant strength intervention", lty="Constant strength intervention"), lwd=1) +
  geom_line(aes(Gratio, speed_strength, col="Constant speed intervention", lty="Constant speed intervention"), lwd=1) +
  scale_x_log10("Generation interval ratio, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(0.67, 1, 1.5)) +
  scale_y_log10("Strength") +
  scale_linetype_manual("", values=3:1) +
  scale_color_manual("", values=rev(colorblind_pal()(3))) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.3, 0.85)
  )

g2 <- ggplot(intervention) +
  geom_line(aes(Gratio, rv, col="Epidemic", lty="Epidemic"), lwd=1) +
  geom_line(aes(Gratio, strength_speed, col="Constant strength intervention", lty="Constant strength intervention"), lwd=1) +
  geom_line(aes(Gratio, speed_speed, col="Constant speed intervention", lty="Constant speed intervention"), lwd=1) +
  scale_x_log10("Generation interval ratio, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(0.67, 1, 1.5)) +
  scale_y_continuous("Speed (1/days)") +
  scale_linetype_manual("", values=3:1) +
  scale_color_manual("", values=rev(colorblind_pal()(3))) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

gtot <- ggarrange(g1, g2, nrow=1)

tikz(file = "control.tex", width = 10, height = 5, standAlone = T)
gtot
dev.off()
tools::texi2dvi('control.tex', pdf = T, clean = T)
