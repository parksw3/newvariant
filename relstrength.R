library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(egg)
library(tikzDevice)

deltavec <- seq(0, 0.15, length.out=101)
rwvec <- seq(-0.1, 0.1, length.out=101)

pardata <- expand.grid(deltavec, rwvec)

Gw <- 5
Gv <- 8

kappa <- 1/5

thetadata <- apply(pardata, 1, function(x) {
  delta <- x[[1]]
  rw <- x[[2]]
  
  theta <- ((1 + kappa * (rw + delta) * Gv)^(1/kappa))/
    ((1 + kappa * rw * Gw)^(1/kappa))
  
  thetahat <- ((1 + kappa * (rw + delta) * Gw)^(1/kappa))/
    ((1 + kappa * rw * Gw)^(1/kappa))
  
  
  data.frame(
    delta=delta,
    rw=rw,
    theta=theta,
    thetahat=thetahat,
    bias=thetahat-theta
  )
}) %>%
  bind_rows

g1 <- ggplot(thetadata) +
  geom_tile(aes(rw, delta, fill=theta)) +
  geom_contour(aes(rw, delta, z=theta), breaks=c(0.8, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4), col="white", lty=1) +
  geom_contour(aes(rw, delta, z=theta), breaks=c(1), col="white", lty=2) +
  annotate("text", x=-0.05, y=0.03, label=expression(theta==1), col="white") +
  scale_x_continuous(expression(Speed~of~the~wild~"types,"~r[w]), expand=c(0, 0)) +
  scale_y_continuous(expression(Relative~"speed,"~delta), expand=c(0, 0)) +
  scale_fill_viridis_c(expression(theta))

g2 <- ggplot(thetadata) +
  geom_tile(aes(rw, delta, fill=thetahat)) +
  geom_contour(aes(rw, delta, z=thetahat), col="white", lty=1) +
  scale_x_continuous(expression(Speed~of~the~wild~"types,"~r[w]), expand=c(0, 0)) +
  scale_y_continuous(expression(Relative~"speed,"~delta), expand=c(0, 0)) +
  scale_fill_viridis_c(expression(hat(theta)), option="A")

g3 <- ggplot(thetadata) +
  geom_tile(aes(rw, delta, fill=bias)) +
  geom_contour(aes(rw, delta, z=bias), breaks=c(-0.8, -0.6, -0.4, -0.2, 0.2), col="white", lty=1) +
  geom_contour(aes(rw, delta, z=bias), breaks=0, col="white", lty=2) +
  annotate("text", x=-0.05, y=0.075, label=expression(hat(theta)==theta), col="white") +
  scale_x_continuous(expression(Speed~of~the~wild~"types,"~r[w]), expand=c(0, 0)) +
  scale_y_continuous(expression(Relative~"speed,"~delta), expand=c(0, 0)) +
  scale_fill_viridis_c(expression(hat(theta)-theta), option="E")

gtot <- ggarrange(g1, g2, g3, nrow=1,
          labels=c("A", "B", "C"),
          draw=FALSE)

ggsave("relstrength.pdf", gtot, width=12, height=3)
