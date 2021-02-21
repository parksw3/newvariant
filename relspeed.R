library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=14, base_family = "Times"))
library(egg)
library(tikzDevice)

thetavec <- seq(1, 2, length.out=101)
Rwvec <- seq(0.5, 1.5, length.out=101)

pardata <- expand.grid(thetavec, Rwvec)

Gw <- 5
Gv <- 8

kappa <- 1/5

deltadata <- apply(pardata, 1, function(x) {
  theta <- x[[1]]
  Rw <- x[[2]]
  
  delta <- ((theta * Rw)^kappa - 1)/(kappa*Gv) -  ((Rw)^kappa - 1)/(kappa*Gw)
  
  deltahat <- ((theta * Rw)^kappa - 1)/(kappa*Gw) -  ((Rw)^kappa - 1)/(kappa*Gw)
  
  data.frame(
    theta=theta,
    Rw=Rw,
    delta=delta,
    deltahat=deltahat,
    bias=deltahat-delta
  )
}) %>%
  bind_rows

g1 <- ggplot(deltadata) +
  geom_tile(aes(Rw, theta, fill=delta)) +
  geom_contour(aes(Rw, theta, z=delta), breaks=c(-0.03, 0.03, 0.06, 0.09), col="white", lty=1) +
  geom_contour(aes(Rw, theta, z=delta), breaks=0, col="white", lty=2) +
  annotate("text", x=1.25, y=1.25, label=expression(delta==0), col="white") +
  scale_x_continuous(expression(Strength~of~the~wild~"types,"~R[w]), expand=c(0, 0)) +
  scale_y_continuous(expression(Relative~"strength,"~theta), expand=c(0, 0)) +
  scale_fill_viridis_c(expression(delta))

g2 <- ggplot(deltadata) +
  geom_tile(aes(Rw, theta, fill=deltahat)) +
  geom_contour(aes(Rw, theta, z=deltahat), breaks=c(0.03, 0.06, 0.09, 0.12, 0.15), col="white", lty=1) +
  scale_x_continuous(expression(Strength~of~the~wild~"types,"~R[w]), expand=c(0, 0)) +
  scale_y_continuous(expression(Relative~"strength,"~theta), expand=c(0, 0)) +
  scale_fill_viridis_c(expression(hat(delta)), option="A")

g3 <- ggplot(deltadata) +
  geom_tile(aes(Rw, theta, fill=bias)) +
  geom_contour(aes(Rw, theta, z=bias), breaks=c(-0.04, -0.02, 0.02, 0.04, 0.06, 0.08), col="white", lty=1) +
  geom_contour(aes(Rw, theta, z=bias), breaks=0, col="white", lty=2) +
  annotate("text", x=0.78, y=1.5, label=expression(hat(delta)==delta), col="white") +
  scale_x_continuous(expression(Strength~of~the~wild~"types,"~R[w]), expand=c(0, 0)) +
  scale_y_continuous(expression(Relative~"strength,"~theta), expand=c(0, 0)) +
  scale_fill_viridis_c(expression(hat(delta)-delta), option="E",
                       breaks=c(-0.06, -0.03, 0, 0.03, 0.06, 0.09))

gtot <- ggarrange(g1, g2, g3, nrow=1,
                  labels=c("A", "B", "C"),
                  draw=FALSE)

ggsave("relspeed.pdf", gtot, width=12, height=3)
