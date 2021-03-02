library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(egg)
source("renewal_det.R")

rr1 <- renewal_det(tmax=150)
rr2 <- renewal_det(Rfun=function(t) ifelse(t < 50, 2.5, ifelse(t < 80, 0.5, 1.2)) ,
                   tmax=150)

g1 <- ggplot(rr1) +
  geom_line(aes(tvec, Ivec1, color="Wild type")) +
  geom_line(aes(tvec, Ivec2, color="Variant")) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Instantaneous incidence (1/days)") +
  scale_color_manual(values=c("red", "black")) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.7, 0.8),
    legend.title = element_blank()
  )

g2 <- ggplot(rr1) +
  geom_line(aes(tvec, Ivec2/(Ivec1 + Ivec2))) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Proportion of the new variant") +
  scale_color_manual(values=c("red", "black")) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(rr1) +
  geom_line(aes(tvec, Rt1, color="Wild type")) +
  geom_line(aes(tvec, Rt2, color="Variant")) +
  geom_line(aes(tvec, Rtest2, color="Variant"), lty=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Reproduction number") +
  scale_color_manual(values=c("red", "black")) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g4 <- ggplot(rr1) +
  geom_line(aes(tvec, Rt2/Rt1, color="True ratio")) +
  geom_line(aes(tvec, Rtest2/Rt1, color="Estimaed ratio")) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Reproduction number ratio") +
  scale_color_manual(values=c("orange", "purple")) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.7, 0.2),
    legend.title = element_blank()
  )

g5 <- ggplot(rr2) +
  geom_line(aes(tvec, Ivec1, color="Wild type")) +
  geom_line(aes(tvec, Ivec2, color="Variant")) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Instantaneous incidence (1/days)") +
  scale_color_manual(values=c("red", "black")) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g6 <- ggplot(rr2) +
  geom_line(aes(tvec, Ivec2/(Ivec1 + Ivec2))) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Proportion of the new variant") +
  scale_color_manual(values=c("red", "black")) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g7 <- ggplot(rr2) +
  geom_line(aes(tvec, Rt1, color="Wild type")) +
  geom_line(aes(tvec, Rt2, color="Variant")) +
  geom_line(aes(tvec, Rtest2, color="Variant"), lty=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Reproduction number") +
  scale_color_manual(values=c("red", "black")) +
  ggtitle("G") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g8 <- ggplot(rr2) +
  geom_line(aes(tvec, Rt2/Rt1, color="True ratio")) +
  geom_line(aes(tvec, Rtest2/Rt1, color="Estimaed ratio")) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
  scale_y_continuous("Reproduction number ratio") +
  scale_color_manual(values=c("orange", "purple")) +
  ggtitle("H") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gtot <- ggarrange(g1, g2, g3, g4,
                  g5, g6, g7, g8,
                  nrow=2,
                  draw=FALSE)

ggsave("Rtbias.pdf", gtot, width=12, height=6)
