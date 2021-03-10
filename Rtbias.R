library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(egg)
library(ggpubr)
library(gridExtra)
source("renewal_det.R")

## fix theta = 1.5
## fix kappa = 1/5
## fix assumed GI
## assume wild type = var but not necessarily the one we're assuming

theta <- 1.5
genfun_assumed <- function(x) dgamma(x, 5, 5/5)
genfun_short <- function(x) dgamma(x, 5, 5/4)
genfun_long <- function(x) dgamma(x, 5, 5/6)

svec <- c("Wild type GI = Variant GI = Assumed GI",
          "Wild type GI = Variant GI < Assumed GI",
          "Assumed GI < Wild type GI = Variant GI",
          "Variant GI < Assumed GI < Wild type GI",
          "Wild type GI < Assumed GI < Variant GI",
          "Variant GI < Wild type GI = Assumed GI",
          "Wild type GI = Assumed GI < Variant GI")

slist <- list(
  list(genfun1=genfun_assumed,
       genfun2=genfun_assumed,
       genfun3=genfun_assumed),
  list(genfun1=genfun_short,
       genfun2=genfun_short,
       genfun3=genfun_assumed),
  list(genfun1=genfun_long,
       genfun2=genfun_long,
       genfun3=genfun_assumed),
  list(genfun1=genfun_long,
       genfun2=genfun_short,
       genfun3=genfun_assumed),
  list(genfun1=genfun_short,
       genfun2=genfun_long,
       genfun3=genfun_assumed),
  list(genfun1=genfun_assumed,
       genfun2=genfun_short,
       genfun3=genfun_assumed),
  list(genfun1=genfun_assumed,
       genfun2=genfun_long,
       genfun3=genfun_assumed)
)

glist <- vector('list', length(svec))

for (i in 1:length(svec)) {
  print(i)
  
  tmparg <- c(theta=theta,
    tmax=150, slist[[i]])
  
  rr <- do.call(renewal_det, tmparg) %>%
    filter(tvec > 15)
  
  
  g1 <- ggplot(rr) +
    geom_line(aes(tvec, Ivec1, color="Wild type")) +
    geom_line(aes(tvec, Ivec2, color="Variant")) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 110)) +
    scale_y_continuous("Incidence (1/days)") +
    scale_color_manual(values=c("red", "black")) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.75, 0.75),
      legend.title = element_blank()
    )
  
  if (i != 1) {
    g1 <- g1 + theme(legend.position="none")
  }
  
  g2 <- ggplot(rr) +
    geom_line(aes(tvec, Rt1, color="Wild type")) +
    geom_line(aes(tvec, Rt2, color="Variant")) +
    geom_line(aes(tvec, Rtest1, color="Wild type"), lty=2) +
    geom_line(aes(tvec, Rtest2, color="Variant"), lty=2) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
    scale_y_continuous("Reproduction number") +
    scale_color_manual(values=c("red", "black")) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none"
    )
  
  g3 <- ggplot(rr) +
    geom_line(aes(tvec, Rt2/Rt1, color="True ratio")) +
    geom_line(aes(tvec, Rtest2/Rtest1, color="Estimated ratio")) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 145)) +
    scale_y_continuous("Relative strength") +
    scale_color_manual(values=c("orange", "purple")) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.7, 0.25),
      legend.title = element_blank()
    )
  
  if (i != 1) {
    g3 <- g3 + theme(legend.position="none")
  }
  
  g4 <- ggplot(rr) +
    geom_abline(intercept=0, slope=1, lty=2) +
    geom_abline(intercept=0, slope=theta, lty=3) +
    geom_path(aes(Rtest1, Rtest2), col="orange") +
    scale_x_continuous("Estimated wild type strength", limits=c(0, 2.6), expand=c(0, 0)) +
    scale_y_continuous("Estimated variant strength", limits=c(0, 2.6*theta), expand=c(0, 0)) +
    scale_color_manual(values=c("orange", "purple")) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.7, 0.2),
      legend.title = element_blank()
    )
  
  gtot <- egg::ggarrange(g1, g2, g3, g4, nrow=1, draw=FALSE)
  
  
  glist[[i]] <- annotate_figure(gtot, top=paste0(LETTERS[i], ". ", svec[i]))
}

gfinal <- do.call(arrangeGrob, c(glist, ncol=1))

ggsave("Rtbias.pdf", gfinal, width=12, height=16)
