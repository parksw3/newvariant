library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size = 12))
library(egg)
library(ggpubr)
library(gridExtra)
library(tikzDevice)
source("renewal_det.R")

## fix theta = 1.5
## fix kappa = 1/5
## fix assumed GI
## assume wild type = var but not necessarily the one we're assuming

theta <- 1.61
genfun_assumed <- function(x) dgamma(x, 5, 5/5)
genfun_short <- function(x) dgamma(x, 5, 5/4)
genfun_long <- function(x) dgamma(x, 5, 5/6)

svec <- c("Variant GI = Wild type GI = Assumed GI",
          "Variant GI $<$ Wild type GI = Assumed GI",
          "Variant GI $>$ Wild type GI = Assumed GI")

slist <- list(
  list(genfun1=genfun_assumed,
       genfun2=genfun_assumed,
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
    tmax=150, slist[[i]],
    Rfun=function(t) {
      if (t < 15) {
        return(2.5)
      } else {
        return(2.5 * (1 + 0.7 * cos((t-15)/10))/1.7)
      }
    })
  
  rr <- do.call(renewal_det, tmparg) %>%
    filter(tvec > 15, tvec < 70)
  
  g1 <- ggplot(rr) +
    geom_line(aes(tvec, Rt1, color="Wild type", lty="True"), lwd=1) +
    geom_line(aes(tvec, Rt2, color="Variant", lty="True"), lwd=1) +
    geom_line(aes(tvec, Rtest1, color="Wild type", lty="Estimated"), lwd=1) +
    geom_line(aes(tvec, Rtest2, color="Variant", lty="Estimated"), lwd=1) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 70)) +
    scale_y_continuous("Reproduction number, $\\mathcal{R}(t)$", limits=c(0.25, 7)) +
    scale_color_manual(values=c("red", "black")) +
    scale_linetype_manual(values=c(2, 1), guide=FALSE) +
    ggtitle(LETTERS[(i-1)*3+1]) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.6, 0.81),
      legend.title = element_blank()
    )
  
  if (i != 1) {
    g1 <- g1 + theme(legend.position="none")
  }
  
  if (i==2) {
    g1 <- ggplot(rr) +
      geom_line(aes(tvec, Rt1, lty="True"), color="black", lwd=1) +
      geom_line(aes(tvec, Rt2, col="True", lty="True"), lwd=1) +
      geom_line(aes(tvec, Rtest1, lty="Estimated"), col="black", lwd=1) +
      geom_line(aes(tvec, Rtest2, col="Estimated", lty="Estimated"), lwd=1) +
      scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 70)) +
      scale_y_continuous("Reproduction number, $\\mathcal{R}(t)$", limits=c(0.25, 7)) +
      scale_color_manual("a", values=c("red", "red")) +
      scale_linetype_manual("a", values=c(2, 1)) +
      ggtitle(LETTERS[(i-1)*3+1]) +
      theme(
        panel.grid = element_blank(),
        legend.position = c(0.6, 0.81),
        legend.title = element_blank()
      )
  }
  
  g2 <- ggplot(rr) +
    geom_line(aes(tvec, Rt2/Rt1, col="True", lty="True"), lwd=1) +
    geom_line(aes(tvec, Rtest2/Rtest1, col="Estimated", lty="Estimated"), lwd=1) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(15, 70)) +
    scale_y_continuous("Relative strength, $\\rho(t)$", limits=c(1, 2.5)) +
    scale_color_manual("a", values=c("orange", "purple")) +
    scale_linetype_manual("a", values=c(2, 1)) +
    ggtitle(LETTERS[(i-1)*3+2]) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.2, 0.81),
      legend.title = element_blank()
    )
  
  if (i != 1) {
    g2 <- g2 + theme(legend.position="none")
  }
  
  g3 <- ggplot(rr) +
    geom_abline(intercept=0, col="purple", slope=theta, lty=1, lwd=1) +
    geom_path(aes(Rtest1, Rtest2), col="orange", lwd=1, lty=2) +
    scale_x_continuous("Wild type strength, $\\mathcal{R}_{\\textrm{wt}}(t)$", limits=c(0, 3), expand=c(0, 0)) +
    scale_y_continuous("Variant strength, $\\mathcal{R}_{\\textrm{var}}(t)$", limits=c(0, 3*theta), expand=c(0, 0)) +
    scale_color_manual(values=c("orange", "purple")) +
    ggtitle(LETTERS[(i-1)*3+3]) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.7, 0.2),
      legend.title = element_blank()
    )
  
  gtot <- egg::ggarrange(g1, g2, g3, nrow=1, draw=FALSE)
  
  glist[[i]] <- annotate_figure(gtot, top=text_grob(svec[i], size=14))
}

tikz(file = "Rtbias.tex", width = 12, height = 10, standAlone = T)
do.call(grid.arrange, c(glist, ncol=1))
dev.off()
tools::texi2dvi('Rtbias.tex', pdf = T, clean = T)
