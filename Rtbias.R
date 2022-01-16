library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size = 14))
library(egg)
library(ggpubr)
library(gridExtra)
library(tikzDevice)
source("renewal_det.R")

## fix theta = 1.61
## fix kappa = 1/5
## fix assumed GI
## assume wild type = var but not necessarily the one we're assuming

theta <- 1.61
genfun_assumed <- function(x) dgamma(x, 5, 5/5)
genfun_short <- function(x) dgamma(x, 5, 5/4)
genfun_long <- function(x) dgamma(x, 5, 5/6)

svec <- c("Variant GI $<$ Wild type GI = Assumed GI",
          "Variant GI = Wild type GI = Assumed GI",
          "Variant GI $>$ Wild type GI = Assumed GI")

slist <- list(
  list(genfun1=genfun_assumed,
       genfun2=genfun_short,
       genfun3=genfun_assumed),
  list(genfun1=genfun_assumed,
       genfun2=genfun_assumed,
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
      if (t < 30) {
        return(2)
      } else if (t < 60) {
        return(0.4)
      } else {
        return(1)
      }
    })
  
  rr <- do.call(renewal_det, tmparg) %>%
    filter(tvec < 70)
  
  g1 <- ggplot(rr) +
    geom_line(aes(tvec, Rt1, color="Wild type", lty="True"), lwd=2) +
    geom_line(aes(tvec, Rt2, color="Variant", lty="True"), lwd=2) +
    geom_line(aes(tvec, Rtest1, color="Wild type", lty="Estimated"), lwd=2) +
    geom_line(aes(tvec, Rtest2, color="Variant", lty="Estimated"), lwd=2) +
    annotate(geom="text",label=LETTERS[(i-1)*3+1],x=-Inf,y=Inf,
             size=5,
             vjust=1.5,hjust=-0.5) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(0, 70)) +
    scale_y_log10("Reproduction number, $\\mathcal{R}(t)$", limits=c(0.39, 8),
                  breaks=c(0.25, 0.5, 1, 2, 4, 8)) +
    scale_color_manual(values=c("red", "black")) +
    scale_linetype_manual(values=c(2, 1), guide=FALSE) +
    coord_fixed() +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.7, 0.81),
      legend.title = element_blank()
    )
  
  if (i != 2) {
    g1 <- g1 + theme(legend.position="none")
  }
  
  if (i!=2) {
    g1 <- ggplot(rr) +
      geom_line(aes(tvec, Rt1, col="Wild Type", lty="Wild Type"), lwd=2) +
      geom_line(aes(tvec, Rt2, col="Variant", lty="Variant"), lwd=2) +
      geom_line(aes(tvec, Rtest1, lty="Estimated"), col="black", lwd=2) +
      geom_line(aes(tvec, Rtest2, col="Estimated", lty="Estimated"), lwd=2) +
      annotate(geom="text",label=LETTERS[(i-1)*3+1],x=-Inf,y=Inf,
               size=5,
               vjust=1.5,hjust=-0.5) +
      scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(0, 70)) +
      scale_y_log10("Reproduction number, $\\mathcal{R}(t)$", limits=c(0.39, 8),
                    breaks=c(0.25, 0.5, 1, 2, 4, 8)) +
      scale_color_manual("a", values=c("red", "red", "black")) +
      scale_linetype_manual("a", values=c(2, 1, 1)) +
      coord_fixed() +
      theme(
        panel.grid = element_blank(),
        legend.position = c(0.7, 0.81),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA)
      )
  }
  
  g2 <- ggplot(rr) +
    geom_line(aes(tvec, Rt2/Rt1, col="True", lty="True"), lwd=2) +
    geom_line(aes(tvec, Rtest2/Rtest1, col="Estimated", lty="Estimated"), lwd=2) +
    annotate(geom="text",label=LETTERS[(i-1)*3+2],x=-Inf,y=Inf,
             size=5,
             vjust=1.5,hjust=-0.5) +
    scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(0, 70)) +
    scale_y_log10("Relative strength, $\\rho(t)$", limits=c(1, 2.5)) +
    scale_color_manual("a", values=c("orange", "purple")) +
    scale_linetype_manual("a", values=c(2, 1)) +
    coord_fixed() +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.7, 0.86),
      legend.title = element_blank(),
      legend.background = element_rect(fill=NA)
    )
  
  if (i != 1) {
    g2 <- g2 + theme(legend.position="none")
  }
  
  g3 <- ggplot(rr) +
    geom_abline(intercept=0, col="purple", slope=theta, lty=1, lwd=2) +
    geom_path(aes(Rtest1, Rtest2, col="Estimated", lty="Estimated"), lwd=2) +
    geom_abline(intercept=0, col="gray", slope=1, lty=1, lwd=1) +
    annotate("text", x=3.6, y=3.2, label="$\\mathcal{R}_{\\textrm{var}}(t)=\\mathcal{R}_{\\textrm{wt}}(t)$",
             angle=45) +
    annotate(geom="text",label=LETTERS[(i-1)*3+3],x=-Inf,y=Inf,
             size=5,
             vjust=1.5,hjust=-0.5) +
    scale_x_continuous("Wild type strength, $\\mathcal{R}_{\\textrm{wt}}(t)$", limits=c(0, 3*theta), expand=c(0, 0)) +
    scale_y_continuous("Variant strength, $\\mathcal{R}_{\\textrm{var}}(t)$", limits=c(0, 3*theta), expand=c(0, 0)) +
    coord_fixed() +
    scale_color_manual("", values=c("orange", "blue")) + 
    scale_linetype_manual("", values=2:3) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.7, 0.2),
      legend.title = element_blank(),
      legend.background = element_rect(fill=NA)
    )
  
  if (i!=3) {
    g3 <- g3 + 
      annotate("text", x=2.5, y=3.6, label="$\\mathcal{R}_{\\textrm{var}}(t)=\\rho \\mathcal{R}_{\\textrm{wt}}(t)$",
               angle=atan(theta)/pi*180, col="purple")
  }
  
  if (i==3) {
    g3 <- g3 + 
      annotate("text", x=1.9, y=3.6, label="$\\mathcal{R}_{\\textrm{var}}(t)=\\rho \\mathcal{R}_{\\textrm{wt}}(t)$",
               angle=atan(theta)/pi*180, col="purple")
  }
  
  if (i != 2) {
    g3 <- g3 +
      geom_smooth(aes(Rtest1, Rtest2, col="Regression", lty="Regression"), method="lm", formula=y~-1+x, fullrange=TRUE, se=FALSE, lwd=2)
    
  }
  
  gtot <- egg::ggarrange(g1, g2, g3, nrow=1, draw=FALSE)
  
  glist[[i]] <- annotate_figure(gtot, top=text_grob(svec[i], size=14))
}

tikz(file = "Rtbias.tex", width = 10, height = 10, standAlone = T)
do.call(grid.arrange, c(glist, ncol=1))
dev.off()
tools::texi2dvi('Rtbias.tex', pdf = T, clean = T)
