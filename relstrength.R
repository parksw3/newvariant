library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=18, base_family = "Times"))
library(ggthemes)
library(egg)
library(directlabels)
library(tikzDevice)

delta <- 0.1

rwvec <- c(-0.15, -0.1, -0.05, 0, 0.05)
kappavec <- c(0, 0.2, 0.5, 1)

Gw <- 5

Gratio <- exp(seq(log(1/1.5), log(1.5), length.out=31))

pardata <- expand.grid(kappavec, rwvec, Gratio)

rhodata <- apply(pardata, 1, function(x) {
  kappa <- x[[1]]
  rw <- x[[2]]
  Gratio <- x[[3]]
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
    kappa=factor(kappa, levels=rev(c(0, 0.2, 0.5, 1))),
    rw=factor(rw, levels=c(rwvec[3:5], rwvec[1:2]),
              labels=c("$r_{\\textrm{wt}} < 0 < r_{\\textrm{var}}$",
                       "$0 = r_{\\textrm{wt}} < r_{\\textrm{var}}$",
                       "$0 < r_{\\textrm{wt}} < r_{\\textrm{var}}$",
                       "$r_{\\textrm{wt}} < r_{\\textrm{var}} < 0$",
                       "$r_{\\textrm{wt}} < r_{\\textrm{var}} = 0$"))
  )

range(filter(rhodata, !kappa %in% c(0, 1))$rho)

textdata <- data.frame(
  rw=c("$r_{\\textrm{wt}} < 0 < r_{\\textrm{var}}$",
                     "$0 = r_{\\textrm{wt}} < r_{\\textrm{var}}$",
                     "$0 < r_{\\textrm{wt}} < r_{\\textrm{var}}$",
                     "$r_{\\textrm{wt}} < r_{\\textrm{var}} < 0$",
                     "$r_{\\textrm{wt}} < r_{\\textrm{var}} = 0$"),
  label_rw=c("$r_{\\textrm{wt}}=-0.05/\\textrm{day}$",
             "$r_{\\textrm{wt}}=0/\\textrm{day}$",
             "$r_{\\textrm{wt}}=0.05/\\textrm{day}$",
             "$r_{\\textrm{wt}}=-0.15/\\textrm{day}$",
             "$r_{\\textrm{wt}}=-0.1/\\textrm{day}$"),
  label_rv=c("$r_{\\textrm{var}}=0.05/\\textrm{day}$",
             "$r_{\\textrm{var}}=0.1/\\textrm{day}$",
             "$r_{\\textrm{var}}=0.15/\\textrm{day}$",
             "$r_{\\textrm{var}}=-0.05/\\textrm{day}$",
             "$r_{\\textrm{var}}=0/\\textrm{day}$")
)

kappa_title1 <- data.frame(
  x=c(1.3, 1.3),
  y=c(1.5, 2.2),
  labels=c("$\\kappa=0$", "$\\kappa=1$"),
  rw="$r_{\\textrm{wt}} < r_{\\textrm{var}} = 0$"
)

kappa_title2 <- data.frame(
  x=c(1.3, 1.3),
  y=c(1.4, 2.4),
  labels=c("$\\kappa=1$", "$\\kappa=0$"),
  rw="$0 < r_{\\textrm{wt}} < r_{\\textrm{var}}$"
)

g1 <- ggplot(rhodata) +
  geom_line(aes(Gratio, rho, lty=kappa, col=kappa), lwd=2) +
  geom_text(data=kappa_title1, aes(x, y, label=labels), col=(colorblind_pal()(4))[c(1, 4)], size=5) +
  geom_text(data=kappa_title2, aes(x, y, label=labels), col=(colorblind_pal()(4))[c(4, 1)], size=5) +
  geom_text(data=textdata, aes(x=3/2, y=3.9, label=label_rw), hjust=1, vjust=1) +
  geom_text(data=textdata, aes(x=3/2, y=3.5, label=label_rv), hjust=1, vjust=1) +
  scale_x_log10("Relative mean generation interval, $\\bar{G}_{\\mathrm{var}}/\\bar{G}_{\\mathrm{wt}}$",
                breaks=c(2/3, 1, 3/2),
                labels=c("2/3", 1, "3/2"),
                expand=c(0, 0.03)) +
  scale_y_log10("Relative strength, $\\rho$", breaks=c(1, 2, 4), limits=c(1, 4), expand=c(0, 0)) +
  facet_wrap(~rw, as.table=F) +
  scale_color_manual("$\\kappa$", values=rev(colorblind_pal()(4))) +
  scale_linetype_manual("$\\kappa$", values=rev(c("solid", "22", "42", "44"))) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.position = c(0.8, 0.8)
  )

tikz(file = "relstrength.tex", width = 10, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('relstrength.tex', pdf = T, clean = T)
