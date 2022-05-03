## This is ssvariant
## https://github.com/parksw3/newvariant.git

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += newvariant.tex $(wildcard *.md)

## newvariant.pdf: newvariant.tex
## newvariant.tex.HEAD~2.oldfile:
## newvariant.ld.pdf: newvariant.tex

Sources += response.tex
## response.pdf: response.tex

Sources += supp.tex
## supp.pdf: supp.tex

######################################################################

# tikz figures; come up with a pipeline

Ignore += relstrength.pdf relspeed.pdf
relspeed.tex: relspeed.Rout ;
relspeed.Rout: relspeed.R
	$(wrapR)

relstrength.tex: relstrength.Rout ;
relstrength.Rout: relstrength.R
	$(wrapR)

Ignore += Rtbias_smooth.pdf control_sim.pdf
Ignore += Rtbias_smooth.tex
Rtbias_smooth.pdf Rtbias_smooth.tex: Rtbias_smooth.Rout ;

Rtbias_smooth.R: renewal_det.R; $(touch)
Rtbias_smooth.Rout: Rtbias_smooth.R
	$(wrapR)

Ignore += Rtbias.tex rel*.png rel*.tex
Rtbias.pdf Rtbias.tex: Rtbias.Rout ;

Rtbias.R: renewal_det.R; $(touch)
Rtbias.Rout: Rtbias.R
	$(wrapR)

Ignore += control_sim.tex
control_sim.tex: control_sim.Rout ;
control_sim.Rout: control_sim.R
	$(wrapR)

Ignore += control.tex
control.tex: control.Rout ;
control.Rout: control.R
	$(wrapR)

%.pdf: %.Rout ;

## pmake dotdir.vdtest

######################################################################

response.pdf: response.tex

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk

-include makestuff/git.mk
-include makestuff/visual.mk
