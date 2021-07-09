## This is ssvariant
## https://github.com/parksw3/newvariant.git

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += newvariant.tex
newvariant.pdf: newvariant.tex

######################################################################

# tikz figures; come up with a pipeline

Ignore += relstrength.pdf relspeed.pdf
relspeed.tex: relspeed.Rout ;
relspeed.Rout: relspeed.R
	$(wrapR)

relstrength.tex: relstrength.Rout ;
relstrength.Rout: relstrength.R
	$(wrapR)

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

## Want to chain and make makestuff if it doesn't exist
## Compress this ¶ to choose default makestuff route
Makefile: makestuff/Makefile
makestuff/Makefile:
	((cd .. && $(MAKE) makestuff) && ln -s ../makestuff .) \
	|| git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk

-include makestuff/git.mk
-include makestuff/visual.mk
