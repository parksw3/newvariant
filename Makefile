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
relspeed.Rout: relspeed.R
	$(wrapR)

relstrength.Rout: relstrength.R
	$(wrapR)

Ignore += Rtbias.tex rel*.png
Rtbias.pdf Rtbias.tex: Rtbias.Rout ;
Rtbias.Rout: Rtbias.R

%.pdf: %.Rout ;

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
clonestuff:
	git clone $(msrepo)/makestuff
localstuff: 
	cd .. && $(MAKE) makestuff
	ln -s ../makestuff .
checkstuff:
	ls makestuff/Makefile


## not tested
flexstuff:
	((cd .. && $(MAKE) makestuff) && ln -s ../makestuff .) \
	|| git clone $(msrepo)/makestuff

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk

-include makestuff/git.mk
-include makestuff/visual.mk
