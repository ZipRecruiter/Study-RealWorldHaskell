default:

pull: dnmfarrell
	git pull --rebase
	(cd dnmfarrell && git pull)

dnmfarrell:
	git clone git@github.com:dnmfarrell/Real-World-Haskell $@
