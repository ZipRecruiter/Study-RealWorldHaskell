SUBMODULES := dnmfarrell
SUBMODULES := $(SUBMODULES:%=%/.git)

default:

force:

pull: $(SUBMODULES)
	git pull --rebase
	git submodule update --recursive --remote

$(SUBMODULES): force
	git submodule init -- $(@:%.git=%)
	git submodule update -- $(@:%.git=%)

clean:
	git submodule deinit --all
