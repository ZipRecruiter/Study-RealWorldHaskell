SUBMODULES := dnmfarrell
SUBMODULES := $(SUBMODULES:%=%/.git)

default:

pull: $(SUBMODULES)
	git pull --rebase
	git submodule update

$(SUBMODULES):
	git submodule init -- $(@:%.git=%)
	git submodule update -- $(@:%.git=%)

clean:
	git submodule deinit --all
