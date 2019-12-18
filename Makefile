SUBMODULES := dnmfarrell
SUBMODULES := $(SUBMODULES:%=%/.git)

default:

pull: $(SUBMODULES)
	git pull --rebase --recurse-submodules
	git submodule update --remote

$(SUBMODULES):
	git submodule init -- $(@:%.git=%)
	git submodule update -- $(@:%.git=%)

clean:
	git submodule deinit --all
