SUBMODULES := dnmfarrell
SUBMODULES := $(SUBMODULES:%=%/.git)

default:

force:

pull: $(SUBMODULES)
	git pull --rebase --recurse-submodules
	git submodule update --remote
	git submodule update

$(SUBMODULES): force
	git submodule init -- $(@:%.git=%)
	git submodule update -- $(@:%.git=%)

clean:
	git submodule deinit --all
