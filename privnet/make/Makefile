#!/usr/bin/make -rf

# Disable pre-existing implicit rules and suffix rules for easier dep debugging.
%.o : %.s
% : RCS/%,v
% : RCS/%
% : %,v
% : s.%
% : SCCS/s.%
.SUFFIXES:
SUFFIXES :=

include config.mk
include helpers.mk
include macros.mk

# To see what rules are being added by the macros, uncomment:
#$(foreach n,$(AUTHORITIES),$(info $(call authority_macro,$(n))))
#$(foreach n,$(RELAYS),$(info $(call relay_macro,$(n))))
#$(foreach n,$(CLIENTS),$(info $(call client_macro,$(n))))

$(foreach n,$(AUTHORITIES),$(eval $(call authority_macro,$(n))))
$(foreach n,$(RELAYS),$(eval $(call relay_macro,$(n))))
$(foreach n,$(CLIENTS),$(eval $(call client_macro,$(n))))

NODES := $(AUTHORITIES) $(RELAYS) $(CLIENTS)

all: $(patsubst %,%/torrc,$(NODES))

start: $(patsubst %,%/start,$(NODES))

stop:
	find . -name pid -exec cat {} + | xargs kill

clean:
	rm -rf $(CLEAN)

.PHONY: clean all start stop
.DEFAULT_GOAL := all
