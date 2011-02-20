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

# Enable secondary expansion of prerequisites
.SECONDEXPANSION:

# Configuration

-include $(CONFIG)

N = $$(@D)
TYPES ?= $(BASE_TYPES)
FIELDS ?= $(BASE_FIELDS)
NODES  ?= $(foreach t,$(TYPES),$($(t)))

TORDIR ?= $(HOME)/tor
TOR ?= $(TORDIR)/src/or/tor
TORFLAGS ?= --quiet
START ?= cd $N; $(N.tor) $(N.torflags) -f $(N.torrc) &

BASE_OR_PORT ?= 3000
BASE_DIR_PORT ?= 4000
BASE_SOCKS_PORT ?= 5000
BASE_CONTROL_PORT ?= 6000
BASE_TYPES ?= AUTHS RELAYS CLIENTS
BASE_FIELDS ?= dir nick id orport dirport socksport controlport ip addr lifetime \
	connlimit tor torflags gencert cert sig_key id_key torrc start gen_torrc

AUTHS   ?= $(patsubst %,a_%,$(shell seq 0 4))
RELAYS  ?= $(patsubst %,r_%,$(shell seq 5 9))
CLIENTS ?= $(patsubst %,c_%,$(shell seq 10 14))


# Helpers

genupasswd ?= python -c 'print open("/dev/urandom", "rb").read(16).encode("base64").strip()'
gencert ?= $(TORDIR)/src/tools/tor-gencert
genport ?= $$(($(2) + `echo $(1) | cut -f2 -d_`))
genenv ?= env -i $(foreach f,$(FIELDS),"$(f)=$(N.$(f))")


# Macros

define FIELDS_macro
N.$(1) = $$$$($$(N)/$(1))
endef

## -------------

define COMMON_macro
$$(P)/dir ?= .
$$(P)/nick ?= $$(shell echo $N | sed -e s/_//)
$$(P)/id ?= $$(shell echo $$(call genport,$(1),0))
$$(P)/orport ?= $$(shell echo $$(call genport,$(1),$(BASE_OR_PORT)))
$$(P)/dirport ?= $$(shell echo $$(call genport,$(1),$(BASE_DIR_PORT)))
$$(P)/socksport ?= $$(shell echo $$(call genport,$(1),$(BASE_SOCKS_PORT)))
$$(P)/controlport ?= $$(shell echo $$(call genport,$(1),$(BASE_CONTROL_PORT)))
$$(P)/ip ?= 127.0.0.1
$$(P)/addr ?= $(N.ip):$(N.dirport)
$$(P)/lifetime ?= 12
$$(P)/connlimit ?= 1
$$(P)/tor ?= $(TOR)
$$(P)/torflags ?= $(TORFLAGS)
$$(P)/gencert ?= $(gencert)
$$(P)/cert ?= keys/authority_certificate
$$(P)/sig_key ?= keys/authority_signing_key
$$(P)/id_key ?= keys/identity_key
$$(P)/torrc ?= $$(PWD)/$N/torrc
$$(P)/start ?= $(START)

.PHONY: $$(P)/start
$$(P)/start: $$(P)/torrc
	$(N.start)

$$(P)/torrc: dirservers
	mkdir -p $N
	$(genenv) $(N.gen_torrc) > $$@

CLEAN := $$(CLEAN) $$(P)
endef

## -------------

define AUTHS_macro
P := $(1)
$$(P)/gen_torrc ?= ./torrc_authority.sh
$(call COMMON_macro,$(1))

$$(P)/cert:
	mkdir -p $N/keys
	$$(genupasswd) > $N/pass
	cd $N; cat pass | $(N.gencert) --create-identity-key \
		-i $(N.id_key) -s $(N.sig_key) -c $(N.cert) \
		-m $(N.lifetime) -a $(N.addr) --passphrase-fd 0
	touch $$@

$$(P)/v3id: $$(P)/cert
	grep fingerprint $N/$(N.cert) | cut -f 2 -d " " > $$@

$$(P)/fp: $$(P)/cert
	(cd $N; \
		echo "DirServer test 127.0.0.1:1 0000000000000000000000000000000000000000" \
		  >> torrc.tmp; \
		echo "OrPort 1" >> torrc.tmp; \
		$(N.tor) --quiet --list-fingerprint --DataDirectory . \
			-f torrc.tmp | cut -f2- -d' ' | sed -e 's/ //g'; \
		rm torrc.tmp &>/dev/null;) > $$@

$$(P)/dirserver: $$(P)/v3id $$(P)/fp
	rm -f $$@
	echo -n "DirServer $(N.nick) v3ident=`cat $N/v3id`" >> $$@
	echo " orport=$(N.orport) no-v2 $(N.addr) `cat $N/fp`" >> $$@

DIRSERVERS := $$(DIRSERVERS) $$(P)/dirserver
P :=
endef

## -------------

define RELAYS_macro
P := $(1)
$$(P)/gen_torrc ?= ./torrc_relay.sh
$(call COMMON_macro,$(1))
P :=
endef

## -------------

define CLIENTS_macro
P := $(1)
$$(P)/gen_torrc ?= ./torrc_client.sh
$(call COMMON_macro,$(1))
P :=
endef

## -------------

dirservers: $$(DIRSERVERS)
	cat $^ > $@
CLEAN := $(CLEAN) dirservers


# Code Generation

## Uncomment the next line to print generated rules and definititions
#$(foreach t,FIELDS $(TYPES),$(foreach n,$($(t)),$(info $(call $(t)_macro,$(n)))))
$(foreach t,FIELDS $(TYPES),$(foreach n,$($(t)),$(eval $(call $(t)_macro,$(n)))))


# Top-level Targets

all: $(patsubst %,%/torrc,$(NODES))

start stop hup log: WORK=.
start-% stop-% hup-% log-%: WORK=$(wildcard $*)

start start-%:
	@$(MAKE) $(patsubst %/torrc,%/start,$(shell find $(WORK) -name 'torrc'))

stop stop-%:
	find $(WORK) -name pid -exec cat {} + | xargs kill

hup hup-%:
	find $(WORK) -name pid -exec cat {} + | xargs kill -HUP

log log-%:
	find $(WORK) -name 'notice.log' -print0 \
	| xargs -0 sh -c 'multitail "$$@" < /dev/tty' multitail

pr-%:
	@echo '$*=$($*)'

clean:
	rm -rf $(CLEAN)

.PHONY: clean all start stop
.DEFAULT_GOAL := all
