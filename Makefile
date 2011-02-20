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

# Configuration

TORDIR = $(HOME)/tor
TOR ?= $(TORDIR)/src/or/tor
TORFLAGS ?= --quiet
START ?= cd $$(@D); $$($$(@D)/tor) $$($$(@D)/torflags) -f $$($$(@D)/torrc) &

BASE_OR_PORT = 3000
BASE_DIR_PORT = 4000
BASE_SOCKS_PORT = 5000
BASE_CONTROL_PORT = 6000

AUTHORITIES ?= $(patsubst %,a_%,$(shell seq 0 4))
RELAYS      ?= $(patsubst %,r_%,$(shell seq 5 9))
CLIENTS     ?= $(patsubst %,c_%,$(shell seq 10 14))


# Helpers

genupasswd = python -c 'print open("/dev/urandom", "rb").read(16).encode("base64").strip()'
gencert = $(TORDIR)/src/tools/tor-gencert
get_port = $$(($(2) + `echo $(1) | cut -f2 -d_`))
genenv = env -i \
	ORPORT=$$($$(@D)/orport) \
	DIRPORT=$$($$(@D)/dirport) \
	SOCKSPORT=$$($$(@D)/socksport) \
	CONTROLPORT=$$($$(@D)/controlport) \
	NUM=$$($$(@D)/id) \
	DIR=$$($$(@D)/dir) \
	NICK=$$($$(@D)/nick) \
	CONNLIMIT=$$($$(@D)/connlimit) \
	TOR=$$($$(@D)/tor)


# Macros

define common_macro
$$(P)/dir ?= .
$$(P)/nick ?= $$(shell echo $$(@D) | sed -e s/_//)
$$(P)/id ?= $$(shell echo $$(call get_port,$(1),0))
$$(P)/orport ?= $$(shell echo $$(call get_port,$(1),$(BASE_OR_PORT)))
$$(P)/dirport ?= $$(shell echo $$(call get_port,$(1),$(BASE_DIR_PORT)))
$$(P)/socksport ?= $$(shell echo $$(call get_port,$(1),$(BASE_SOCKS_PORT)))
$$(P)/controlport ?= $$(shell echo $$(call get_port,$(1),$(BASE_CONTROL_PORT)))
$$(P)/ip ?= 127.0.0.1
$$(P)/addr ?= $$($$(@D)/ip):$$($$(@D)/dirport)
$$(P)/lifetime ?= 12
$$(P)/connlimit ?= 1
$$(P)/tor ?= $(TOR)
$$(P)/torflags ?= $(TORFLAGS)
$$(P)/gencert ?= $(gencert)
$$(P)/cert ?= keys/authority_certificate
$$(P)/sig_key ?= keys/authority_signing_key
$$(P)/id_key ?= keys/identity_key
$$(P)/torrc ?= $$(PWD)/$$(@D)/torrc
$$(P)/start ?= $(START)

.PHONY: $$(P)/start
$$(P)/start: $$(P)/torrc
	$$($$(@D)/start)

$$(P)/torrc: dirservers
	mkdir -p $$(@D)
	$(genenv) $$($$(@D)/gen_torrc) > $$@

CLEAN := $$(CLEAN) $$(P)
endef

## Define a tor authority node

define authority_macro
P := $(1)
$$(P)/gen_torrc ?= ./torrc_authority.sh
$(call common_macro,$(1))

$$(P)/cert:
	mkdir -p $$(@D)/keys
	$$(genupasswd) > $$(@D)/pass
	cd $$(@D); cat pass | $$($$(@D)/gencert) --create-identity-key \
		-i $$($$(@D)/id_key) -s $$($$(@D)/sig_key) -c $$($$(@D)/cert) \
		-m $$($$(@D)/lifetime) -a $$($$(@D)/addr) --passphrase-fd 0
	touch $$@

$$(P)/v3id: $$(P)/cert
	grep fingerprint $$(@D)/$$($$(@D)/cert) | cut -f 2 -d " " > $$@

$$(P)/fp: $$(P)/cert
	(cd $$(@D); \
		echo "DirServer test 127.0.0.1:1 0000000000000000000000000000000000000000" >> torrc.tmp; \
		echo "OrPort 1" >> torrc.tmp; \
		$$($$(@D)/tor) --quiet --list-fingerprint --DataDirectory . \
			-f torrc.tmp | cut -f2- -d' ' | sed -e 's/ //g'; \
		rm torrc.tmp &>/dev/null;) > $$@

$$(P)/dirserver: $$(P)/v3id $$(P)/fp
	rm -f $$@
	echo -n "DirServer $$($$(@D)/nick) v3ident=`cat $$(@D)/v3id`" >> $$@
	echo " orport=$$($$(@D)/orport) no-v2 $$($$(@D)/addr) `cat $$(@D)/fp`" >> $$@

CERTS := $$(CERTS) $$(P)/cert
P :=
endef

## Define a tor relay node

define relay_macro
P := $(1)
$$(P)/gen_torrc ?= ./torrc_relay.sh
$(call common_macro,$(1))
P :=
endef

## Define a client relay node

define client_macro
P := $(1)
$$(P)/gen_torrc ?= ./torrc_client.sh
$(call common_macro,$(1))
P :=
endef

CLEAN := $(CLEAN) dirservers

dirservers: $(patsubst %,%/dirserver,$(AUTHORITIES))
	cat $^ > $@


# Model Generation

# To see what rules are being added by the macros, uncomment:
#$(foreach n,$(AUTHORITIES),$(info $(call authority_macro,$(n))))
#$(foreach n,$(RELAYS),$(info $(call relay_macro,$(n))))
#$(foreach n,$(CLIENTS),$(info $(call client_macro,$(n))))

$(foreach n,$(AUTHORITIES),$(eval $(call authority_macro,$(n))))
$(foreach n,$(RELAYS),$(eval $(call relay_macro,$(n))))
$(foreach n,$(CLIENTS),$(eval $(call client_macro,$(n))))

NODES := $(AUTHORITIES) $(RELAYS) $(CLIENTS)

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
	find $(WORK) -name 'notice.log' -print0 | xargs -0 sh -c 'multitail "$$@" < /dev/tty' multitail

clean:
	rm -rf $(CLEAN)

.PHONY: clean all start stop
.DEFAULT_GOAL := all
