## Define commonly used variables for a node $(P)

define common_macro
$$(P)/dir ?= $$(@D)
$$(P)/nick ?= $$(shell echo $$(@D) | sed -e s/_//)
$$(P)/id ?= $$(shell echo $$(call get_port,$(1),0))
$$(P)/orport ?= $$(shell echo $$(call get_port,$(1),$(BASE_OR_PORT)))
$$(P)/dirport ?= $$(shell echo $$(call get_port,$(1),$(BASE_DIR_PORT)))
$$(P)/socksport ?= $$(shell echo $$(call get_port,$(1),$(BASE_SOCKS_PORT)))
$$(P)/ip ?= 127.0.0.1
$$(P)/addr ?= $$($$(@D)/ip):$$($$(@D)/dirport)
$$(P)/lifetime ?= 12
$$(P)/connlimit ?= 1
$$(P)/tor ?= $(TOR)
$$(P)/torflags ?= $(TORFLAGS)
$$(P)/gencert ?= $(gencert)
$$(P)/cert ?= $$(@D)/$$(@D)/keys/authority_certificate
$$(P)/sig_key ?= $$(@D)/$$(@D)/keys/authority_signing_key
$$(P)/id_key ?= $$(@D)/$$(@D)/keys/identity_key
$$(P)/torrc ?= $$(PWD)/$$(@D)/torrc

.PHONY: $$(P)/start
$$(P)/start: $$(P)/torrc
	$$($$(@D)/tor) $$($$(@D)/torflags) -f $(PWD)/$$(@D)/torrc &

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
	mkdir -p $$(@D)/$$(@D)/keys
	$$(genupasswd) > $$(@D)/pass
	cat $$(@D)/pass | $$($$(@D)/gencert) --create-identity-key \
		-i $$($$(@D)/id_key) -s $$($$(@D)/sig_key) -c $$($$(@D)/cert) \
		-m $$($$(@D)/lifetime) -a $$($$(@D)/addr) --passphrase-fd 0
	touch $$@

$$(P)/v3id: $$(P)/cert
	grep fingerprint $$($$(@D)/cert) | cut -f 2 -d " " > $$@

$$(P)/fp: $$(P)/cert
	(cd $$(@D); \
		echo "DirServer test 127.0.0.1:1 0000000000000000000000000000000000000000" >> torrc.tmp; \
		echo "OrPort 1" >> torrc.tmp; \
		$$($$(@D)/tor) --quiet --list-fingerprint --DataDirectory $$(@D) \
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
