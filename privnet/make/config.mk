TORDIR = $(HOME)/tor
TOR ?= $(HOME)/tor/src/or/tor
TORFLAGS ?= --quiet

BASE_OR_PORT = 3000
BASE_DIR_PORT = 4000
BASE_SOCKS_PORT = 5000

AUTHORITIES ?= a_01 a_02 a_03 a_04 a_05 a_06 a_07 a_08 a_09 a_10
RELAYS      ?= r_11 r_12 r_13 r_14 r_15 r_16 r_17 r_18 r_19 r_20
CLIENTS     ?= c_21 c_22 c_23 c_24 c_25 c_26 c_27 c_28 c_29 c_30
