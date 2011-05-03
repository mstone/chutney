#!/bin/sh
erlc check_bootstrap.erl && erl -noshell -s check_bootstrap main -s init stop
