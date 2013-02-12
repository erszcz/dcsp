#!/bin/sh

erl -sasl sasl_error_logger '{file,"logs/sasl.log"}' -sname dcsp-test -pa ebin/ -boot start_sasl -s dcsp
