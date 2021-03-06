#/--------------------------------------------------------------------
#| Copyright 2013-2014 Erisata, UAB (Ltd.)
#|
#| Licensed under the Apache License, Version 2.0 (the "License");
#| you may not use this file except in compliance with the License.
#| You may obtain a copy of the License at
#|
#|     http://www.apache.org/licenses/LICENSE-2.0
#|
#| Unless required by applicable law or agreed to in writing, software
#| distributed under the License is distributed on an "AS IS" BASIS,
#| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#| See the License for the specific language governing permissions and
#| limitations under the License.
#\--------------------------------------------------------------------
REBAR=rebar

all: compile-all

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile skip_deps=true

compile-all:
	$(REBAR) compile

compile-web:
	make -C priv/www compile

check: test itest

test: compile
	mkdir -p logs
	env ERL_AFLAGS='-config test/sys' $(REBAR) eunit skip_deps=true verbose=1

itest: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_AFLAGS='-config test/itest-sys' $(REBAR) ct skip_deps=true $(CT_ARGS) || grep Testing logs/raw.log

rtest: compile itest
	@echo '##  Starting web server at http://localhost:8017/. Enter `q().` to quit.'
	mkdir -p logs
	env ERL_LIBS=deps erl -pa ebin -pa itest -config test/sys -s auth -s auth -s sync -s auth_RTEST

clean: clean-itest
	$(REBAR) clean skip_deps=true

clean-all: clean-itest
	$(REBAR) clean

clean-itest:
	rm -f itest/*.beam

clean-deps:
	rm -rf deps

.PHONY: all deps compile compile-all check test itest wtest clean clean-all clean-itest clean-deps clean-web

