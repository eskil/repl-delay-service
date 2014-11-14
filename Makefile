ERL ?= erl
REBAR ?= rebar

.PHONY: deps

all: deps compile

compile:
	@$(REBAR) compile

app:
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

webmachine: app
	@exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl \
		-sname dispatch_webmachine \
		-s lager \
		-s reloader \
		-s dispatch_core \
		-s dispatch_webmachine

all:
	@$(REBAR) get-deps
	@$(REBAR) compile

release: all
	cd rel && $(REBAR) generate

.PHONE: release all
