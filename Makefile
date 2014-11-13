all:
	rebar get-deps
	rebar compile

release: all
	cd rel && rebar generate

.PHONE: release all
