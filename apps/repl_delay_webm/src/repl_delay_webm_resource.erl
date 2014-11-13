-module(repl_delay_webm_resource).
-export([
	 init/1,
	 allowed_methods/2,
         content_types_provided/2,
	 to_json/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[
      {"application/json", to_json},
      {"text/html", to_html}
     ], RD, Ctx}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(RD, Ctx) ->
    {json_body(wrq:req_qs(RD)), RD, Ctx}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    {"<html><body>Hello, new world</body></html>", ReqData, State}.

json_body(QS) ->
     mochijson:encode({struct, QS}).
