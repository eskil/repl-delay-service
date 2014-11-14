-module(repl_delay_webm_resource).
-export([
     init/1,
     allowed_methods/2,
     content_types_provided/2,
     to_json/2,
     to_html/2
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
    PathInfo = wrq:path_info(RD),
    case PathInfo of
        [] ->
            {json_body(repl_delay_core_server:get_replication_delays()), RD, Ctx};
        [{id, "min"}] ->
            {json_body(repl_delay_core_server:get_min_replication_delay()), RD, Ctx};
        [{id, "max"}] ->
            {json_body(repl_delay_core_server:get_max_replication_delay()), RD, Ctx}
    end.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    PathInfo = wrq:path_info(ReqData),
    case PathInfo of
        [] ->
            {"<html><body>I am repl delay service, I need erlydtl for body</body></html>", ReqData, State};
        [{id, "min"}] ->
            {"<html><body>I am repl delay service, I need erlydtl for min</body></html>", ReqData, State};
        [{id, "max"}] ->
            {"<html><body>I am repl delay service, I need erlydtl for maxg</body></html>", ReqData, State}
    end.

json_body(QS) ->
     mochijson2:encode({struct, QS}).
