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
      {"text/html", to_html},
      {"application/json", to_json}
     ], RD, Ctx}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(RD, Ctx) ->
    PathInfo = wrq:path_info(RD),
    case PathInfo of
        [] ->
            {json_body(repl_delay_core_server:get_replication_delays()), RD, Ctx};
        [{id, "min"}] ->
            {json_body([repl_delay_core_server:get_min_replication_delay()]), RD, Ctx};
        [{id, "max"}] ->
            {json_body([repl_delay_core_server:get_max_replication_delay()]), RD, Ctx}
    end.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(RD, Ctx) ->
    PathInfo = wrq:path_info(RD),
    case PathInfo of
        [] ->
            {json_body(repl_delay_core_server:get_replication_delays()), RD, Ctx};
        [{id, "min"}] ->
	    {min, Val} = repl_delay_core_server:get_min_replication_delay(),
	    {float_to_list(float(Val), [{decimals, 4}, compact]) ++ "\n", RD, Ctx};
        [{id, "max"}] ->
	    {max, Val} = repl_delay_core_server:get_max_replication_delay(),
	    {float_to_list(float(Val), [{decimals, 4}, compact]) ++ "\n", RD, Ctx}
    end.

json_body(QS) ->
     mochijson2:encode({struct, QS}).
