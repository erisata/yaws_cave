%%%
%%%
%%%
-module(yaws_cave_example).
-compile([{parse_transform, lager_transform}]).
-export([out/1, handle_request/4]).
-include_lib("yaws/include/yaws_api.hrl").

-define(URL_ROOT, "auth").
-define(MEDIATYPE_JSON, "application/json").
-define(MEDIATYPE_HTML, "text/html").
-define(MEDIATYPE_PLAIN, "text/plain").


%%
%%
%%
out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(Arg#arg.req),
    lager:debug("Handling request: path=~p, params=~p, method=~p", [Path, Uri#url.querypart, Method]),
    yaws_cage_rest:out(?MODULE, Arg, #{
        debug => true
    }).


%%
%%
%%
handle_request(["admin"], 'GET', _Arg, _Opts) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_PLAIN, [
            "Admin\n"
        ]}
    ];

handle_request(["login"], 'GET', _Arg, _Opts) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_PLAIN, [
            "login\n"
        ]}
    ].


