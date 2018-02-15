%%%
%%%
%%%
-module(auth_example).
-compile([{parse_transform, lager_transform}]).
-export([out/1, handle_request/4]).
-include_lib("yaws/include/yaws_api.hrl").

-define(URL_ROOT, "auth").
-define(MEDIATYPE_JSON, "application/json").
-define(MEDIATYPE_HTML, "text/html").


%%
%%
%%
out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(Arg#arg.req),
    lager:debug("Handling request: path=~p, params=~p, method=~p", [Path, Uri#url.querypart, Method]),
    auth_login:out(Path, Method, Arg).


%%
%%
%%
handle_request(["test"], 'GET', _Arg, _Opts) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_HTML, [
            "<html>\n",
            "    <head>\n",
            "        <title>Test page</title>\n",
            "    </head>\n",
            "    <body>\n",
            "        <h1>Testing</h1>\n",
            "    </body>\n",
            "</html>\n"
        ]}
    ].