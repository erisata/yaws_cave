%%%
%%%
%%%
-module(auth_http_login).

-compile([{parse_transform, lager_transform}]).
-export([auth/2]).
-export([make_jwt/2, check_jwt/1]).
-include_lib("yaws/include/yaws_api.hrl").
-include_lib("public_key/include/public_key.hrl").


%%% ============================================================================
%%% API Functions.
%%% ============================================================================

%%
%%
%%
auth(Arg = #arg{req = Req, headers = #headers{authorization = Authorization, other = OtherHeaders}}, _Auth) ->
    case Authorization of
        {undefined, undefined, OrigAuthHeader} ->
            % Use header based authentication for all the REST API.
            % It is safer to use header based auth for actions with side effects.
            % See more on CSRF attacks.
            case string:tokens(OrigAuthHeader, " ") of
                ["Bearer", JWT] ->
                    case check_jwt(JWT) of
                        {ok, _UserId}    -> true;
                        {error, _Reason} -> false
                    end;
                _OtherTokens ->
                    false
            end;
        {Username, Password, _OrigAuthHeader} ->
            case auth_type:login(Username, Password) of
                {ok, _Credentials} -> true;
                _Other             -> false
            end;
        undefined ->
            RequestMethod = yaws_api:http_request_method(Req),
            HaveWSUpgrade = [] =/= [ ok || {http_header, _, 'Upgrade', _, "websocket"} <- OtherHeaders ],
            case {RequestMethod, HaveWSUpgrade} of
                {'GET', true} ->
                    % Use cookie based authentication when initiating web sockets.
                    % WebSockets have no way to provide authentication headers on connect.
                    case auth_login:get_auth_token(Arg) of
                        {ok, _UserId}    -> true;
                        {error, _Reason} -> false
                    end;
                _ ->
                    lager:warning(
                        "Non-WebSocket request rejected, had no auth header: ~p-~p.",
                        [RequestMethod, yaws_api:http_request_path(Req)]
                    ),
                    false
            end
    end.


%%
%%
%%
make_jwt(UserId, UserName) ->
    Issuer  = auth_app:get_env(jwt_issuer, erlang:atom_to_list(auth_app:name())),
    ExpSecs = auth_app:get_env(jwt_validity, 3600),
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ZeroSecs = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    JWTHeader = json_base64_encode(#{
        alg => <<"RS256">>,
        typ => <<"JWT">>
    }),
    JWTClaims = json_base64_encode(#{
        iss  => erlang:iolist_to_binary(Issuer),
        sub  => erlang:iolist_to_binary(UserId),
        name => erlang:iolist_to_binary(UserName),
        exp  => NowSecs - ZeroSecs + ExpSecs,
        iat  => NowSecs - ZeroSecs
    }),
    JWTPayload = <<JWTHeader/binary, $., JWTClaims/binary>>,
    JWTSignature = make_signature(JWTPayload),
    JWT = <<JWTPayload/binary, $., JWTSignature/binary>>,
    {ok, JWT}.


%%
%%
%%
check_jwt(JWT) when is_list(JWT) ->
    check_jwt(erlang:iolist_to_binary(JWT));

check_jwt(JWT) when is_binary(JWT) ->
    case binary:split(JWT, <<$.>>, [global]) of
        [JWTHeader, JWTClaims, JWTSignature] ->
            JWTPayload = <<JWTHeader/binary, $., JWTClaims/binary>>,
            case check_signature(JWTPayload, base64url_decode(JWTSignature)) of
                true ->
                    Creds = jiffy:decode(base64url_decode(JWTClaims), [return_maps]),
                    lager:debug("xxxxxxxxxxx Creds=~p", [Creds]),
                    case Creds of
                        #{<<"sub">> := UserId} ->
                            {ok, erlang:binary_to_list(UserId)};
                        Claims ->
                            {error, {bad_jwt_claims, Claims}}
                    end;
                false ->
                    {error, bad_jwt_signature}
            end;
        _ ->
            {error, {bad_jwt_structure, JWT}}
    end.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%
%%
%%
make_signature(Payload) ->
    {ok, Filename} = auth_app:get_env(jwt_key_file),
    {ok, Password} = auth_app:get_env(jwt_key_pass),
    {ok, KeyFile} = file:read_file(Filename),
    [PrivateKeyEntry] = public_key:pem_decode(KeyFile),
    PrivateKeyDecoded = public_key:pem_entry_decode(PrivateKeyEntry, Password),
    base64url_encode(public_key:sign(Payload, sha256, PrivateKeyDecoded)).


%%
%%
%%
check_signature(Payload, Signature) ->
    {ok, Filename} = auth_app:get_env(jwt_pub_file),
    {ok, PublicKey} = file:read_file(Filename),
    [PublicKeyEntry] = public_key:pem_decode(PublicKey),
    PublicKeyDecoded = public_key:pem_entry_decode(PublicKeyEntry),
    public_key:verify(Payload, sha256, Signature, PublicKeyDecoded).


%%
%%
%%
json_base64_encode(JsonTerm) ->
    base64url_encode(jiffy:encode(JsonTerm)).


%%
%%
%%
base64url_encode(Data) ->
    Base64 = base64:encode(Data),
    ToUrl1 = binary:replace(Base64, <<"+">>, <<"-">>, [global]),
    ToUrl2 = binary:replace(ToUrl1, <<"/">>, <<"_">>, [global]),
    ToUrl3 = binary:replace(ToUrl2, <<"=">>, <<"">>,  [global]),
    ToUrl3.


%%
%%
%%
base64url_decode(Base64Url) when is_binary(Base64Url) ->
    Base64Url0 = binary:replace(Base64Url,  <<"-">>, <<"+">>, [global]),
    Base64Url1 = binary:replace(Base64Url0, <<"_">>, <<"/">>, [global]),
    Base64Url2 = case size(Base64Url1) rem 4 of
        0 -> Base64Url1;
        1 -> <<Base64Url1/binary, $=, $=, $=>>;
        2 -> <<Base64Url1/binary, $=, $=>>;
        3 -> <<Base64Url1/binary, $=>>
    end,
    base64:decode(Base64Url2).


