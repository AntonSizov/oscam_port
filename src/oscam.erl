-module(oscam).

-behaviour(gen_server).

%% API Functions Exports
-export([
	start_link/0,
	des_login_key_get/2,
	md5_crypt/1,
	des_encrypt/2,
	des_decrypt/2
]).

%% Tests
-export([
	test/0
]).

%% gen_server Function Exports
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
    terminate/2,
	code_change/3
]).

-record(st, {
    port :: pid()
}).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec des_login_key_get(binary(), binary()) -> {ok, binary()}.
des_login_key_get(InitKey = <<_:112>>, NcdKey = <<_:112>>) ->
    gen_server:call(?MODULE, {req, des_login_key_get, [InitKey, NcdKey]}).

-spec md5_crypt(binary()) -> {ok, binary()}.
md5_crypt(Password) when is_binary(Password) ->
	gen_server:call(?MODULE, {req, md5_crypt, [Password]}).

-spec des_encrypt(binary(), binary()) -> {ok, binary()}.
des_encrypt(DesKey = <<_:128>>, Binary) when is_binary(Binary) ->
	gen_server:call(?MODULE, {req, des_encrypt, [DesKey, Binary]}).

-spec des_decrypt(binary(), binary()) -> {ok, binary()}.
des_decrypt(DesKey = <<_:128>>, Binary) when is_binary(Binary) ->
	gen_server:call(?MODULE, {req, des_decrypt, [DesKey, Binary]}).

%% ===================================================================
%% GenServer Function Definitions
%% ===================================================================

init([]) ->
	PrivDir = code:priv_dir(oscam_port),
	Name = filename:absname("oscam", PrivDir),
    Port = open_port({spawn, Name}, [{packet, 2}, binary]),
    {ok, #st{port = Port}}.

handle_call({req, F, Args}, _From, St = #st{port = Port}) ->
	Port ! {self(), {command, pack_buff(F, Args)}},
	receive
		{Port, {data, Data}} ->
			{reply, {ok, unpack_buff(Data)}, St};
		Response ->
			error_logger:error_report("Got invalid response",
										{response, Response}),
			{stop, {invalid_response, Response}, St}
	end;

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

pack_buff(des_login_key_get, [InitKey, NcdKey]) -> [1, InitKey, NcdKey];
pack_buff(md5_crypt, [Pass]) -> [2, Pass, 0];
pack_buff(des_encrypt, [DesKey, Binary]) -> [3, DesKey, Binary];
pack_buff(des_decrypt, [DesKey, Binary]) -> [4, DesKey, Binary].

unpack_buff([Result]) -> Result;
unpack_buff(AnyThing) -> AnyThing.

%% ===================================================================
%% Test
%% ===================================================================

-spec test() -> ignore.
test() ->
	test_des_login_key_get(),
	test_md5_crypt().

test_des_login_key_get() ->
	Init = <<16#1b72fda24b4e538c4780cd4db6a4:112>>,
	Ncd = <<16#0102030405060708091011121314:112>>,
	{ok, DesKey = <<136,56,246,34,81,200,127,0,117,176,166,100,167,114,136,0>>} =
		des_login_key_get(Init,Ncd),
	io:format("des_login_key_get OK. DesKeyGot: ~p~n", [DesKey]).

test_md5_crypt() ->
	Password = <<"password">>,
	{ok, Passcrypt = <<"$1$abcdefgh$G//4keteveJp0qb8z2DxG/">>} = md5_crypt(Password),
	io:format("test_md5_crypt OK. Passcrypt: ~p~n", [Passcrypt]).
