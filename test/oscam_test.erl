-module(oscam_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
	ok = application:start(oscam_port).

all_test_() ->
	{"All tests",
		{setup,
		fun setup/0,
			[fun test_des_login_key_get/0,
			fun test_md5_crypt/0]}}.

test_des_login_key_get() ->
	Init = <<16#1b72fda24b4e538c4780cd4db6a4:112>>,
	Ncd = <<16#0102030405060708091011121314:112>>,
	{ok, <<136,56,246,34,81,200,127,0,117,176,166,100,167,114,136,0>>} =
		oscam:des_login_key_get(Init,Ncd).

test_md5_crypt() ->
	Password = <<"password">>,
	{ok, <<"$1$abcdefgh$G//4keteveJp0qb8z2DxG/">>} = oscam:md5_crypt(Password).
