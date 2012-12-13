-module(mblocks_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-include_lib("public_key/include/public_key.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    %% Generate rsa keypair and save to ets memory table
    %% Needed for auth and protocol encryption
    %% TODO: It's right place for this code ?
    ets:new(mb_rsa_key, [named_table, {read_concurrency, true}]),
    {ok, PrivKey, PubKey} = gen_rsa_keypair(1024),
    {_, RsaPubBin, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', PubKey),
    ets:insert(mb_rsa_key, [{private, PrivKey}, {public, PubKey}, {public_bin, RsaPubBin}]),

	{ok, {{one_for_one, 10, 10}, []}}.


-spec gen_rsa_keypair(pos_integer()) -> {ok, rsa_private_key(), rsa_public_key()}.
gen_rsa_keypair(BitLength) ->
    PemBin = list_to_binary(os:cmd("openssl genrsa " ++ integer_to_list(BitLength) ++  " 2>/dev/null")),
    [RSAEntry] = public_key:pem_decode(PemBin),
    PrivKey = public_key:pem_entry_decode(RSAEntry),
    PubKey = get_pubkey_from_priv(PrivKey),
    {ok, PrivKey, PubKey}.

-spec get_pubkey_from_priv(rsa_private_key()) -> rsa_public_key().
get_pubkey_from_priv(#'RSAPrivateKey'{modulus=M, publicExponent=P}) ->
    #'RSAPublicKey'{
        modulus=M,
        publicExponent=P}.
