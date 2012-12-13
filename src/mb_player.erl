-module(mb_player).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("include/mc_client_packets.hrl").
-include("include/mc_server_packets.hrl").
-include("include/mc_two_way_packets.hrl").

-record(state, {
    writer,
    username
    }).

-record(auth_state, {
    writer,
    verify_token,
    eid,
    username
}).

send_reply_pkt(Writer, Packet) ->
    Writer ! {packet, Packet}.

%%% Client API
start_link(Args, _Opts) ->
    gen_server:start_link(?MODULE, Args, []).

%%% Server functions
init([Writer]) ->
    {ok, #auth_state{writer=Writer}}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({pkt, #handshake{username=UserName, protocol_version=49}}, State) ->
    %% FIXME: check username
    PubBin = ets:lookup_element(mb_rsa_key, public_bin, 2),
    ServerId = integer_to_list(crypto:rand_uniform(0, 4294967296) bsl 32
                + crypto:rand_uniform(0, 4294967296), 16), % 4294967296 is 2^32

    VerifyToken = crypto:rand_bytes(4),
    NewState = State#auth_state{username=UserName, verify_token=VerifyToken, eid=ServerId},
    send_reply_pkt(State#auth_state.writer, #encryption_key_request{
                server_id = ServerId,
                public_key_length = byte_size(PubBin),
                public_key = PubBin,
                verify_token_length = 4,
                verify_token = VerifyToken
            }).
    {noreply, NewState};

handle_cast({pkt, #handshake{protocol_version=ClientVersion}}, State) ->
    if
        ClientVersion > 49 ->
            {stop, "Outdated server!", State};
        ClientVersion < 49 ->
            {stop, "Outdated client!", State}
    end;

handle_cast({pkt, #encryption_key_response{
                        shared_secret=SharedSecretEncrypted,
                        verify_token_response=VerifyTokenResponse}}, State) ->
    PrivateKey = ets:lookup_element(mb_rsa_key, private, 2),
    VerifyToken = public_key:decrypt_private(VerifyTokenResponse, PrivateKey),
    if
      VerifyToken =/= State#auth_state.verify_token ->
        {stop, "Invalid client reply", State}.
      true ->
    end,
    SharedSecret = public_key:decrypt_private(SharedSecretEncrypted, PrivateKey),
    send_reply_pkt(State#auth_state.writer, #encryption_key_response{
            shared_secret_length = 0,
            shared_secret = <<>>,
            verify_token_length = 0,
            verify_token_response = <<>>
        });

handle_cast({pkt, #client_statuses{payload=0}}, State) -> %% Initial spawn
    %% TODO: If server.IsOnlineMode()
    check_minecraft_session(State#auth_state.eid),
    {noreply, State};

handle_cast({pkt, Packet}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok;

terminate(Reason, State) ->
    Writer = element(2, State),
    Writer ! {stop, Reason},
    % TODO: message in chat if client logged in.
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

%%% Private functions


check_minecraft_session(ServerId) ->
    PublicBin = ets:lookup_element(mb_rsa_key, public_bin, 2),
    ShaCtx = crypto:sha_init(),
    ShaCtx2 = crypto:sha_update(ShaCtx, ServerId),
    ShaCtx3 = crypto:sha_update(ShaCtx2, Param2)
    ShaCtx4 = crypto:sha_update(ShaCtx3, PublicBin),

