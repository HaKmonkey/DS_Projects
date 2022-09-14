-module(project1).

-export([start_worker/1, start_server/1, mine_coin/2, worker/1, server/1]).

mine_coin(From, K) ->
    Rand_string = "jonathan.bravo;" ++ base64:encode(crypto:strong_rand_bytes(6)),
    Hash_string = io_lib:format("~64.16.0b",[binary:decode_unsigned(crypto:hash(sha256, Rand_string))]),
    Zero_check = lists:concat(lists:duplicate(K, "0")),
    Prefix = string:slice(Hash_string, 0, K),
    B = string:equal(Prefix,Zero_check),
    io:fwrite("~s ~s~n", [Rand_string, Hash_string]),
    if 
        B -> From ! {found_coin, Rand_string, Hash_string};
        true -> mine_coin(From, K)
    end.

spawn_mining(0, _, _) ->
    done;
spawn_mining(I, K, From) when I > 0 ->
    spawn(?MODULE, mine_coin, [From, K]),
    spawn_mining(I-1, K, From).

worker(Server) ->
    {mining_server, Server} ! {send_start, self()},
    io:fwrite("send worker message"),
    receive
        {start_mining_server, K, Server} ->
            io:fwrite("got message"),
            spawn_mining(8, K, Server)
    end,
    worker(Server).

server(K) ->
    receive
        {found_coin, Rand_string, Hash_string} ->
            io:fwrite("~s\t~s~n", [Rand_string, Hash_string]);
        {start_mining, K} ->
            spawn_mining(8, K, self());
        {send_start, Worker} ->
            io:fwrite("Got message"),
            Worker ! {start_mining_server, K, self()}
    end,
    server(K).

start_server(K) ->
    register(mining_server, spawn(?MODULE, server, [K])).

start_worker(Server) ->
    spawn(?MODULE, worker, [Server]).