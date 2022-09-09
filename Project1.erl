-module(test).

-import(list, [duplicate/2]).

-export([worker/1, encode_hash/2, generate_string/1, check_hash/3, start/1]).

generate_string(Worker) ->
    Byte_num = rand:uniform(100),
    Rand_string = "jonathan.bravo;" ++ binary:bin_to_list(binary:encode_hex(crypto:strong_rand_bytes(Byte_num))),
    Worker ! {gen_string, Rand_string}.

encode_hash(Worker, Rand_string) ->
    Hash_string = erlang:integer_to_list(binary:decode_unsigned(crypto:hash(sha256, Rand_string)), 16),
    Worker ! {encode_hash, Hash_string}.
    %Hash ! Hash_string.

check_hash(Worker, K, Hash_string) ->
    Prefix = lists:concat(lists:duplicate(K, "0")),
    B = string:find(Hash_string, Prefix) =:= Hash_string,
    if
        B ->
            Worker ! {check_hash, true, Hash_string}; 
        true ->
            Worker ! {check_hash, false}
        end.

% will take a value K
worker(K) ->
    receive
        {check_hash, false} ->
            generate_string(self()),
            worker(K);
        {gen_string, Rand_string} ->
            encode_hash(self(), Rand_string),
            worker(K);
        {encode_hash, Hash_string} ->
            check_hash(self(), K, Hash_string),
            worker(K);
        {check_hash, true, Hash_string} ->
            io:fwrite("~s~n",[Hash_string])
    end.


start(K) ->
    Worker1 = spawn(fun() -> worker(K) end),
    generate_string(Worker1).




% check_hash(K, G_hash) ->
%     Prefix = lists:concat(lists:duplicate(K, "0")),
%     B = string:find(G_hash, Prefix) =:= G_hash,
%     if
%         B ->
%             %io:fwrite("~s",["Success"]);
%             %H_state = true;
%         end.



% hash_test(S) ->
%     Hash_string = erlang:integer_to_list(
%         binary:decode_unsigned(
%             crypto:hash(sha256, S)), 16),
%     io:fwrite("~s\t~p", [S, Hash_string]).