-module(project1).

% -export([worker/0, encode_hash/3, generate_string/2, check_hash/3, start/1]).

% generate_string(Worker) ->
%     Byte_num = rand:uniform(100),
%     Rand_string = "jonathan.bravo;" ++ binary:bin_to_list(binary:encode_hex(crypto:strong_rand_bytes(Byte_num))),
%     Worker ! {encode_hash, Rand_string}.

% encode_hash(Worker, Rand_string) ->
%     Hash_string = erlang:integer_to_list(binary:decode_unsigned(crypto:hash(sha256, Rand_string)), 16),
%     Worker ! {check_hash, Hash_string}.

% check_hash(Worker, Hash_string, K) ->
%     Prefix = lists:concat(lists:duplicate(K, "0")),
%     B = string:find(Hash_string, Prefix) =:= Hash_string,
%     if
%         B ->
%             io:fwrite("~s~n~n",[Hash_string]),
%             Worker ! {write_hash, Hash_string};
%         true ->
%             io:fwrite("~w~n~n", [Worker]),
%             Worker ! {gen_string}
%         end.

% % will take a value K
% worker(K) ->
%     receive
%         {gen_string} ->
%             generate_string(self()),
%             worker(K);
%         {encode_hash, Rand_string} ->
%             encode_hash(self(), Rand_string),
%             worker(K);
%         {check_hash, Hash_string, K} ->
%             check_hash(self(), Hash_string, K),
%             worker(K);
%         {write_hash, Hash_string} ->
%             io:fwrite("~s~n",[Hash_string]),
%             worker(K)
%     end.

-export([start/1]).

mine_coin(Host, K) ->
    %Byte_num = rand:uniform(100),
    Rand_string = "jonathan.bravo;" ++ binary:bin_to_list(binary:encode_hex(crypto:strong_rand_bytes(10))),
    Hash_string = erlang:integer_to_list(binary:decode_unsigned(crypto:hash(sha256, Rand_string)), 16),
    %io:fwrite(Hash_string),
    Prefix = lists:concat(lists:duplicate(K, "0")),
    B = string:find(Hash_string, Prefix) =:= Hash_string,
    if
        B -> 
            Host ! {Rand_string, Hash_string};
            %io:fwrite("~s~n~n",[Hash_string]);
        true ->
            mine_coin(Host, K)
    end.

% will take a value K
worker(K) ->
    mine_coin(self(), K),
    receive
        {Rand_string, Hash_string} ->
            % Host ! {Rand_string, Hash_string},
            io:fwrite("~s    ~s~n~n", [Rand_string, Hash_string]),
            worker(K)
    end.


% server(K) ->
%     receive
%         spawn_worker ->
%             spawn(fun() -> worker(self(), K) end);
%         {Rand_string, Hash_string} ->
%             io:fwrite("~s    ~s~n~n", [Rand_string, Hash_string]),
%             server(K)
%    end.

start(K) ->
    spawn(fun() -> worker(K) end).
    % Server = spawn(fun() -> server(K) end),
    % Server ! spawn_worker.

    %io:fwrite("~w~n", [Worker1]),
