-module(project3).

- export([start/2, chord_node/0]).

chord_node() ->
    Id = io_lib:format(
        "~64.16.0b",[
            binary:decode_unsigned(
                crypto:hash(
                    sha3_256,
                    erlang:pid_to_list(self())
                )
            )
        ]
    ), % node id from hashed ip; here we use sha256 instead of sha1
    % get message about finding successor - given id
    io:fwrite("~p~n", [Id]).

% make_chord_ring() ->
%     done.

spawn_node(0) ->
    done;
spawn_node(I) when I > 0 ->
    Pid = spawn(?MODULE, chord_node, []),
    Id = io_lib:format(
        "~64.16.0b",[
            binary:decode_unsigned(
                crypto:hash(
                    sha3_256,
                    erlang:pid_to_list(Pid)
                )
            )
        ]
    ),
    % global:register_name(Id, Pid),
    % Gpid = global:whereis_name(Id),
    io:format("ID: ~p~n",[Id]),
    spawn_node(I-1).

% NumNodes is the number of nodes to generate
% NumRequests is the number of requests each node will make
start(NumNodes, NumRequests) ->
    M = erlang:trunc(math:ceil(math:sqrt(NumNodes))),
    ChordSize = erlang:trunc(math:pow(2, M)),
    io:fwrite("M: ~p   Chord: ~p   Requests: ~p~n", [M, ChordSize, NumRequests]),
    % persistent_term:put(nodes, dict:new()),
    spawn_node(NumNodes).
