-module(project3).

-export([start/2, server/1, chord_node/1]).

get_id(Pid, M) ->
    Hash = binary:decode_unsigned(crypto:hash(sha256,erlang:pid_to_list(Pid))),
    Hash rem erlang:trunc(math:pow(2,M)).

resolve_chord_id(Id, ChordSize, Step) ->
    chord_ring ! {resolve_pid, self(), Step},
    receive
        {id_result, NextNode} ->
            if
                NextNode == false ->
                    resolve_chord_id(Id, ChordSize, Step + 1 rem ChordSize);
                true ->
                    NextNode
            end
    end.

make_finger_table(I, M, _, FingerTable) when I == M-1 ->
    FingerTable;
make_finger_table(I, M, Id, FingerTable) when I < M ->
    ChordSize = erlang:trunc(math:pow(2, M)),
    Step = erlang:trunc(Id + math:pow(2, I)),
    NextNode = resolve_chord_id(Id, ChordSize, Step),
    {_, NextNodePid} = NextNode,
    NewFingerTable = lists:append([{Step, NextNodePid}], FingerTable),
    make_finger_table(I+1, M, Id, NewFingerTable).

chord_node(M) ->
    Id = get_id(self(), M),
    FingerTable = make_finger_table(0, M, Id, []),
    io:format("~p   ~p~n", [self(), Id]),
    receive
        test ->
            io:fwrite("~p   ~p   ~p~n", [self(), Id, FingerTable])
    end,
    chord_node(M).

spawn_node(0, _) ->
    chord_ring ! finished_spawning;
spawn_node(NumNodes, M) when NumNodes > 0 ->
    Pid = spawn(?MODULE, chord_node, [M]),
    Id = get_id(Pid, M),
    chord_ring ! {update_node_list, [{Id, Pid}]},
    %register(get_node_name(Id), Pid),
    spawn_node(NumNodes - 1, M).

% the server is here just to manage the initalization of everything
% server will manage a list of PIDs to IDs instead of registering to atoms
server(NodeList) ->
    receive
        {spawn_nodes, NumNodes, M} ->
            spawn_node(NumNodes, M),
            NewNodeList = NodeList;
        {update_node_list, [{Id, Pid}]} ->
            NewNodeList = lists:append([{Id, Pid}], NodeList);
        {resolve_pid, From, Step} ->
            From ! {id_result, lists:keyfind(Step, 1, NodeList)},
            NewNodeList = NodeList;
        finished_spawning ->
            io:fwrite("finished spawning~n"),
            NewNodeList = NodeList
    end,
    server(NewNodeList).

% NumRequests is the number of requests each node will make
% TODO need to make sure to pass NumRequests as well
% TODO as a new node is generated the tables for relevant nodes should update
start(NumNodes, NumRequests) ->
    % M = erlang:trunc(math:ceil(math:sqrt(NumNodes))),
    M = 6,
    io:fwrite("M: ~p   Requests: ~p~n", [M, NumRequests]),
    register(chord_ring, spawn(?MODULE, server, [[]])),
    chord_ring ! {spawn_nodes, NumNodes, M}.