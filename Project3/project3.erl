-module(project3).

-export([start/2, server/1, chord_node/2]).

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
                    {_, NextNodePid} = NextNode,
                    NextNodePid
            end
    end.

make_finger_table(I, M, _, FingerTable) when I == M-1 ->
    lists:sort(FingerTable);
make_finger_table(I, M, Id, FingerTable) when I < M ->
    ChordSize = erlang:trunc(math:pow(2, M)),
    Step = erlang:trunc(Id + math:pow(2, I)),
    NextNode = resolve_chord_id(Id, ChordSize, Step),
    NewFingerTable = lists:append([{Step, NextNode}], FingerTable),
    %NextNode = resolve_chord_id(Id, ChordSize, Step),
    make_finger_table(I+1, M, Id, NewFingerTable).

chord_node(M, FingerTable) ->
    Id = get_id(self(), M),
    receive
        test ->
            NewFingerTable = make_finger_table(0, M, Id, FingerTable),
            io:fwrite("~p   ~p   ~p~n", [self(), Id, FingerTable]);
        update ->
            NewFingerTable = make_finger_table(0, M, Id, []),
            io:format("~p   ~p   ~p~n", [self(), Id, FingerTable])
    end,
    chord_node(M, NewFingerTable).

spawn_node(0, _) ->
    chord_ring ! finished_spawning;
spawn_node(NumNodes, M) when NumNodes > 0 ->
    Pid = spawn(?MODULE, chord_node, [M, []]),
    Id = get_id(Pid, M),
    io:fwrite("~p~p~n",[Id, Pid]),
    chord_ring ! {update_node_list, Id, Pid},
    spawn_node(NumNodes - 1, M).

% the server is here just to manage the initalization of everything
% server will manage a list of PIDs to IDs instead of registering to atoms
update_nodes(NodeList) ->
    Fun = fun(Pid) -> Pid ! update end,
    lists:keymap(Fun, 2, NodeList).

server(NodeList) ->
    receive
        {spawn_nodes, NumNodes, M} ->
            spawn_node(NumNodes, M),
            NewNodeList = NodeList;
        {update_node_list, Id, Pid} ->
            NewNodeList = lists:append([{Id, Pid}], NodeList),
            %Pid ! update;
            update_nodes(NewNodeList);
        {resolve_pid, From, Step} ->
            %io:fwrite("NL: ~p~n",[NodeList]),
            From ! {id_result, lists:keyfind(Step, 1, NodeList)},
            NewNodeList = NodeList;
        finished_spawning ->
            io:fwrite("finished spawning~n"),
            NewNodeList = NodeList
    end,
    server(NewNodeList).

% NumRequests is the number of requests each node will make
% TODO should be able to insert or remove nodes now
% TODO need to make sure to pass NumRequests as well
%% Nodes should make requests and tell us how many jumps were needed I think
start(NumNodes, NumRequests) ->
    % M = erlang:trunc(math:ceil(math:sqrt(NumNodes))),
    M = 6,
    io:fwrite("M: ~p   Requests: ~p~n", [M, NumRequests]),
    register(chord_ring, spawn(?MODULE, server, [[]])),
    chord_ring ! {spawn_nodes, NumNodes, M}.