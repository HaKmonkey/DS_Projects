-module(project3).

-export([start/2, server/2, chord_node/2]).

get_id(Pid, M) ->
    Hash = binary:decode_unsigned(crypto:hash(sha256,erlang:pid_to_list(Pid))),
    Hash rem erlang:trunc(math:pow(2,M)).

resolve_chord_id(Id, ChordSize, Step) ->
    chord_ring ! {resolve_pid, self(), Step},
    receive
        {id_result, NextNode} ->
            if
                NextNode == false ->
                    resolve_chord_id(Id, ChordSize, (Step + 1) rem ChordSize);
                true ->
                    {_, NextNodePid} = NextNode,
                    NextNodePid
            end
    end.

make_finger_table(I, M, _, FingerTable) when I == M ->
    lists:sort(FingerTable);
make_finger_table(I, M, Id, FingerTable) when I < M ->
    ChordSize = erlang:trunc(math:pow(2, M)),
    Step = erlang:trunc(Id + math:pow(2, I)) rem ChordSize,
    NextNode = resolve_chord_id(Id, ChordSize, Step),
    NewFingerTable = lists:append([{Step, NextNode}], FingerTable),
    make_finger_table(I+1, M, Id, NewFingerTable).

make_request(Id, M, FingerTable, RequestKey, TempRK, Pid, NumJumps) ->
    ChordSize = erlang:trunc(math:pow(2, M)),
    if 
        TempRK < 0 ->
            WrappedTempRK = TempRK + 64;
        true ->
            WrappedTempRK = TempRK
    end,
    RequestNode = lists:keyfind(WrappedTempRK, 1, FingerTable),
    if
        RequestNode == false ->
            make_request(Id, M, FingerTable, RequestKey, WrappedTempRK - 1, Pid, NumJumps);
        TempRK == Id ->
            LastStep = erlang:trunc(Id + math:pow(2, M-1)) rem ChordSize,
            LastNode = lists:keyfind(LastStep, 1, FingerTable),
            io:fwrite("Id: ~p Key: ~p LastStep: ~p LastNode: ~p~n",[Id, RequestKey, LastStep, LastNode]),
            {AssumedId, LastNodePid} = LastNode,
            LastNodePid ! {forwarding_request, Pid, RequestKey, NumJumps + 1, AssumedId};
        true ->
            {AssumedId, RequestNodePid} = RequestNode,
            io:fwrite("~p jumped to ~p~n", [Id, RequestNodePid]),
            RequestNodePid ! {forwarding_request, Pid, RequestKey, NumJumps + 1, AssumedId}
    end.

successor_list(_, _, _, SList, 0) ->
    SList;
successor_list(ChordSize, AssumedId, Id, SList, X) ->
    NewSList = lists:append([AssumedId rem ChordSize], SList),
    successor_list(ChordSize, AssumedId+1, Id, NewSList, X-1).

chord_node(M, FingerTable) ->
    Id = get_id(self(), M),
    ChordSize = erlang:trunc(math:pow(2, M)),
    receive
        print_self ->
            io:fwrite("~p   ~p   ~p~n", [self(), Id, FingerTable]),
            NewFingerTable = FingerTable;
        update ->
            NewFingerTable = make_finger_table(0, M, Id, []);
        request ->
            RequestKey = rand:uniform(ChordSize) - 1,
            if
                Id == RequestKey ->
                    self() ! {accept_key, RequestKey, 0};
                true ->
                    make_request(Id, M, FingerTable, RequestKey, RequestKey, self(), 0)
            end,
            NewFingerTable = FingerTable;
        {forwarding_request, Pid, RequestKey, NumJumps, AssumedId} ->
            if
                Id == RequestKey ->
                    self() ! {give_key, Pid, RequestKey, NumJumps};
                Id > AssumedId, RequestKey =< Id ->
                    self() ! {give_key, Pid, RequestKey, NumJumps};
                Id < AssumedId ->
                    X = ((Id + ChordSize) - AssumedId) + 1,
                    SList = successor_list(ChordSize, AssumedId, Id, [], X),
                    Member = lists:member(RequestKey, SList),
                    if
                        Member == true ->
                            self() ! {give_key, Pid, RequestKey, NumJumps};
                        true ->
                            make_request(Id, M, FingerTable, RequestKey, RequestKey, Pid, NumJumps)
                    end;
                true ->
                    make_request(Id, M, FingerTable, RequestKey, RequestKey, Pid, NumJumps)
            end,
            NewFingerTable = FingerTable;
        {accept_key, RequestKey, NumJumps} ->
            io:fwrite("~p requested ~p and got in ~p jumps~n", [self(), RequestKey, NumJumps]),
            NewFingerTable = FingerTable;
        {give_key, Pid, RequestKey, NumJumps} ->
            Pid ! {accept_key, RequestKey, NumJumps},
            NewFingerTable = FingerTable
    end,
    chord_node(M, NewFingerTable).

spawn_node(0, _) ->
    chord_ring ! finished_spawning;
spawn_node(NumNodes, M) when NumNodes > 0 ->
    Pid = spawn(?MODULE, chord_node, [M, []]),
    Id = get_id(Pid, M),
    chord_ring ! {update_node_list, Id, Pid},
    spawn_node(NumNodes - 1, M).

% the server is here just to manage the initalization of everything
% server will manage a list of PIDs to IDs instead of registering to atoms
update_nodes(NodeList) ->
    Fun = fun(Pid) -> Pid ! update end,
    lists:keymap(Fun, 2, NodeList).

start_requests(_, 0) ->
    chord_ring ! finished_requests;
start_requests(NodeList, NumRequests) ->
    Fun = fun(Pid) -> Pid ! request end,
    lists:keymap(Fun, 2, NodeList),
    start_requests(NodeList, NumRequests - 1).

server(NodeList, NumRequests) ->
    receive
        {spawn_nodes, NumNodes, M} ->
            spawn_node(NumNodes, M),
            NewNodeList = NodeList;
        {update_node_list, Id, Pid} ->
            NewNodeList = lists:append([{Id, Pid}], NodeList),
            update_nodes(NewNodeList);
        {resolve_pid, From, Step} ->
            From ! {id_result, lists:keyfind(Step, 1, NodeList)},
            NewNodeList = NodeList;
        finished_spawning ->
            io:fwrite("finished spawning   ~p~n", [NodeList]),
            timer:sleep(0500),
            start_requests(NodeList, NumRequests),
            NewNodeList = NodeList;
        finished_requests ->
            io:fwrite("finished requests~n"),
            NewNodeList = NodeList
    end,
    server(NewNodeList, NumRequests).

% TODO should be able to insert or remove nodes now
start(NumNodes, NumRequests) ->
    % M = erlang:trunc(math:ceil(math:sqrt(NumNodes))),
    M = 6,
    register(chord_ring, spawn(?MODULE, server, [[], NumRequests])),
    chord_ring ! {spawn_nodes, NumNodes, M}.