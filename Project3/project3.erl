-module(project3).

- export([start/2, server/0, chord_node/1]).



resolve_chord_id(Id, ChordSize, Step) ->
    % Id is where we are CURRENTLY
    % M is used to get chord size to check all positions
    % StepAtom = erlang:list_to_atom(erlang:integer_to_list(Step)),
    NextNode = whereis(erlang:list_to_atom(erlang:integer_to_list(Step))),
    %NextStep = erlang:atom_to_list(list_to_integer(Step)) + 1,
    %NextStepAtom = erlang:list_to_atom(erlang:integer_to_list(NextStep)),
    if
        NextNode == undefined ->
            resolve_chord_id(Id, ChordSize, Step + 1 rem ChordSize);
        true ->
            %io:fwrite("NOT UNDEFINED"),
            NextNode
    end.



make_finger_table(I, M, _, FingerTable) when I == M-1 ->
    FingerTable;
make_finger_table(I, M, Id, FingerTable) when I < M ->
    ChordSize = erlang:trunc(math:pow(2, M)),
    Step = erlang:trunc(Id + math:pow(2, I)),
    NextNode = resolve_chord_id(Id, ChordSize, Step),
    % find if a pid exists for Step - if not, check next int until back at self
    NewFingerTable = lists:append([{Step, NextNode}], FingerTable),
    make_finger_table(I+1, M, Id, NewFingerTable).



chord_node(M) ->
    Id = binary:decode_unsigned(crypto:hash(sha256,erlang:pid_to_list(self())))
        rem erlang:trunc(math:pow(2,M)),
    FingerTable = make_finger_table(0, M, Id, []),
    %io:format("~p   ~p~n", [self(), Id]).
    receive
        test ->
            io:fwrite("~p   ~p   ~p~n", [self(), Id, FingerTable])
    end,
    chord_node(M).



%% will literally just sort the hash values (IDs) and generate and send
%% finger tables to the nodes
make_chord_ring(NodeList) ->
    % keys are just all possible positions on the chord ring
    %io:fwrite("~p~n", [erlang:registered()]),
    %global:registered_names(),
    io:fwrite("~p~n", [NodeList]).



spawn_node(0, From, NodeList, _) ->
    From ! {finished_spawning, NodeList};
spawn_node(NumNodes,From, NodeList, M) when NumNodes > 0 ->
    Pid = spawn(?MODULE, chord_node, [M]),
    Id = binary:decode_unsigned(crypto:hash(sha256,erlang:pid_to_list(Pid)))
        rem erlang:trunc(math:pow(2,M)),
    register(erlang:list_to_atom(erlang:integer_to_list(Id)), Pid),
    NewNodeList = lists:append([{Id, Pid}], NodeList),
    spawn_node(NumNodes - 1, From, NewNodeList, M).



% the server is here just to manage the initalization of everything
server() ->
    receive
        {spawn_nodes, NumNodes, M} ->
            spawn_node(NumNodes, self(), [], M);
        {finished_spawning, NodeList} ->
            make_chord_ring(NodeList)
    end,
    server().



% NumNodes is the number of nodes to generate
% NumRequests is the number of requests each node will make
% TODO need to make sure to pass NumRequests as well
start(NumNodes, NumRequests) ->
    % M = erlang:trunc(math:ceil(math:sqrt(NumNodes))),
    M = 4,
    ChordSize = erlang:trunc(math:pow(2, M)),
    io:fwrite("M: ~p   Chord: ~p   Requests: ~p~n", [M, ChordSize, NumRequests]),
    register(chord_host, spawn(?MODULE, server, [])),
    chord_host ! {spawn_nodes, NumNodes, M}.
