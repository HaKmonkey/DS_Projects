-module(project3).

-include("tools.hrl").
-export([start/2, server/1, chord_node/3, loopsearch/4, loop_node/2]).

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

make_finger_table(I, M, _, FingerTable) when I == M ->
  lists:sort(FingerTable);
make_finger_table(I, M, Id, FingerTable) when I < M ->
  ChordSize = erlang:trunc(math:pow(2, M)),
  Step = erlang:trunc(Id + math:pow(2, I)),
  NextNode = resolve_chord_id(Id, ChordSize, Step rem ChordSize),
  NewFingerTable = lists:append([{Step, NextNode}], FingerTable),
  make_finger_table(I+1, M, Id, NewFingerTable).

chord_node(M, FingerTable, NumRequests) ->
  Id = get_id(self(), M),
  receive
    print_self ->
      io:fwrite("~p   ~p   ~p~n", [self(), Id, FingerTable]),
      NewFingerTable = FingerTable;
    update ->
      NewFingerTable = make_finger_table(0, M, Id, []);
    {start_lookup, Key} ->
      if
        Key -> ;
        true -> 
      end
      io:fwrite("start look up: ~p   ~p   ~p ~p ~n", [self(), Id, FingerTable, NumRequests]),
      NewFingerTable = FingerTable;
  end,
  chord_node(M, NewFingerTable, NumRequests).

spawn_node(0, _, NumRequests) ->
  chord_ring ! {finished_spawning, NumRequests};
spawn_node(NumNodes, M, NumRequests) when NumNodes > 0 ->
  Pid = spawn(?MODULE, chord_node, [M, [], NumRequests]),
  Id = get_id(Pid, M),
  chord_ring ! {update_node_list, Id, Pid},
  spawn_node(NumNodes - 1, M, NumRequests).

% the server is here just to manage the initalization of everything
% server will manage a list of PIDs to IDs instead of registering to atoms
update_nodes(NodeList) ->
  Fun = fun(Pid) -> Pid ! update end,
  lists:keymap(Fun, 2, NodeList).

server(NodeList) ->
  receive
    {spawn_nodes, NumNodes, M, NumRequests} ->
      spawn_node(NumNodes, M, NumRequests),
      NewNodeList = NodeList;
    {update_node_list, Id, Pid} ->
      NewNodeList = lists:append([{Id, Pid}], NodeList),
      update_nodes(NewNodeList);
    {resolve_pid, From, Step} ->
      From ! {id_result, lists:keyfind(Step, 1, NodeList)},
      NewNodeList = NodeList;
    {finished_spawning, NumRequests} ->
      io:fwrite("finished spawning   ~p ~p ~n", [NodeList, NumRequests]),
      loop_node(NodeList, length(NodeList)),
      NewNodeList = NodeList
  end,
  server(NewNodeList).

% NumRequests is the number of requests each node will make
% TODO should be able to insert or remove nodes now
% TODO need to make sure to pass NumRequests as well
%% Nodes should make requests and tell us how many jumps were needed
start(NumNodes, NumRequests) ->
  % M = erlang:trunc(math:ceil(math:sqrt(NumNodes))),
  M = 6,
  io:fwrite("M: ~p   Requests: ~p~n", [M, NumRequests]),
  register(chord_ring, spawn(?MODULE, server, [[]])),
  chord_ring ! {spawn_nodes, NumNodes, M, NumRequests}.

loop_node(NodeList, 0) ->
  ok;
loop_node(NodeList, Size) when Size > 0->
  New_ID = element(2,lists:nth(Size, NodeList)),
  Temp = erlang:trunc(math:pow(2, ?M)),
  Key = random:uniform(Temp),
  New_ID ! {start_lookup, Key},
  loop_node(NodeList, Size - 1).

loopsearch(0, _, _, _) -> ok;

loopsearch(NumRequests, FingerTable, ID, Target) when NumRequests > 0->
  Temp = erlang:trunc(math:pow(2, ?M)),
  if
    Target < ID -> New_Target = Target + Temp;
    true -> New_Target = Target
  end,
  FingerMaps = maps:from_list(FingerTable),
  FingerKeys = maps:keys(FingerMaps),
  LastKey = looptable(FingerKeys,length(FingerKeys), New_Target),
  maps:get(LastKey, FingerMaps) ! {keep_loopup, Target},
  io:fwrite("FingerKeys ~p, ID ~p , target ~p ~n",[LastKey, ID, Target]),
  loopsearch(NumRequests - 1, FingerTable, ID, Target).

looptable(FingerKeys, 0, NewTarget) ->
  ok;


looptable(FingerKeys, Curr_Index, NewTarget) when Curr_Index > 1 ->
  LastKey = lists:nth(Curr_Index, FingerKeys),
  SecondKey = lists:nth(Curr_Index - 1 , FingerKeys),
  if

    (Curr_Index == length(FingerKeys)) and (NewTarget > LastKey) -> LastKey;
    true -> if
              (NewTarget =< LastKey) and (NewTarget > SecondKey) -> SecondKey;
              true -> looptable(FingerKeys, Curr_Index-1, NewTarget)
            end
  end.