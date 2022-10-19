%%%-------------------------------------------------------------------
%%% @author yuhong
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Oct 2022 4:14 PM
%%%-------------------------------------------------------------------
-module(project3).
-author("yuhong").
-include("tools.hrl").
%% API
-export([start/2, pot/1, node/0, chord_ring/1, loop_join/1]).

chord_ring(NodeList)->
  receive
    create ->
      Node_Name = create_Node(),
      Temp = "node_" ++ integer_to_list(Node_Name),
      Key = list_to_atom(Temp),
      Key ! {create, Node_Name},
      NewNodeList = lists:append([Node_Name], NodeList),
      chord_ring(NewNodeList),
      io:fwrite("Key: ~p ~n",[Key]);

    join ->
      Node_Name = create_Node(),
      Temp = "node_" ++ integer_to_list(Node_Name),
      Key = list_to_atom(Temp),
      Key ! {join, Node_Name},
      NewNodeList = lists:append([Node_Name], NodeList),
      chord_ring(NewNodeList),
      io:fwrite("Key: ~p ~n",[Key])

  end.


start(NumberNodes, NumberRequests)->
  register(chord_ring, spawn(?MODULE, chord_ring,[[]])),
  chord_ring ! create,
  loop_join(NumberNodes-1).

pot(1) -> 2;

pot(N) -> 2*pot(N-1).

node() ->
  receive
    {create,NodeID} ->
      io:fwrite("create hereee ~p ~n",[NodeID]);
    {join, NodeID} -> io:fwrite("join hereee ~p ~n", [NodeID])
  end.

create_Node() ->
  X = pot(?M),
  Pid = spawn(?MODULE, node, []),
  <<Hash:160>> = crypto:hash(sha, pid_to_list(Pid)),
  Key_Hash = Hash rem X,
  Temp = "node_" ++ integer_to_list(Key_Hash),
  register(list_to_atom(Temp),Pid),
  Key_Hash.

loop_join(0) ->
  ok;

loop_join(NumberNodesN) ->
  chord_ring ! join,
  loop_join(NumberNodesN - 1).