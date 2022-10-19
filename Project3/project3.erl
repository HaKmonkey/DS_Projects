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
-export([start/2, pot/1, node/4, chord_ring/1, loop_join/1, shuffle/1, getNodeName/1]).

chord_ring(NodeList)->
  receive
    create ->
      Node_Name = create_Node(),
      Temp = "node_" ++ integer_to_list(Node_Name),
      Key = list_to_atom(Temp),
      Key ! {create, Node_Name},
      NewNodeList = lists:append([Node_Name], NodeList),
      chord_ring(NewNodeList);

    join ->
      Node_Name = create_Node(),
      Temp = "node_" ++ integer_to_list(Node_Name),
      Key = list_to_atom(Temp),
      Join_ID = shuffle(NodeList),
      Key ! {join, Node_Name},
      getNodeName(Join_ID) ! {join, Join_ID, Node_Name},
      NewNodeList = lists:append([Node_Name], NodeList),
      chord_ring(NewNodeList)

  end.


start(NumberNodes, NumberRequests)->
  register(chord_ring, spawn(?MODULE, chord_ring,[[]])),
  chord_ring ! create,
  timer:sleep(500),
  loop_join(NumberNodes-1).

pot(1) -> 2;

pot(N) -> 2*pot(N-1).

node(NID, Successor, Predecessor, Fingertable) ->
  io:fwrite("successor test: ~p ~n",[Successor]),
  receive
    {create,NodeID} ->
      New_Predecessor = nil,
      New_Successor = NodeID,
      io:fwrite("~p ~n",[NodeID]),
      node(NodeID, NodeID, New_Predecessor, []);
    {join, NewId} ->
      New_Predecessor = nil,
      io:fwrite("test: ~p ~p ~p ~n",[NewId, Successor]),
      if
        (NewId > NID and NewId < Successor) ->
          getNodeName(NewId) ! {NID, Successor};
        (NID == Successor) ->
          getNodeName(NewId) ! {NewId, NID};
        true -> getNodeName(Successor) ! {join, NewId}
      end;
    {join, NodeID, New_Successor} ->
      NID = NodeID,
      node(NID, New_Successor, Predecessor, Fingertable)
  end.

create_Node() ->
  X = pot(?M),
  Pid = spawn(?MODULE, node, [nil,nil,nil,[]]),
  <<Hash:160>> = crypto:hash(sha, pid_to_list(Pid)),
  Key_Hash = Hash rem X,
  Temp = "node_" ++ integer_to_list(Key_Hash),
  register(list_to_atom(Temp),Pid),
  Key_Hash.

loop_join(0) ->
  ok;

loop_join(NumberNodesN) ->
  chord_ring ! join,
  timer:sleep(500),
  loop_join(NumberNodesN - 1).

shuffle(List) ->
  lists:nth(rand:uniform(length(List)), List).

getNodeName(NodeID)->
  list_to_atom("node_" ++ integer_to_list(NodeID)).

