-module(project2).

-export([start/3, gossip_node/1, push_sum_node/1, spawn_node/4, find_neighbor/2, full_neighbor_node/1, full_neighbor_node/3]).

gossip_node(X) ->
    io:fwrite("Gossip Node~p Started~n", [X]).

push_sum_node(X) ->
    io:fwrite("Push Sum Node~p Started~n", [X]).

spawn_node(0, NodeList, _, Topology) ->
    io:format("~w ~p ~n",[NodeList,Topology]),
    find_neighbor(NodeList, Topology);
    %% we have the full nodeList, Then according to the Topology algorithm, we can find the neighbor list to each node.

spawn_node(X, NodeList, Algorithm, Topology) when X > 0 ->
    case {Algorithm} of
        {'gossip'} ->
            Pid = spawn(?MODULE, gossip_node, [X]);
        {'push-sum'} ->
            Pid = spawn(?MODULE, push_sum_node, [X])
    end,
    NewNodeList = lists:append([Pid], NodeList),
    spawn_node(X-1, NewNodeList, Algorithm, Topology).

start(NumNodes, Topology, Algorithm) ->
    case {Topology} of
        {'full'} -> X = NumNodes;
        {'line'} -> X = NumNodes;
        {'2D'} -> X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2));
        {'imp2D'} -> X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2))
    end,
    spawn_node(X, [], Algorithm, Topology),
    io:format("~p ~p ~p~n", [X, Topology, Algorithm]).

find_neighbor(NodeList, Topology) ->
    case Topology of
        'full' -> neighbor = full_neighbor_node(NodeList)
end.

num(L) -> length([X || X <- L, X < 1]).

full_neighbor_node(list) when is_list(list)->
    full_neighbor_node(list, num(list), []).

full_neighbor_node(list, 0, acc) ->
    acc,
    io:fwrite("~w ~n",[acc]);

full_neighbor_node(list, i, acc) when i > 0 ->
    full_neighbor_node(list, i-1, lists:delete(lists:nth(i-1,list),list)).

        
    
