-module(project2).

-export([start/3, gossip_node/1, push_sum_node/1]).

gossip_node(X) ->
    io:fwrite("Gossip Node~p Started~n", [X]).

push_sum_node(X) ->
    io:fwrite("Push Sum Node~p Started~n", [X]).

spawn_node(0, NodeList, _) ->
    io:format("~w~n",[NodeList]);
spawn_node(X, NodeList, Algorithm) when X > 0 ->
    case {Algorithm} of
        {'gossip'} -> Pid = spawn(?MODULE, gossip_node, [X]);
        {'push-sum'} -> Pid = spawn(?MODULE, push_sum_node, [X])
    end,
    NewNodeList = lists:append([Pid], NodeList),
    spawn_node(X-1, NewNodeList, Algorithm).

start(NumNodes, Topology, Algorithm) ->
    case {Topology} of
        {'full'} -> X = NumNodes;
        {'line'} -> X = NumNodes;
        {'2D'} -> X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2));
        {'imp2D'} -> X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2))
    end,
    spawn_node(X, [], Algorithm),
    io:format("~p ~p ~p~n", [X, Topology, Algorithm]).