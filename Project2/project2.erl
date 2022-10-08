-module(project2).

-import(tools,[shuffle/1,randomize/1,randomize/2]).

-export([start/3, gossip_node/1, push_sum_node/1, spawn_node/4, find_neighbor/2,
    full_neighbor_node/1, full_neighbor_node/3,
    line_neighbor_node/2, line_neighbor_node/4,
    twoD_neighbor_node/3, twoD_neighbor_node/5,
    imp2D/3, imp2D/5, random_neighbor/3]).

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
    N = length(NodeList),
    M = erlang:trunc(math:sqrt(N)),
    case {Topology} of
        {'full'} ->
            Neighbor = full_neighbor_node(NodeList),
            io:fwrite("~w ~n", [Neighbor]);
        {'line'} ->
            Neighbor = line_neighbor_node(NodeList,N),
            io:fwrite("~w ~n", [Neighbor]);
        {'2D'} ->
            Neighbor = twoD_neighbor_node(NodeList,M,N),
            io:fwrite("~w ~n", [Neighbor]);
        {'imp2D'} ->
            Neighbor = imp2D(NodeList, M, N),
            io:fwrite("~w ~n", [Neighbor])

end.

num(L) -> length([X || X <- L, X < 1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%topology specific function
%% !!! The list start from1 which is different from elixir.
full_neighbor_node(List) when is_list(List) ->
    io:fwrite("~p ~n",[lists:nth(1, List)]),
    full_neighbor_node(List, length(List), []).

full_neighbor_node(List, 0, Acc) ->
    Acc;

full_neighbor_node(List, I, Acc) when I > 0 ->
    full_neighbor_node(List, I-1, [lists:delete(lists:nth(I,List),List)|Acc]).



line_neighbor_node(NodeList, N) when is_list(NodeList)->
    line_neighbor_node(NodeList, length(NodeList), [], N).

line_neighbor_node(NodeList, 0, Acc, N) ->
    Acc;

line_neighbor_node(NodeList, I, Acc, N) when I == 1 ->
    line_neighbor_node(NodeList, I-1, [lists:nth(I + 1, NodeList)|Acc], N);

line_neighbor_node(NodeList, I, Acc, N) when I == N ->
    line_neighbor_node(NodeList, I-1, [lists:nth(I-1, NodeList)|Acc], N);

line_neighbor_node(NodeList, I, Acc, N) ->
    line_neighbor_node(NodeList, I-1, [[lists:nth(I-1, NodeList),lists:nth(I + 1, NodeList)]|Acc], N).



twoD_neighbor_node(NodeList, N, M) when is_list(NodeList)->
    twoD_neighbor_node(NodeList, M, [], N, M).

twoD_neighbor_node(NodeList, 0, Acc, N, M) ->
    Acc,
    io:fwrite("~w ~n", [Acc]);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I == 1 ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I+N,NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I == N ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+N,NodeList),lists:nth(I+N-1,NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I == M ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I-N,NodeList),lists:nth(I-N-1,NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I == M-N+1 ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I-N,NodeList),lists:nth(I-N+1,NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I > 1 , I < N ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+1, NodeList),lists:nth(I+N,NodeList),lists:nth(I+N-1,NodeList),lists:nth(I+N+1,NodeList)] | Acc],N,M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I > (M-N) , I < M ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+1, NodeList),lists:nth(I-N,NodeList),lists:nth(I-N+1,NodeList),lists:nth(I-N-1,NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I rem N == 0 ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) when I rem N == 1 ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList)] | Acc], N, M);

twoD_neighbor_node(NodeList, I, Acc, N, M) ->
    twoD_neighbor_node(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList)] | Acc], N, M).



imp2D(NodeList, N, M) when is_list(NodeList)->
    imp2D(NodeList, M, [], N, M).

imp2D(NodeList, 0, Acc, N, M) ->
    Acc;

imp2D(NodeList, I, Acc, N, M) when I == 1 ->
    imp2D(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I+N,NodeList),random_neighbor(NodeList, [lists:nth(I+1,NodeList),lists:nth(I+N,NodeList)], I)] | Acc], N, M );

imp2D(NodeList, I, Acc, N, M) when I == N ->
    imp2D(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+N,NodeList),lists:nth(I+N-1,NodeList),random_neighbor(NodeList,[lists:nth(I-1,NodeList),lists:nth(I+N,NodeList),lists:nth(I+N-1,NodeList)], I)] | Acc], N, M);

imp2D(NodeList, I, Acc, N, M) when I == M ->
    imp2D(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I-N,NodeList),lists:nth(I-N-1,NodeList),random_neighbor(NodeList,[lists:nth(I-1,NodeList),lists:nth(I-N,NodeList),lists:nth(I-N-1,NodeList)], I)] |Acc], N, M);

imp2D(NodeList, I, Acc, N, M) when I == M-N+1 ->
    imp2D(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I-N,NodeList),lists:nth(I-N+1,NodeList), random_neighbor(NodeList,[lists:nth(I+1,NodeList),lists:nth(I-N,NodeList),lists:nth(I-N+1,NodeList)],I)] | Acc], N, M );

imp2D(NodeList, I, Acc, N, M) when I > 1 , I < N ->
    imp2D(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+1, NodeList),lists:nth(I+N,NodeList),lists:nth(I+N-1,NodeList),lists:nth(I+N+1,NodeList), random_neighbor(NodeList, [lists:nth(I-1,NodeList),lists:nth(I+1, NodeList),lists:nth(I+N,NodeList),lists:nth(I+N-1,NodeList),lists:nth(I+N+1,NodeList)], I)] | Acc],N,M);

imp2D(NodeList, I, Acc, N, M) when I > (M-N) , I < M ->
    imp2D(NodeList, I-1, [[lists:nth(I-1,NodeList),lists:nth(I+1, NodeList),lists:nth(I-N,NodeList),lists:nth(I-N+1,NodeList),lists:nth(I-N-1,NodeList), random_neighbor(NodeList,[lists:nth(I-1,NodeList),lists:nth(I+1, NodeList),lists:nth(I-N,NodeList),lists:nth(I-N+1,NodeList),lists:nth(I-N-1,NodeList)],I)] | Acc], N, M);

imp2D(NodeList, I, Acc, N, M) when I rem N == 0 ->
    imp2D(NodeList, I-1,  [[lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList), random_neighbor(NodeList, [lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList)], I)] | Acc], N, M);

imp2D(NodeList, I, Acc, N, M) when I rem N == 1 ->
    imp2D(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList), random_neighbor(NodeList, [lists:nth(I+1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList)], I)] | Acc], N, M);

%% TODO if the matrix only have 8 node, it failed.(To this data structure we do not have the other node)
imp2D(NodeList, I, Acc, N, M) ->
    imp2D(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList), random_neighbor(NodeList, [lists:nth(I+1,NodeList),lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList)], I)] | Acc], N, M).


random_neighbor(NodeList, List, I) ->
    Temp = lists:delete(lists:nth(I,NodeList),NodeList),
    Temp2 = lists:filter(fun (Elem) -> not lists:member(Elem,List) end, Temp),
%%    io:fwrite("I:~p Temp2:~w Temp:~w List: ~w ~n", [I, Temp2, Temp, List]),
    if
        Temp2 == [] ->
            ok;
        true ->
            hd(shuffle(Temp2))


    end.
%%    hd(shuffle(Temp2)).