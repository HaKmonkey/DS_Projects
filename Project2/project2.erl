-module(project2).

-import(tools,[shuffle/1,randomize/1,randomize/2,while/1,while/2]).

-export([start/3, gossip_node/0, push_sum_node/1, spawn_node/4, find_neighbor/2,
    full_neighbor_node/1, full_neighbor_node/3,
    line_neighbor_node/2, line_neighbor_node/4,
    twoD_neighbor_node/3, twoD_neighbor_node/5,
    imp2D/3, imp2D/5, random_neighbor/3, send_neighbor_list/2, find_curr_neighbor/2]).

%% Data_List: [Rumor,Neighbor_List].
%%gossip_node() ->
%%
%%    receive
%%        {neighborList, NeighborList} ->
%%            io:fwrite("Neighbor: ~w ID: ~p ~n",[NeighborList,self()]);
%%        {message,Rumor,Neighbors} ->
%%            if
%%                Rumor < 10 ->
%%                    New_Rumor = Rumor + 1,
%%                    hd(shuffle(Neighbors)) ! {message, New_Rumor, Curr_Neighbor},
%%                    io:fwrite("Cur_Neighbor:~w Curr_ID: ~p ~n",[Curr_Neighbor, self()])
%%
%%            end
%%
%%
%%    end,
%%    gossip_node().

gossip_node() ->
    receive
        {rumor,NodeList,RumorMap} ->
            Curr_ID = self(),
            Curr_Neighbor = element(2,find_curr_neighbor(Curr_ID,NodeList)),
%%            io:fwrite("debug test info: ~w ~n",[Curr_Neighbor]),
            if
                is_list(Curr_Neighbor) -> Next_ID = hd(shuffle(Curr_Neighbor));
                true -> Next_ID = Curr_Neighbor
            end,

            Curr_Rumor = element(2,maps:find(Curr_ID,RumorMap)),
            Rumor_Values = maps:values(RumorMap),
            Flag = lists:member(10,Rumor_Values),
            if
                Flag == false->
                    New_RumorMap = maps:update(Curr_ID,Curr_Rumor + 1, RumorMap),
                    io:fwrite("Gossip Rumor Test: ~w ~n",[New_RumorMap]),
                    Next_ID ! {rumor, NodeList, New_RumorMap};
                true -> done
            end;
        stop ->
            done

    end,
    gossip_node().

push_sum_node(X) ->
    io:fwrite("Push Sum Node~p Started~n", [X]).


spawn_node(0, NodeList, _, Topology) ->
    io:format("~w ~p ~n",[NodeList,Topology]),
    find_neighbor(NodeList, Topology);
    %% we have the full nodeList, Then according to the Topology algorithm, we can find the neighbor list to each node.

spawn_node(X, NodeList, Algorithm, Topology) when X > 0 ->
    case {Algorithm} of
        {'gossip'} ->
            Pid = spawn(?MODULE, gossip_node, []);
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
        {'imp2D'} ->
            if
                NumNodes < 16 ->
                    X = erlang:trunc(math:pow(math:ceil(math:sqrt(16)), 2));
                true ->
                    X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2))
            end

    end,
    spawn_node(X, [], Algorithm, Topology),
    io:format("~p ~p ~p~n", [X, Topology, Algorithm]).

find_curr_neighbor(Curr_ID, NodeList) ->
    maps:find(Curr_ID, NodeList).
%%    io:fwrite("find Neighbor: ~p ~w ~n",[Curr_ID,Res]).

find_neighbor(NodeList, Topology) ->
    N = length(NodeList),
    M = erlang:trunc(math:sqrt(N)),
    case {Topology} of
        {'full'} ->
            Neighbor = full_neighbor_node(NodeList),
            send_neighbor_list(NodeList, Neighbor),
            io:fwrite("~w ~n", [Neighbor]);
        {'line'} ->
            Neighbor = line_neighbor_node(NodeList,N),
            send_neighbor_list(NodeList, Neighbor),
            io:fwrite("~w ~n", [Neighbor]);
        {'2D'} ->
            Neighbor = twoD_neighbor_node(NodeList,M,N),
            send_neighbor_list(NodeList, Neighbor),
            io:fwrite("~w ~n", [Neighbor]);
        {'imp2D'} ->
            Neighbor = imp2D(NodeList, M, N),
            send_neighbor_list(NodeList, Neighbor),
            io:fwrite("~w ~n", [Neighbor])

end.


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
    Acc;

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

imp2D(NodeList, I, Acc, N, M) ->
    imp2D(NodeList, I-1, [[lists:nth(I+1,NodeList),lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList), random_neighbor(NodeList, [lists:nth(I+1,NodeList),lists:nth(I-1,NodeList),lists:nth(I+N, NodeList),lists:nth(I-N,NodeList), lists:nth(I+N-1, NodeList), lists:nth(I-N-1, NodeList), lists:nth(I+N+1, NodeList), lists:nth(I-N+1, NodeList)], I)] | Acc], N, M).


random_neighbor(NodeList, List, I) ->
    Temp = lists:delete(lists:nth(I,NodeList),NodeList),
    Temp2 = lists:filter(fun (Elem) -> not lists:member(Elem,List) end, Temp),
    hd(shuffle(Temp2)).

send_neighbor_list(NodeList, Neighbor) ->
    Temp = lists:zip(NodeList, Neighbor),
    Rumor_List = lists:duplicate(length(NodeList),0),
    Rumor = lists:zip(NodeList,Rumor_List),
    Rumor_Map = maps:from_list(Rumor),
    Neighbor_Map = maps:from_list(Temp),
    io:fwrite("Dict ~w", [Neighbor_Map]),
    Node_ID = hd(shuffle(NodeList)),
    Node_ID ! {rumor, Neighbor_Map, Rumor_Map},
%%    lists:foreach(fun(N) ->
%%        Node_Id = element(1,N),
%%        Node_Neighbor = element(2,N),
%%        Node_Id ! {rumor, Temp}
%%                  end,Temp),
%%    Random_Info = hd(shuffle(Temp)),
%%    Random_ID = element(1, Random_Info),
%%    Random_Neighbor = element(2,Random_Info),
%%    Random_ID ! {message,0,Random_Neighbor},
    io:fwrite("############################################~n").



