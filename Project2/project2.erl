-module(project2).

-export([start/3, server/5, gossip_node/2, propagate_rumor/3, push_sum_node/5]).

shuffle(List) ->
    lists:nth(rand:uniform(length(List)), List).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

neighbors_from_grid(Self, NodeList) ->
    SelfIndex = index_of(Self, NodeList),
    Side = trunc(math:sqrt(length(NodeList))),
    Row = trunc(math:ceil(SelfIndex / Side)),
    Col = ((SelfIndex - 1) rem Side) + 1,
    Adj = lists:seq(-1,1), % -1, 0, 1
    Coords = [{Row+Y, Col+X} || Y <- Adj, X <- Adj, Row+Y > 0, Row+Y =< Side, Col+X > 0, Col+X =< Side],
    F = fun({R, C}) -> lists:nth(((R - 1) * Side) + C, NodeList) end,
    Neighbors = lists:map(F, Coords),
    lists:delete(Self, Neighbors).

get_neighbor(Self, Topology, NodeList) ->
    case {Topology} of
        {'full'} ->
            NeighborList = lists:delete(Self, NodeList);
        {'line'} ->
            SelfIndex = index_of(Self, NodeList),
            if
                SelfIndex == 1 ->
                    NeighborList = [lists:nth(2, NodeList)];
                SelfIndex == length(NodeList) ->
                    NeighborList = [lists:nth(SelfIndex-1, NodeList)];
                true ->
                    NewNodeList = lists:sublist(NodeList, SelfIndex-1, 3),
                    NeighborList = lists:delete(Self, NewNodeList)
            end;
        {'2D'} ->
            NeighborList = neighbors_from_grid(Self, NodeList);
        {'imp2D'} ->
            InitNeighborList = neighbors_from_grid(Self, NodeList),
            LessSelf = lists:delete(Self, NodeList),
            ImpNeighbor = lists:delete(InitNeighborList, LessSelf),
            NeighborList = lists:append([shuffle(ImpNeighbor)], InitNeighborList)
    end,
    shuffle(NeighborList). % Returns the randomly selected neighbor

propagate_rumor(From, Topology, NodeList) ->
    Neighbor = get_neighbor(From, Topology, NodeList),
    %io:fwrite("Gossip From: ~w To: ~w~n", [From, Neighbor]),
    Neighbor ! {rumor, Topology, NodeList},
    propagate_rumor(From, Topology, NodeList).

gossip_node(From, Rumor) ->
    receive
        {rumor, Topology, NodeList} ->
            SendNode = spawn(?MODULE, propagate_rumor, [self(), Topology, NodeList]),
            NewRumor = Rumor + 1
    end,
    %io:fwrite("~w heard rumor ~p times~n", [self(), NewRumor]),
    gossip_node(From, self(), NewRumor, SendNode).

gossip_node(Host, _, 10, SendNode) ->
    Host ! {finished},
    exit(SendNode, "Heard rumor 10 times."); % kill propagate_rumor
gossip_node(Host, From, Rumor, SendNode) ->
    receive
        {rumor, _, _} ->
            NewRumor = Rumor + 1
    end,
    %io:fwrite("~w heard rumor ~p times~n", [From, NewRumor]),
    gossip_node(Host, From, NewRumor, SendNode).

push_sum_node(From, _, _, Estimate, 3) ->
    From ! {finished},
    io:fwrite("E: ~p~n",[Estimate]),
    exit(self(), "Estimate didn't change 3 times.");
push_sum_node(From, Sum, Weight, Estimate, Count) ->
    receive
        {start, Topology, NodeList} ->
            Neighbor = get_neighbor(self(), Topology, NodeList),
            %io:fwrite("Gossip From: ~w To: ~w~n", [From, Neighbor]),
            NewCount = Count,
            NewEstimate = Estimate,
            NewSum = Sum / 2,
            NewWeight = Weight / 2,
            Neighbor ! {push, NewSum, NewWeight, Topology, NodeList};
        {push, S, W, Topology, NodeList} ->
            Neighbor = get_neighbor(self(), Topology, NodeList),
            Check = math:pow(10,-10),
            NewEstimate = (S + Sum) / (W + Weight),
            if 
                Estimate - NewEstimate < Check ->
                    NewCount = Count + 1;
                true ->
                    NewCount = 0
            end,
            NewSum = (S + Sum) / 2,
            NewWeight = (W + Weight) / 2,
            Neighbor ! {push, NewSum, NewWeight, Topology, NodeList}
    end,
    push_sum_node(From, NewSum, NewWeight, NewEstimate, NewCount).

spawn_node(0, NodeList, From, _) ->
    From ! {finished_spawning, NodeList};
spawn_node(X, NodeList, From, Algorithm) when X > 0 ->
    case {Algorithm} of
        {'gossip'} ->
            Pid = spawn(?MODULE, gossip_node, [From, 0]);
        {'push-sum'} ->
            Pid = spawn(?MODULE, push_sum_node, [From, X, 1, (X/1), 0])
    end,
    NewNodeList = lists:append([Pid], NodeList),
    spawn_node(X-1, NewNodeList, From, Algorithm).

server(StartTime, _, _, Count, NodeCount) when Count == NodeCount ->
    EndTime = erlang:timestamp(),
    io:fwrite("FINISHED in ~pms~n", [timer:now_diff(EndTime, StartTime)]);
server(StartTime, Topology, Algorithm, Count, NodeCount) ->
    receive
        {'spawn_nodes', X} ->
            spawn_node(X, [], self(), Algorithm),
            NewCount = Count,
            %io:format("~p ~p ~p~n", [X, Topology, Algorithm]);
            NewNodeCount = X;
        {finished_spawning, NodeList} ->
            %io:fwrite("~w~n",[NodeList]),
            SeedNode = shuffle(NodeList),
            case {Algorithm} of
                {'gossip'} ->
                    SeedNode ! {rumor, Topology, NodeList};
                {'push-sum'} ->
                    SeedNode ! {start, Topology, NodeList}
            end,
            NewCount = Count,
            NewNodeCount = NodeCount;
        {finished} ->
            NewNodeCount = NodeCount,
            NewCount = Count + 1
    end,
    server(StartTime, Topology, Algorithm, NewCount, NewNodeCount).

start(NumNodes, Topology, Algorithm) ->
    StartTime = erlang:timestamp(),
    case {Topology} of
        {'full'} -> X = NumNodes;
        {'line'} -> X = NumNodes;
        {'2D'} -> X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2));
        {'imp2D'} -> X = erlang:trunc(math:pow(math:ceil(math:sqrt(NumNodes)), 2))
    end,
    Server = spawn(?MODULE, server, [StartTime, Topology, Algorithm, 0, 0]),
    Server ! {'spawn_nodes', X}.