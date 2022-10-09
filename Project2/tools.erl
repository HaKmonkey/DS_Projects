%%%-------------------------------------------------------------------
%%% @author hongy
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2022 7:06 PM
%%%-------------------------------------------------------------------
-module(tools).
-author("hongy").

%% API
-export([shuffle/1,randomize/1,randomize/2]).

%% shuffle(List1) -> List2
%% Takes a list and randomly shuffles it. Relies on random:uniform
%%
shuffle(List) ->
  %% Determine the log n portion then randomize the list.
  randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
  randomize(List);
randomize(T, List) ->
  lists:foldl(fun(_E, Acc) ->
    randomize(Acc)
              end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
  D = lists:map(fun(A) ->
    {random:uniform(), A}
                end, List),

  {_, D1} = lists:unzip(lists:keysort(1, D)),
  D1.





