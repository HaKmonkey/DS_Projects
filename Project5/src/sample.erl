-module(sample).

-behaviour(supervisor).

-behaviour(application).

-compile(export_all).

main(A) -> mad:main(A).

stop(_) -> ok.

start() -> start(normal, []).

start(_, _) ->
    cowboy:start_clear(http,
                       [{port, application:get_env(n2o, port, 8001)}],
                       #{env => #{dispatch => n2o_cowboy:points()}}),
    
    supervisor:start_link({local, host}, sample, []). % the host name is 
    

init([]) ->
    kvs:join(),
    project4:start_host(),
    {ok, {{one_for_one, 5, 10}, []}}.


% nonode@nohost