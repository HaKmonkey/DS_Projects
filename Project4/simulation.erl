-module(simulation).

-export([start/0]).


start() ->
    {_, HostPid, HostName} = peer:start(#{name => host, connection => standard_io}),

    peer:call(HostPid, project4, start_host, []),

    timer:sleep(500),

    {_, User1Pid, _} = peer:start(#{name => peer:random_name(), connection => standard_io}),
    {_, User2Pid, _} = peer:start(#{name => peer:random_name(), connection => standard_io}),

    timer:sleep(500),

    User1Node = peer:call(User1Pid, project4, start_user, [test1, HostName]),
    User2Node = peer:call(User2Pid, project4, start_user, [test2, HostName]),

    timer:sleep(500),

    peer:send(User1Pid, User1Node, {new_user, "user1", "test1"}),
    peer:send(User2Pid, User2Node, {new_user, "user2", "test2"}),

    timer:sleep(500),

    peer:send(User1Pid, User1Node, {subscribe, "user2"}),

    peer:send(User2Pid, User2Node, {make_tweet, "This is a tweet"}),

    peer:send(User1Pid, User1Node, {make_tweet, "This is another tweet"}),
    peer:send(User1Pid, User1Node, {retweet, 0}).

    % to stop host
    % peer:stop(User1Pid),
    % peer:stop(User2Pid),
    % peer:stop(HostPid).




% <0.89.0> ! {new_user, UserName, Password}.
% <0.89.0> ! {login, UserName, Password}.
% <0.89.0> ! log_off.
% <0.89.0> ! {make_tweet, Tweet}.
% <0.89.0> ! {retweet, QueryId}.
% <0.89.0> ! {subscribe, SubscribeTo}.
% <0.89.0> ! {search_tweets_by_tag, Tag}.
% <0.89.0> ! search_tweets_by_mention.
% <0.89.0> ! search_tweets_by_subscription.
% <0.89.0> ! help.