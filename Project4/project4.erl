% TODO: implement the password auth for users

% subscribe to a certain user ->
%% searching tweets by username ->
%%% maintaining a subscribtion list

-module(project4).

-export([
    start_host/0,
    start_user/0,
    user_node/0,
    host_server/0,
    user_auth_server/0,
    tweet_log_server/0
]).
%-compile(export_all).


user_auth_server() ->
    receive
        {check_user, UserName, Password, Tweet, From} ->
            ExistingUser = ets:member(twitter_users, UserName),
            From ! {user_result, ExistingUser, UserName, Password, Tweet};
        {user_result, ExistingUser, UserName, Password, _} ->
            if
                ExistingUser == false ->
                    self() ! {register_user, UserName, Password};
                true ->
                    io:fwrite("That user already exists, please choose another~n")
            end;
        {request_user, UserName, Password} ->
            self() ! {check_user, UserName, Password, "", self()};
        {register_user, UserName, Password} ->
            ets:insert_new(twitter_users, {UserName, Password});
        print_users ->
            io:fwrite("~p", [ets:info(twitter_users)])
    end,
    user_auth_server().


tweet_log_server() ->
    receive
        {start_tweet, UserName, Password, Tweet} ->
            user_server ! {check_user, UserName, Password, Tweet, self()};
        {user_result, ExistingUser, UserName, _, Tweet} ->
            if
                ExistingUser == false ->
                    io:fwrite("That user does not exist~n");
                true ->
                    self() ! {publish_tweet, UserName, Tweet}
            end;
        {publish_tweet, UserName, Tweet} ->
            TweetLength = string:length(Tweet),
            if 
                TweetLength > 140 ->
                    io:fwrite("Tweet is too long, needs to be =< 140 char~n");
                true ->
                    ets:insert_new(twitter_tweets, {UserName, Tweet}),
                    io:fwrite("~p:~n\t~p~n", [UserName, Tweet])
            end;
        print_tweets ->
            io:fwrite("~p", [ets:info(twitter_tweets)])
    end,
    tweet_log_server().
        
% can search for tags using
% TestString = "This is a #test".
% string:find(TestString, "#"). >> "#test"
% ^ will find the searched tag, but if they are similar then it will find both

host_server() ->
    receive
        {make_user, UserName, Password} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            user_server ! {request_user, UserName, Hash};
        {write_tweet, UserName, Password, Tweet} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            tweet_server ! {start_tweet, UserName, Hash, Tweet};
        print_users ->
            user_server ! print_users;
        print_tweets ->
            tweet_server ! print_tweets
    end,
    host_server().


user_node() ->
    receive
        awake ->
            io:fwrite("This is the Pid for the user node: ~p~n~n", [self()]);
        {new_user, UserName, Password} ->
            twitter_host ! {make_user, UserName, Password};
        {login, UserName, Password} -> % don't have this implemented yet...
            io:fwrite("~p  ~p~n", [UserName, Password]);
        {make_tweet, UserName, Password, Tweet} -> % need to fix this after implementing the login...
            twitter_host ! {write_tweet, UserName, Password, Tweet};
        {subscribe, SubscribeTo} -> % not implemented yet...
            io:fwrite("~p~n",[SubscribeTo]);
        {search_tweets_by_tag, Tag} -> % not implemented yet...
            io:fwrite("~p~n", [Tag]);
        {search_tweets_by_mention} -> % not implemented yet...
            done
    end,
    user_node().


start_host() ->
    ets:new(twitter_users, [set, named_table, public]),
    ets:new(twitter_tweets, [set, named_table, public]),
    register(user_server, spawn(?MODULE, user_auth, [])),
    register(tweet_server, spawn(?MODULE, tweet_log, [])),
    register(twitter_host, spawn(?MODULE, host_server, [])).

% TwitterHost, Username, Password
start_user() ->
    Pid = spawn(?MODULE, user_node, []),
    Pid ! awake,
    io:fwrite("
        Here is a list of commands you can use:~n
        \t~p ! {new_user, UserName, Password}
        \t~p ! {login, UserName, Password}
        \t~p ! {make_tweet, Tweet}
        \t~p ! {subscribe, SubscribeTo}
        \t~p ! {search_tweets_by_tag, Tag}
        \t~p ! {search_tweets_by_mention}~n
    ",[Pid,Pid,Pid,Pid,Pid,Pid]).