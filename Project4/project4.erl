% TODO:
% Need to catch if the password is incorrect somehow
% implement the password auth for users
% Need to implement tracking for logged in state - perhaps in user table?
% use logged in state to then allow tweeting functionality

% subscribe to a certain user ->
%% searching tweets by username ->
%%% maintaining a subscribtion list

-module(project4).

-export([
    start_host/0,
    start_user/1,
    user_node/1,
    host_server/0,
    user_auth_server/0,
    tweet_log_server/0
]).
%-compile(export_all).

% everything will need a user result message
user_auth_server() ->
    receive
        {request_user, From, UserName, Password, Reason} ->
            if
                Reason == "New User" ->
                    self() ! {register_user, UserName, Password, From};
                true ->
                    self() ! {login_request, UserName, Password, From}
            end;
        {register_user, UserName, Password, From} ->
            ExistingUser = ets:member(twitter_users, UserName),
            if 
                ExistingUser == false ->
                    ets:insert_new(twitter_users, {UserName, Password, online}),
                    From ! {online, UserName, Password};
                true ->
                    io:fwrite("That user already exists, please choose another~n")
            end;
        {login_request, UserName, Password, From} ->
            ExistingUser = ets:member(twitter_users, UserName),
            {U, P, _} = ets:match_object(twitter_users, {UserName, Password, offline}),
            if 
                ExistingUser == true ->
                    ets:update_element(twitter_users, UserName, {3, online}), % update user to be online
                    From ! {online, U, P};
                true ->
                    io:fwrite("Perhaps the username or password was incorrect~n")
            end;
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
        {make_user, UserName, Password, From} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            user_server ! {request_user, From, UserName, Hash, "New User"};
        {login, UserName, Password, From} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            user_server ! {request_user, From, UserName, Hash, "Login"};
        {write_tweet, UserName, Password, Tweet} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            tweet_server ! {start_tweet, UserName, Hash, Tweet};
        print_users ->
            user_server ! print_users;
        print_tweets ->
            tweet_server ! print_tweets
    end,
    host_server().


search_tweets() -> 
    ets:first(twitter_tweets).
    % get the first key, then use it to get the following keys in a loop
    % searching for the key phrase
    % will need to modify how tweets are stored


get_status(TableName) ->
    [{_, UserName, _}] = ets:lookup(TableName, user_info),
    [{_, _, Status}] = ets:lookup(twitter_users, UserName),
    {UserName, Status}.


user_node(TableName) ->
    receive
        awake ->
            io:fwrite("This is the Pid for the user node: ~p~n~n", [self()]),
            ets:new(TableName, [set, named_table, public]);
        {new_user, UserName, Password} ->
            twitter_host ! {make_user, UserName, Password, self()};
        {login, UserName, Password} ->
            twitter_host ! {login, UserName, Password, self()};
        {online, UserName, Password} ->
            ets:insert_new(TableName, {user_info, UserName, Password});
        log_off ->
            [{_, UserName, _}] = ets:lookup(TableName, user_info),
            ets:update_element(twitter_users, UserName, {3, offline}),
            erlang:exit(self(), normal);
        {make_tweet, UserName, Password, Tweet} -> % need to fix this after implementing the login...
            {_, Status} = get_status(TableName),
            if
                Status == online -> 
                    twitter_host ! {write_tweet, UserName, Password, Tweet};
                true ->
                    io:fwrite("You need to log in first~n")
            end;
        {subscribe, SubscribeTo} -> % Need to figure out how to show tweets now...
            {_, Status} = get_status(TableName),
            if
                Status == online -> 
                    ets:insert_new(TableName, {subscribed, SubscribeTo});
                true ->
                    io:fwrite("You need to log in first~n")
            end,
            io:fwrite("Subscribed to: ~p~n", [SubscribeTo]);
        {search_tweets_by_tag, Tag} -> % not implemented yet...
            {_, Status} = get_status(TableName),
            io:fwrite("~p~n", [Tag]);
        {search_tweets_by_mention} -> % not implemented yet...
            {UserName, Status} = get_status(TableName),
            done
    end,
    user_node(TableName).


start_host() ->
    ets:new(twitter_users, [set, named_table, public]),
    ets:new(twitter_tweets, [set, named_table, public]),
    register(user_server, spawn(?MODULE, user_auth, [])),
    register(tweet_server, spawn(?MODULE, tweet_log, [])),
    register(twitter_host, spawn(?MODULE, host_server, [])).

% TwitterHost, Username, Password
start_user(TableName) ->
    Pid = spawn(?MODULE, user_node, [TableName]),
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