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
    start_user/2,
    user_node/2,
    host_server/2,
    user_auth_server/0,
    tweet_log_server/1
]).
%-compile(export_all).

user_auth_server() ->
    receive
        awake ->
            ets:new(twitter_users, [set, named_table, private]),
            io:fwrite("User Auth server started.~n");
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
        {log_off, UserName} ->
            ets:update_element(twitter_users, UserName, {3, offline});
        {get_status, UserName, From, Reason, Message} ->
            [{_, _, Status}] = ets:lookup(twitter_users, UserName),
            From ! {status, UserName, Status, Reason, Message}
    end,
    user_auth_server().


search_tweets(_, _, Key, MatchList) when Key == '$end_of_table' ->
    MatchList;
search_tweets(Table, By, Key, MatchList) -> 
    NextKey = ets:next(Table, Key),
    [{_, _, Tweet}] = ets:lookup(Table, NextKey),
    Tag = string:find(Tweet, By),
    if
        Tag == By -> UpdatedMatchList = lists:append([Tweet], MatchList);
        true -> UpdatedMatchList = MatchList
    end,
    search_tweets(Table, By, NextKey, UpdatedMatchList).


tweet_log_server(Id) ->
    receive
        awake ->
            ets:new(twitter_tweets, [set, named_table, private]),
            io:fwrite("Tweet Log server started.~n"),
            NewId = Id;
        {publish_tweet, UserName, Tweet} ->
            TweetLength = string:length(Tweet),
            if 
                TweetLength > 140 ->
                    io:fwrite("Tweet is too long, needs to be =< 140 char~n");
                true ->
                    ets:insert_new(twitter_tweets, {Id, UserName, Tweet}),
                    io:fwrite("~p:~n\t~p~n", [UserName, Tweet])
            end,
            NewId = Id + 1;
        {search_tweets, By, From} ->
            FirstKey = ets:first(twitter_tweets),
            MatchList = search_tweets(twitter_tweets, By, FirstKey, []),
            From ! {print_search_results, MatchList},
            NewId = Id
    end,
    tweet_log_server(NewId).


host_server(UserServer, TweetServer) ->
    receive
        awake ->
            UserServer ! awake,
            TweetServer ! awake,
            io:fwrite("Host IP: ~p~n", [self()]);
        {make_user, UserName, Password, From} ->
            UserServer ! {request_user, From, UserName, Password, "New User"};
        {login, UserName, Password, From} ->
            UserServer ! {request_user, From, UserName, Password, "Login"};
        {write_tweet, UserName, Tweet} ->
            TweetServer ! {publish_tweet, UserName, Tweet};
        {log_off, UserName} ->
            UserServer ! {log_off, UserName};
        {get_status, UserName, From, Reason, Message} ->
            UserServer ! {get_status, UserName, From, Reason, Message};
        {search_tweets, Message, From} ->
            TweetServer ! {search_tweets, Message, From}
    end,
    host_server(UserServer, TweetServer).


user_node(TableName, HostPid) ->
    receive
        awake ->
            io:fwrite("This is the Pid for the user node: ~p~n~n", [self()]),
            ets:new(TableName, [set, named_table, private]);
        {new_user, UserName, Password} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            HostPid ! {make_user, UserName, Hash, self()};
        {login, UserName, Password} ->
            Hash = binary:decode_unsigned(crypto:hash(sha256,Password)),
            HostPid ! {login, UserName, Hash, self()};
        {online, UserName, Password} ->
            ets:insert_new(TableName, {user_info, UserName, Password});
        log_off -> % update the access to the table to be private
            [{_, UserName, _}] = ets:lookup(TableName, user_info),
            HostPid ! {log_off, UserName},
            erlang:exit(self(), normal);
        {make_tweet, Tweet} -> % need to fix this after implementing the login...
            [{_, UserName, _}] = ets:lookup(TableName, user_info),
            HostPid ! {get_status, UserName, self(), "Tweet", Tweet};
        {subscribe, SubscribeTo} -> % Need to figure out how to show tweets now...
            [{_, UserName, _}] = ets:lookup(TableName, user_info),
            HostPid ! {get_status, UserName, self(), "Subscribe", SubscribeTo};
        {search_tweets_by_tag, Tag} -> % not implemented yet...
            [{_, UserName, _}] = ets:lookup(TableName, user_info),
            HostPid ! {get_status, UserName, self(), "Search Tags", Tag};
        search_tweets_by_mention -> % not implemented yet...
            [{_, UserName, _}] = ets:lookup(TableName, user_info),
            Mention = "@" + UserName,
            HostPid ! {get_status, UserName, self(), "Search Mentions", Mention};
        {status, UserName, Status, Reason, Message} ->
            if
                Status == online ->
                    if
                        Reason == "Tweet" ->
                            HostPid ! {write_tweet, UserName, Message};
                        Reason == "Subscribe" ->
                            ets:insert_new(TableName, {subscribed, Message}),
                            io:fwrite("Subscribed to: ~p~n", [Message]);
                        Reason == "Search Tags" ->
                            HostPid ! {search_tweets, Message, self()};
                        Reason == "Search Mentions" ->
                            HostPid ! {search_tweets, Message, self()}
                    end;
                true ->
                    io:fwrite("You need to log in first~n")
            end;
        {print_search_results, MatchList} ->
            io:fwrite("~p~n", [MatchList])
    end,
    user_node(TableName, HostPid).


start_host() ->
    UserServer = spawn(?MODULE, user_auth_server, []),
    TweetServer = spawn(?MODULE, tweet_log_server, [0]),
    register(twitter_host,spawn(?MODULE, host_server, [UserServer, TweetServer])),
    twitter_host ! awake.


start_user(TableName, HostPid) ->
    Pid = spawn(?MODULE, user_node, [TableName, HostPid]),
    Pid ! awake,
    io:fwrite("
        Here is a list of commands you can use:~n
        \t~p ! {new_user, UserName, Password}
        \t~p ! {login, UserName, Password}
        \t~p ! log_off
        \t~p ! {make_tweet, Tweet}
        \t~p ! {subscribe, SubscribeTo}
        \t~p ! {search_tweets_by_tag, Tag}
        \t~p ! {search_tweets_by_mention}~n
    ",[Pid,Pid,Pid,Pid,Pid,Pid,Pid]).