-module(project4).

-export([start/0, server/2]).



% user will send and receive tweets...
% the server will send tweets out to all users that are online and also keep
%% a list of the tweets?



server(UserList, Tweets) ->
    receive
        {register_user, UserName} ->
            ExistingUser = lists:member(UserName, UserList),
            if
                ExistingUser == false ->
                    NewUserList = lists:append([UserName], UserList);
                true ->
                    io:fwrite("That user already exists, please choose another~n"),
                    NewUserList = UserList
            end,
            NewTweets = Tweets;
        {send_tweet, UserName, Tweet} ->
            TweetLength = string:length(Tweet),
            ExistingUser = lists:member(UserName, UserList),
            if 
                TweetLength > 140 ->
                    NewTweets = Tweets,
                    io:fwrite("Tweet is too long, needs to be =< 140 char~n");
                ExistingUser == false ->
                    NewTweets = Tweets,
                    io:fwrite("That user does not exist~n");
                true ->
                    NewTweets = lists:append([Tweet], Tweets),
                    io:fwrite("~p says: ~p", [UserName, Tweet])
            end,
            NewUserList = UserList;
        print_users ->
            io:fwrite("~p", [UserList]),
            NewTweets = Tweets,
            NewUserList = UserList;
        print_tweets ->
            io:fwrite("~p", [Tweets]),
            NewTweets = Tweets,
            NewUserList = UserList
    end,
    server(NewUserList, NewTweets).



start() ->
    register(twit_host, spawn(?MODULE, server, [[], []])).