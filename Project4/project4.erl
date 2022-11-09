-module(project4).

-export([start/0, server/2, user_auth/1, tweet_log/1]).



% user will send and receive tweets...
% the server will send tweets out to all users that are online and also keep
%% a list of the tweets?

user_auth(Users) ->
    receive
        {check_user, UserName, From} ->
            ExistingUser = lists:member(UserName, Users),
            From ! {user_result, ExistingUser, UserName},
            UpdatedUsers = Users;
        {user_result, ExistingUser, UserName} ->
            if
                ExistingUser == false ->
                    self() ! {register_user, UserName};
                true ->
                    io:fwrite("That user already exists, please choose another~n")
            end,
            UpdatedUsers = Users;
        {request_user, UserName} ->
            self() ! {check_user, UserName, self()},
            UpdatedUsers = Users;
        {register_user, UserName} ->
            UpdatedUsers = lists:append([UserName], Users);
        print_users ->
            io:fwrite("~p", [Users]),
            UpdatedUsers = Users
    end,
    user_auth(UpdatedUsers).



tweet_log(Tweets) ->
    receive
        {send_tweet, UserName, Tweet} ->
            TweetLength = string:length(Tweet),
            ExistingUser = lists:member(UserName, UserList),
            if 
                TweetLength > 140 ->
                    UpdatedTweets = Tweets,
                    io:fwrite("Tweet is too long, needs to be =< 140 char~n");
                ExistingUser == false ->
                    UpdatedTweets = Tweets,
                    io:fwrite("That user does not exist~n");
                true ->
                    UpdatedTweets = lists:append([{UserName, Tweet}], Tweets),
                    io:fwrite("~p says: ~p", [UserName, Tweet])
            end;
        print_tweets ->
            io:fwrite("~p", [Tweets]),
            UpdatedTweets = Tweets
    end,
    tweet_log(UpdatedTweets).
        


server(UserAuth, TweetLog) ->
    receive
        {make_user, UserName} ->
            UserAuth ! {request_user, UserName};
        {send_tweet, UserName, Tweet} ->
            TweetLog ! {send_tweet, UserName, Tweet}; % edit this
        print_users ->
            UserAuth ! print_users;
        print_tweets ->
            TweetLog ! print_tweets
    end,
    server(UserAuth, TweetLog).



start() ->
    UserAuth = spawn(?MODULE, user_auth, [[]]),
    TweetLog = spawn(?MODULE, tweet_log, [[]]),
    register(twitter, spawn(?MODULE, server, [UserAuth, TweetLog])).