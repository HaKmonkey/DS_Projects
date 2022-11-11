-module(project4).

-export([start/0, server/0, user_auth/0, tweet_log/0]).

% Could we use records for users and tweets?
% TODO: add passwords for the users


user_auth() ->
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
    user_auth().



tweet_log() ->
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
    tweet_log().
        

% can search for tags using
% TestString = "This is a #test".
% string:find(TestString, "#"). >> "#test"
% ^ will find the searched tag, but if they are similar then it will find both



% subscribe to a certain user ->
%% searching tweets by username ->
%%% maintaining a subscribtion list


server() ->
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
    server().



start() ->
    ets:new(twitter_users, [set, named_table, public]),
    ets:new(twitter_tweets, [set, named_table, public]),
    register(user_server, spawn(?MODULE, user_auth, [])),
    register(tweet_server, spawn(?MODULE, tweet_log, [])),
    register(twitter_host, spawn(?MODULE, server, [])).