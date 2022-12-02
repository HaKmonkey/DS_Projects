-module(simulation).

-export([start/1]).


spawn_user_hosts(0, UserList) ->
    UserList;
spawn_user_hosts(NumUsers, UserList) ->
    {_, UserPid, _} = peer:start(#{name => peer:random_name(), connection => standard_io}),
    UpdatedUserList = lists:append([UserPid], UserList),
    spawn_user_hosts(NumUsers-1, UpdatedUserList).


gen_rand_string() ->
    RandString = base64:encode_to_string(crypto:strong_rand_bytes(6)),
    RandString.


rand_table_name() ->
    RandString = gen_rand_string(),
    HashString = io_lib:format("~64.16.0b",[binary:decode_unsigned(crypto:hash(sha256, RandString))]),
    list_to_atom(HashString).


random_subset(List, P) ->
    lists:filter(fun(_) -> rand:uniform() < P end, List).


do_subs(Parent, Child, UserName, UserNames) ->
    Options = lists:delete(UserName, UserNames),
    Choices = random_subset(Options, 0.33),
    [peer:send(Parent, Child, {subscribe, Choice}) || Choice <- Choices],
    Choices.


index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


zipf_sum(0, ZipfSum) ->
    ZipfSum;
zipf_sum(NumUsers, ZipfSum) ->
    NewZipfSum = ZipfSum + (1/NumUsers),
    zipf_sum(NumUsers-1, NewZipfSum).


zipf_freq(SubFreq, User, ZipfSum) ->
    UserIndex = index_of(User, SubFreq),
    ZipfFreq = (1/UserIndex) * ZipfSum,
    ZipfFreq.
    

get_tweet_number(Parent, Child, User, Password, TweetNums) ->
    if 
        TweetNums == false ->
            UserData = {Parent, Child, User, Password, 0};
        true ->
            {_, TweetNum} = TweetNums,
            UserData = {Parent, Child, User, Password, TweetNum}
    end,
    UserData.


make_tweets(0, _, Parent, Child)->
    io:fwrite("~p~p is done tweeting~n", [Parent, Child]);
make_tweets(TweetNum, MaxNum, Parent, Child) when TweetNum =< MaxNum/2 ->
    HT = rand:uniform(10),
    RT = rand:uniform(3)-1,
    if
        HT >= 6 ->
            peer:send(Parent, Child, {make_tweet, gen_rand_string()}),
            timer:sleep(200);
        true ->
            peer:send(Parent, Child, {retweet, RT}),
            timer:sleep(200)
    end;
make_tweets(TweetNum, MaxNum, Parent, Child) ->
    peer:send(Parent, Child, {make_tweet, gen_rand_string()}),
    timer:sleep(200),
make_tweets(TweetNum-1, MaxNum, Parent, Child).


start(NumUsers) ->
    % starts host
    {_, HostPid, HostName} = peer:start(#{name => host, connection => standard_io}),
    peer:call(HostPid, project4, start_host, []),
    timer:sleep(500),

    UserHostList = spawn_user_hosts(NumUsers, []), % replace the 2 lines below

    timer:sleep(500),

    UserNodes = [{
        UserPid,
        peer:call(UserPid, project4, start_user, [rand_table_name(), HostName]),
        gen_rand_string(),
        gen_rand_string()
    } || UserPid <- UserHostList],

    UserNames = [UserName || {_, _, UserName, _} <- UserNodes],

    timer:sleep(500),

    NewUsersFun = fun({Parent, Child, UserName, Password}) ->
        peer:send(Parent, Child, {new_user, UserName, Password})
    end,

    lists:map(NewUsersFun, UserNodes),
    
    timer:sleep(500),

    AllChoices = lists:append([do_subs(Parent, Child, UserName, UserNames) || {Parent, Child, UserName, _} <- UserNodes]),

    SubFreq = lists:reverse(lists:keysort(2, [{UserName, length([X || X <- AllChoices, X =:= UserName])/NumUsers} || UserName <- lists:uniq(AllChoices)])),

    ZipfSum = zipf_sum(NumUsers, 0),

    TweetFreq = [{User, zipf_freq(SubFreq, {User, F}, ZipfSum)} || {User, F} <- SubFreq],

    BaseTweets = rand:uniform(NumUsers),

    NumTweets = [{User, trunc(math:ceil(Freq*BaseTweets))} || {User, Freq} <- TweetFreq],


    UserNodesWithTweets = lists:reverse(lists:keysort(5, [get_tweet_number(Parent, Child, User, Pass, lists:keyfind(User, 1, NumTweets)) || {Parent, Child, User, Pass} <- UserNodes])),

    timer:sleep(500),

    io:fwrite("~p~n", [UserNodesWithTweets]),

    timer:sleep(500),

    [make_tweets(TweetNum, TweetNum, Parent, Child) || {Parent, Child, _, _, TweetNum} <- UserNodesWithTweets],
    
    timer:sleep(1500),

    % to stop host
    [peer:stop(UserPid) || {UserPid, _, _, _} <- UserNodes],
    peer:stop(HostPid).




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