-module(index).

-compile(export_all).

-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").

write_match(Match) ->
    nitro:insert_top(history,nitro:render(#message{body = Match})).

listener(Room) ->
    receive
        {subscribed, SubbedTo} ->
            Match = "Subscribed to " ++ SubbedTo,
            n2o:send({topic, Room}, #client{data = {subscribed, Match}});
        {print_tweet, TweetMessage} ->
            n2o:send({topic, Room}, #client{data = {print_tweet, TweetMessage}});
        {print_search_results, MatchList} ->
            n2o:send({topic, Room}, #client{data = {print_search_results, MatchList}})
    end,
    listener(Room).

event(init) ->
    Room = n2o:session(room),
    EarPid = spawn(?MODULE, listener, [Room]),
    io:fwrite("~p~n", [Room]),
    Key = {topic, Room},
    n2o:reg(Key),
    n2o:reg(n2o:sid()),
    UserName = n2o:user(),
    nitro:clear(history),
    nitro:update(home_title, #title{body=UserName ++ "'s Home"}),
    nitro:update(
        logout,
        #button{
            id = logout,
            body = "Logout\n\n" ++ UserName,
            postback = logout
        }
    ),
    nitro:update(
        mentions,
        #button{
            id = mentions,
            body = "Search\nMentions",
            postback = search_mentions
        }
    ),
    nitro:update(
        subs,
        #button{
            id = subs,
            body = "Search\nSubs",
            postback = search_subs
        }
    ),
    nitro:update(
        tag_btn,
        #button{
            id = tag_btn,
            body = "Search\nTags",
            postback = search_tags,
            source = [tags]
        }
    ),
    nitro:update(
        heading,
        [#h2{
            id = heading,
            body = UserName ++ "'s Home"
        }]
    ),
    nitro:update(
        send,
        #button{
            id = send,
            body = "Tweet",
            postback = tweet,
            source = [message]
        }
    ),
    nitro:update(
        subscribe,
        #button{
            id = subscribe,
            body = "Subscribe",
            postback = sub_to,
            source = [sub_text]
        }
    ),
    nitro:update(
        re_tweet,
        #button{
            id = re_tweet,
            body = "Re-Tweet",
            postback = retweet,
            source = [tweet_id]
        }
    ),
    Pid = n2o:session(user_pid),
    Pid ! {store_index_pid, EarPid};
event(logout) ->
    Pid = n2o:session(user_pid),
    Pid ! log_off,
    n2o:user([]),
    n2o:session(user_pass, []),
    nitro:redirect("/app/login.htm");
event(sub_to) ->
    Pid = n2o:session(user_pid),
    SubToUser = nitro:to_list(nitro:q(sub_text)),
    Pid ! {subscribe, SubToUser},
    nitro:update(
        sub_text,
        #textarea{
            id = sub_text,
            autofocus = true,
            placeholder = "User"
        }
    ),
    ok;
event(tweet) ->
    Pid = n2o:session(user_pid),
    Tweet = nitro:to_list(nitro:q(message)),
    Pid ! {make_tweet, Tweet},
    nitro:update(
        message,
        #textarea{
            id = message,
            rows = 2,
            autofocus = true,
            placeholder = "What's happening?"
        }
    ),
    ok;
event(retweet) ->
    Pid = n2o:session(user_pid),
    TweetId = nitro:to_list(nitro:q(tweet_id)),
    Pid ! {retweet, list_to_integer(TweetId)},
    nitro:update(
        tweet_id,
        #textarea{
            id = tweet_id,
            autofocus = true,
            placeholder = "Tweet Id"
        }
    ),
    ok;
event(search_mentions) ->
    Pid = n2o:session(user_pid),
    Pid ! {search_tweets_by_mention},
    ok;
event(search_subs) ->
    Pid = n2o:session(user_pid),
    Pid ! {search_tweets_by_subscription},
    ok;
event(search_tags) ->
    Pid = n2o:session(user_pid),
    Tag = nitro:to_list(nitro:q(tags)),
    Pid ! {search_tweets_by_tag, Tag},
    nitro:update(
        tags,
        #textarea{
            id = tags,
            autofocus = true,
            placeholder = "#Tag"
        }
    ),
    ok;
event(#client{data = {subscribed, Match}}) ->
    write_match(Match),
    ok;
event(#client{data = {print_tweet, TweetMessage}}) ->
    write_match(TweetMessage),
    ok;
event(#client{data = {print_search_results, MatchList}}) ->
    [write_match(Tweet) || Tweet <- MatchList],
    ok;
event(_) -> ok.