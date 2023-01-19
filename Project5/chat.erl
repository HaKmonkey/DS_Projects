-module(chat).
-include_lib("nitro.hrl").
-compile(export_all).

main() ->
  #dtl { file     = "login",
         app      = review,
         bindings = [ { body, body() } ] }.

body() ->
  [ #span    { id=title,       body="Your nickname: " },
    #textbox { id=user,        body="Anonymous" },
    #panel   { id=history },
    #textbox { id=message },
    #button  { id=send,        source=[user,message],
                               body="Send",
                               postback=chat } ].

event(init) -> wf:reg(room), wf:async("looper",fun loop/1);
event(chat) -> User    = wf:q(user),
               Message = wf:q(message),
               n2o_async:send("looper",{chat,User,Message}).

loop({chat,User,Message}) ->
    Terms = #panel { body = [
            #span {body=User},":", #span { body = Message } ]},
    wf:insert_bottom(history, Terms),
    wf:flush(room).