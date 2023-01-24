-module(login).

-compile([export_all]).

-include_lib("nitro/include/nitro.hrl").

-include_lib("n2o/include/n2o.hrl").

event(init) ->
    nitro:update(
        loginButton,
        #button{
            id = loginButton,
            body = "Login",
            postback = login,
            source = [user, pass]
        }
    ),
    nitro:update(
        newUserButton,
        #button{
            id = newUserButton,
            body = "New User",
            postback = new_user,
            source = [user, pass]
        }
    );
event(login) ->
    UserName = nitro:to_list(nitro:q(user)),
    Password = nitro:to_list(nitro:q(pass)),
    Pid = n2o:session(user_pid),
    Pid ! {login, UserName, Password, self()},
    receive
        {user_status, online} ->
            n2o:user(UserName),
            n2o:session(room, UserName),
            n2o:session(user_pass, Password),
            nitro:redirect("/app/index.htm");
        incorrect ->
            io:fwrite("INCORRECT~n")
    end, 
    ok;
event(new_user) ->
    Pid = project4:start_user('nonode@nohost'),
    UserName = nitro:to_list(nitro:q(user)),
    Password = nitro:to_list(nitro:q(pass)),
    Pid ! {new_user, UserName, Password, self()},
    receive
        {user_status, online} ->
            n2o:user(UserName),
            n2o:session(room, UserName),
            n2o:session(user_pid, Pid),
            n2o:session(user_pass, Password),
            nitro:redirect("/app/index.htm");
        {user_status, exists} ->
            nitro:update(warning, #panel{
                id = warning,
                class = 'alert show',
                body = [
                    #span{class='fas fa-exclamation-circle'},
                    #span{class=msg, body="Warning: That user already exists."},
                    #button{
                        id='warn-btn',
                        class='close-btn',
                        postback = close_alert,
                        body=#span{class='fas fa-times'}
                    }
                ]
            })
    end,
    ok;
event(close_alert) ->
    nitro:update(warning, #panel{
        id = warning,
        class = 'alert hide',
        body = [
            #span{class='fas fa-exclamation-circle'},
            #span{class=msg, body="Warning: That user already exists."},
            #button{
                id='warn-btn',
                class='close-btn',
                postback = close_alert,
                body=#span{class='fas fa-times'}
            }
        ]
    }),
    ok;
event(_) -> [].


