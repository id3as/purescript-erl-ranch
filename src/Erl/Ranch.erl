-module(erl_ranch@foreign).

-export([ startListenerImpl/6
        , stopListener/1
        , spawnDefaultHandlerImpl/1
        , handshakeImpl/1
        ]).

stopListener(Atom) ->
  fun() ->
      ranch:stop_listener(Atom)
  end.

startListenerImpl(Left, Right, Ref, Module, Options, Handler) ->
    fun() ->
            case ranch:start_listener(Ref, Module, Options, 'erl_ranch@ps', Handler) of
                {ok, ListenerPid} -> Right(ListenerPid);
                {error, Error} -> Left(Error)
            end
    end.

spawnDefaultHandlerImpl(Fn) ->
    fun() ->
            {ok, proc_lib:spawn_link(fun() -> Fn() end)}
    end.

handshakeImpl(Ref) ->
    fun() ->
            {ok, Socket} = ranch:handshake(Ref),
            Socket
    end.
