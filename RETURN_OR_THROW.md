# Throw or return a tuple {error, Reason}

Mandatory read: https://learnyousomeerlang.com/errors-and-exceptions

Suggested read: http://erlang.org/doc/reference_manual/errors.html

## TL; DR
* avoid catching exception only to wrap it into ```{error, Exception}``` 
tuple, let the caller decide whether to handle error with ```try ... catch```, 
or pass it up the call stack (so initial throw location is not lost)
* return ```{error, Reason}``` for errors only (if empty queue is not an
 error, return ```empty```, not ```{error, empty}```)
* return a tuple when immediate caller handles both ```{ok, Value}``` 
 and ```{error, Reason}``` without simply bubbling up ```{error, Reason}```


## Returning a tuple
Recommended when calling code is *expected* to handle all returned values.
Example:

    case lists:keytake(Key, 1, List) of
        {value, Tuple, List2} ->
            consume(Tuple);
        false ->
            notify_missing_key(Key)
    end.

Code above expresses the intent: missing key in the list is *expected*. 
It's not an error, hence ```false``` atom returned from lists:keytake.

Another example of multiple flows *expected* from the caller would be
queue API. In most cases, code using queue *expects* the queue to be
empty at some point:

    case queue:out(Queue) of
        {{value, Item}, Q2} ->
            {reply, {ok, Item}, State#state{queue = Q2}};
        {empty, Q1} ->
            {noreply, State, hibernate}
    end.

Yet another example would be iterating over a queue. 'Empty queue' is
completely expected when the iteration is over. So queue:out/1 must not
be throwing to indicate an empty queue.

One more example of expected:

    get_permissions(FbId) ->
        case file:read_file(FbId) of
            {ok, Binary} ->
                binary_to_term(Binary);
            {error, enoent} ->
                % it's *not* an error to have this user missing
                fbid_does_not_exist
        end.
            
### Converting a tuple to exception

When code is not ready to handle anything other than success, there is
a simple way to express this:

    {ok, Binary} = file:read_file("/tmp/file").
    
Code above is a statement "I cannot proceed if file cannot be read".
Bonus: call stack points the exact location (module, line) where this
error occurred.

If there is a handler for this error, it can be done as:

    catch
        error:{badmatch, {error, enoent}} ->
            % recover from missing file...


### Bubble wrap technique

Bubbling up the error is discouraged. Example:

    case file:read_file("/tmp/file") of
        {ok, Binary} ->
            consume_binary(Binary);
        {error, Reason} ->
            {error, Reason}
    end.

Deficiencies of bubbling up the error:
 * requires 'case' statement in every function in the call stack up to
a point where this error can be handled
 * loses context (stack trace to actual error location)
 * prevents from using convenient syntax, see example below
 
Non-wrapped form:

    use_fd(open(filename(join("a", "b", "ext")))).
    
Bubble-wrap form:
    
    case join("a", "b", "ext") of
       {ok, Joined} ->
          case filename(Joined) of
             {ok, Filename} ->
                case open(Filename) of
                   {ok, Fd} ->
                      use_fd(Fd);
                   Else ->
                      Else
                end;
            Else1 ->
                Else1
           end;
       Else2 ->
           Else2
    end.

## Throwing an exception
For most cases, exception is thrown by the underlying library (OTP/ERTS).

### Throwing with throw/1
Expresses a condition that is *expected* to be handled somewhere up
the call stack, potentially many frames above the throw/1 location. A
good replacement for 'bubble wrap' logic above.

    try
        Joined = join("a", "b", "ext"),
        Filename = filename(Joined),
        Fd = open(Filename), % this one does throw(enoent)
        use_fd(fd)
    catch
        enoent ->
           % it is known how to handle enoent here, so do this here
           
           % do not catch any other exceptions - we cannot handle them here
    end.    


It is not recommended to ```throw``` across different modules. This
defines an interface which cannot be described using ```-spec ...```
statement. It is recommended to limit the use of throws only for local
returns to a single module.

Less common example is returning default value through multiple stack frames.
This removes the need to bubble-wrap and unwrap in all intermediate 
functions.

    get_value(Key) ->
        try
            find_key(Key)
        catch
            {default, Default} ->
                Default
        end.
        
    find_key(Key) ->
        Table = find_table(Key),
        find_key_in_table(Table, Key).
        
    find_key_in_table(Table, Key) ->
        case ets:lookup(Table, Key) of
            [Ret] ->
                Ret;
            [] ->
                % don't want to bubble wrap this one in
                %  find_key or any other imtermediate 
                %  function.
                throw({default, {Table, Key, missing})
        end.
        
Example above is acceptable, but not recommended, especially for high-churn
code, as exception may eventually sneak through module borders and become a
part of an interface.


### Errors
In most cases, errors are generated by OTP libraries (including ERTS).
Throwing an error means that exceptional situation cannot be handled where
it occurred. However it is expected that the error is handled without need 
to crash the process.

An example of this behaviour:

    handle_call({get, Key}, _From, State) ->
        try
            One = do_1(Key),
            Two = do_2(One),
            do_3(Two)
        catch
            error:badarith ->
                % we know how to recover after badarith, because we
                %   *expect* it to happen
                Recovered = recover_here(),
                {reply, {ok, Recovered}, State}
        end.

Direct calls to error/1,2 should be used to denote a *logical* error in the code.

### Exiting
When it is clear that error cannot be handled within this process, 
exit/1,2 is to be used. And example of this behaviour is process exiting when
error (including those raised by error/1,2) is not handled.

Another example is a worker process with corrupt state. It can no longer
continue, and must crash to be restarted by the supervisor.

### Raise, or do not use raise/3
This function is not intended to be used by the application developer,
and of any help only when stack trace needs to be modified. This kind
of modification is often misleading and is usually discouraged. In
short, raise/3 is reserved for OTP.

## Catching an exception
Rule of thumb: exception should be caught at a level where it is clear
how to handle it.

***Avoid catching an exception just to duly note it and pass forward***:

    % do *not* do this!
    try
        statement()
    catch
        Class:Reason:Stack ->
           ?LOG_ERROR("Got an error ~p:~p (~p)", [Class, Reason, Stack]),
           error(Reason)
    end.
    
General rule of thumb:

***Catch expected exceptions*** and do not attempt to catch exceptions when
there is no clear recovery done where it's caught.