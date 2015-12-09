-module(led_array).

-export([go/0, display/1]).

-define(CE_PIN, 8).

spi_write(Handle, Register, Value) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value]).

spi_read(Handle, Register) ->
    {ok, [_, Value]} =
	spi_interface:transfer_spi_data(Handle, [Register, 0]),
    Value.

init(Handle) ->
    Configuration = spi_read(Handle, 4),
%%    spi_write(Handle, 7, 0),
    spi_write(Handle, 1, 0),
    lists:foreach(fun (Address) -> spi_write(Handle, Address, 0) end,
		  lists:seq(16#20, 16#2F)),
    spi_write(Handle, 4, Configuration bor 1),
    timer:sleep(2000).

display_picture(_Handle, [], _N) ->
    ok;
display_picture(Handle, [{ValueRed, ValueGreen} | Rest], N) ->
    spi_write(Handle, (16#20 + (N - 1)), ValueRed),
    spi_write(Handle, (16#28 + (N - 1)), ValueGreen),
    display_picture(Handle, Rest, N+1).

display_picture(Handle, Picture) ->
    display_picture(Handle, Picture, 1).

convert_indeces_to_int(List) ->
    lists:foldl(fun (X, Value0) ->
			Value0 bor (1 bsl (8-X))
		end, 0, List).

to_picture(List) ->
    to_picture(lists:usort(List), [], 1).

to_picture(_List, Result, 9) ->
    lists:reverse(Result);
to_picture(List, Result, N) ->
    SubListRed = [X-$A+1 || {[X, N0], C} <- List,
			    N == N0 - $0, C rem 2 == 1],
    SubListGreen = [X-$A+1 || {[X, N0], C} <- List,
			    N == N0 - $0, C >= 2],
    RowValueRed = convert_indeces_to_int(SubListRed),
    RowValueGreen = convert_indeces_to_int(SubListGreen),
    to_picture(List, [{RowValueRed, RowValueGreen} | Result], N+1).

line([X1, Y], [X2, Y]) ->
    lists:map(fun (X) ->
		      [X, Y]
	      end, lists:seq(X1, X2));
line([X, Y1], [X, Y2]) ->
    lists:map(fun (Y) ->
		      [X, Y]
	      end, lists:seq(Y1, Y2)).

handle_code(Code) ->
    {ok,ErlTokens,_}=erl_scan:string(Code),

    %% Now parse the tokens into the abstract form
    {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),

    Bindings=erl_eval:add_binding('Visa',fun led_array:display/1,
				  erl_eval:new_bindings()),
    %% Now evaluate the string
    erl_eval:exprs(ErlAbsForm,Bindings),
    ok.

display(List) ->
    Picture = to_picture(List),
    Handle = get(handle),
    io:format("Handle: ~p, Picture: ~p~n", [Handle, Picture]),
    display_picture(get(handle), Picture),
    ok.

receive_loop() ->
    Result =
	receive
	    {code, Code} ->
		io:format("Received code.~n"),
		handle_code(Code);
	    {close, undefined} ->
		io:format("Received close request.~n"),
		close
	end,
    if (Result /= close) ->
	    receive_loop();
       true ->
	    close
    end.

go() ->
    register(code_receiver, self()),
    {ok, Handle} = spi_interface:open_spi_bus(?CE_PIN),
    init(Handle),
    put(handle, Handle),
    Zeros = lists:duplicate(8, {0, 0}),
    display_picture(Handle, Zeros),
    receive_loop(),
    display_picture(Handle, Zeros),
    %%Configuration = spi_read(Handle, 4),
    %%spi_write(Handle, 4, Configuration band 2#11111110),
    ok = spi_interface:close_spi_bus(Handle),
    unregister(code_receiver).
