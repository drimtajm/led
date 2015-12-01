-module(single_matrix).

-export([go/0, display/1]).

-define(CE_PIN, 24).

-define(CHAR0, [16#3C,16#42,16#42,16#42,16#42,16#42,16#42,16#3C]).
-define(CHAR1, [16#10,16#30,16#50,16#10,16#10,16#10,16#10,16#7C]).
-define(SPIRAL, [2#11111111, 2#00000001, 2#11111101, 2#10000101,
		 2#10100101, 2#10111101, 2#10000001, 2#11111111]).
-define(TEST_LIST, ["A1", "B2", "C3", "D4",
		    "A8", "B7", "C6", "D5", "E4", "F3", "G2", "H1"]).

spi_write(Handle, Register, Value) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value]).

init(Handle) ->
    spi_write(Handle, 16#9, 0),
    spi_write(Handle, 16#a, 3),
    spi_write(Handle, 16#b, 7),
    spi_write(Handle, 16#c, 1),
    spi_write(Handle, 16#f, 0).

display_picture(_Handle, [], _N) ->
    ok;
display_picture(Handle, [Value | Rest], N) ->
    spi_write(Handle, N, Value),
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
    SubList = [X-$A+1 || [X, N0] <- List,
			 N == N0 - $0],
    RowValue = convert_indeces_to_int(SubList),
    to_picture(List, [RowValue | Result], N+1).

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

    Bindings=erl_eval:add_binding('Visa',fun single_matrix:display/1,
				  erl_eval:new_bindings()),
    %% Now evaluate the string
    erl_eval:exprs(ErlAbsForm,Bindings),
    ok.

display([{_Coordinate, _Colour} | _] = List) ->
    display([Coordinate || {Coordinate, Colour} <- List,
			   Colour == 1]);
display(List) ->
    Picture = to_picture(List),
    Handle = get(handle),
    io:format("Handle: ~p, Picture: ~p~n", [Handle, Picture]),
    display_picture(get(handle), Picture),
    ok.

clear() ->
    Handle = get(handle),
    display_picture(Handle, lists:duplicate(8, 0)).

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
    clear(),
    receive_loop(),
   %%display_picture(Handle, ?CHAR0),
   %%timer:sleep(200),
   %%display_picture(Handle, ?CHAR1),
   %%timer:sleep(1000),
   %%Picture = to_picture(?TEST_LIST),
   %%display_picture(Handle, Picture),
   %%timer:sleep(2000),
   %%display_picture(Handle, ?SPIRAL),
   %%timer:sleep(500),
   %%TestList = line("A2", "H2") ++ line("B1", "B7") ++ line("E6", "G6"),
   %%TestPicture = to_picture(TestList),
   %%display_picture(Handle, TestPicture),
   %%timer:sleep(3500),
    clear(),
    ok = spi_interface:close_spi_bus(Handle),
    unregister(code_receiver).
