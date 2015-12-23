
-module(double_matrix).

-export([go/0, display/1, animate/1, init/0]).

-define(CE_PIN, 24).

-define(CHAR0, [16#3C,16#42,16#42,16#42,16#42,16#42,16#42,16#3C]).
-define(CHAR1, [16#10,16#30,16#50,16#10,16#10,16#10,16#10,16#7C]).
-define(SPIRAL, [2#11111111, 2#00000001, 2#11111101, 2#10000101,
		 2#10100101, 2#10111101, 2#10000001, 2#11111111]).
-define(TEST_LIST, ["A1", "B2", "C3", "D4",
		    "A8", "B7", "C6", "D5", "E4", "F3", "G2", "H1"]).

spi_write(Handle, Register, Value) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value,
					     Register, Value]).
spi_write(Handle, Register, Value1, Value2) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value1,
					     Register, Value2]).

init() ->
    {ok, Handle} = spi_interface:open_spi_bus(?CE_PIN),
    init(Handle),
    put(handle, Handle),
    Zeros = lists:duplicate(8, 0),
    display_chars(Handle, Zeros, Zeros).

close() ->
    Handle = get(handle),
    io:format("Handle is now: ~w~n", [Handle]),
    ok = spi_interface:close_spi_bus(Handle).

init(Handle) ->
    spi_write(Handle, 16#9, 0),
    spi_write(Handle, 16#a, 3),
    spi_write(Handle, 16#b, 7),
    spi_write(Handle, 16#c, 1),
    spi_write(Handle, 16#f, 0).

display_chars(_Handle, [], [], _N) ->
    ok;
display_chars(Handle, [Value1 | Rest1], [Value2 | Rest2], N) ->
    spi_write(Handle, N, Value2, Value1),
    display_chars(Handle, Rest1, Rest2, N+1).

display_chars(Handle, Char1, Char2) ->
    display_chars(Handle, Char1, Char2, 1).

convert_indeces_to_int(List) ->
    lists:foldl(fun (X, Value0) ->
			Value0 bor (1 bsl (8-X))
		end, 0, List).

to_chars(List) ->
    to_chars(List, [], [], 1).

to_chars(_List, ResultL, ResultR, 9) ->
    {lists:reverse(ResultL), lists:reverse(ResultR)};
to_chars(List, ResultL, ResultR, N) ->
    SubList = [X-$A+1 || [X, N0] <- List,
			 N == N0 - $0],
    SubListL = [X || X <- SubList, X =< 8],
    SubListR = [X-8 || X <- SubList, X > 8],
    RowValueL = convert_indeces_to_int(SubListL),
    RowValueR = convert_indeces_to_int(SubListR),
    to_chars(List, [RowValueL | ResultL],
	     [RowValueR | ResultR], N+1).

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

    Bindings0=erl_eval:add_binding('Visa',fun double_matrix:display/1,
				   erl_eval:new_bindings()),
    Bindings=erl_eval:add_binding('Animera',fun double_matrix:animate/1,
				  Bindings0),
    %% Now evaluate the string
    erl_eval:exprs(ErlAbsForm,Bindings),
    ok.

display([{_Coordinate, _Colour} | _] = List) ->
    display([Coordinate || {Coordinate, Colour} <- List,
			   Colour == 1]);
display(List) ->
    {CharL, CharR} = to_chars(List),
    Handle = get(handle),
    io:format("Handle: ~p, CharL: ~p, CharR: ~p~n", [Handle, CharL, CharR]),
    display_chars(get(handle), CharL, CharR),
    ok.

animate([]) ->
    ok;
animate(List0) ->
    List = sort_by_row(List0),
    animate([hd(List)], tl(List)).

sort_by_row(List) ->
    lists:sort(fun ([_LetterX, DigitX], [_LetterY, DigitY]) ->
		       DigitX =< DigitY
	       end, List).

animate(DisplayList, []) ->
    display(DisplayList);
animate(DisplayList, [First | Rest]) ->
    display(DisplayList),
    timer:sleep(5),
    animate([First | DisplayList], Rest).

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
    Zeros = lists:duplicate(8, 0),
    display_chars(Handle, Zeros, Zeros),
    receive_loop(),
   %%display_chars(Handle, ?CHAR0, ?CHAR1),
   %%timer:sleep(200),
   %%display_chars(Handle, ?CHAR1, ?CHAR1),
   %%timer:sleep(1000),
   %%display_chars(Handle, ?SPIRAL, ?CHAR1),
   %%timer:sleep(500),
   %%TestList = line("A2", "M2") ++ line("B1", "B7") ++ line("E6", "P6"),
   %%{CL, CR} = to_chars(TestList),
   %%display_chars(Handle, CL, CR),
   %%timer:sleep(3500),
    Zeros = lists:duplicate(8, 0),
    display_chars(Handle, Zeros, Zeros),
    close(),
    unregister(code_receiver).
