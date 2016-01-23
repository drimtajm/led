%% coding: utf-8
-module(matrix_controller).

-include("matrix.hrl").
-export([go/0, display/2, animate/2, close/0]).

-define(DISPLAY_TYPE, colour_matrix).

get_initial_register_values(colour_matrix) ->
    RegValues0 = [#register_value{register = Address, value = 0}
		  || Address <- lists:seq(16#20, 16#2F)],
    [#register_value{register = 1, value = 0} | RegValues0]
	++ [#register_value{register = 4, value = 1, add = true}];
get_initial_register_values(double_matrix) ->
    [#register_value{register = 16#9, value = 0},
     #register_value{register = 16#a, value = 3},
     #register_value{register = 16#b, value = 7},
     #register_value{register = 16#c, value = 1},
     #register_value{register = 16#f, value = 0}].

get_ce_pin(colour_matrix) -> 8;
get_ce_pin(double_matrix) -> 24.

init() ->
    DisplayType = ?DISPLAY_TYPE,
    RegisterValues = get_initial_register_values(DisplayType),
    Configuration = #configuration{
		       ce_pin = get_ce_pin(DisplayType),
		       initial_register_values = RegisterValues,
		       display_type = DisplayType},
    {ok, Handle} = matrix_lib:init(Configuration),
    put(handle, Handle),
    display([], 0).

close() ->
    Handle = get(handle),
    io:format("Handle is now: ~w~n", [Handle]),
    ok = matrix_lib:teardown(Handle).

handle_code(Code) ->
    {ok,ErlTokens,_}=erl_scan:string(Code),

    %% Now parse the tokens into the abstract form
    {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),

    Bindings0=erl_eval:add_binding('Visa', fun ?MODULE:display/2,
				   erl_eval:new_bindings()),
    Bindings=erl_eval:add_binding('Animera', fun ?MODULE:animate/2,
				  Bindings0),
    %% Now evaluate the string
    erl_eval:exprs(ErlAbsForm,Bindings),
    display([], 0),
    ok.

display(PointList, WaitTime) ->
    do_display(PointList, fun matrix_lib:display/3, WaitTime).

animate(PointList, WaitTime) ->
    do_display(PointList, fun matrix_lib:animate/3, WaitTime).

do_display(PointList0, DisplayFunction, WaitTimeInSeconds) ->
    PointList = case ?DISPLAY_TYPE of
		    double_matrix -> PointList0;
		    colour_matrix ->
			[{Coordinate, get_colour(ColourIndex)}
			 || {Coordinate, ColourIndex} <- PointList0]
		end,
    DisplayFunction(get(handle), ?DISPLAY_TYPE, PointList),
    WaitTimeInMilliseconds = trunc(WaitTimeInSeconds * 1000),
    timer:sleep(WaitTimeInMilliseconds),
    ok.

get_colour(String) when is_list(String) ->
    NewString = unicode:characters_to_list(list_to_binary(String)),
    string:to_lower(NewString);
get_colour(1) -> "röd";
get_colour(2) -> "grön";
get_colour(3) -> "orange".


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
    init(),
    receive_loop(),
    display([], 0),
    close(),
    unregister(code_receiver).
