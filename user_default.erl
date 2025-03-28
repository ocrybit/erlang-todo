-module(user_default).
-export([start/0]).

start() ->
    io:format("Todo app started!~n"),
    State = {0, []},
    start(State).

start({ Count, Todos }) ->
    Command = string:trim(io:get_line("enter a command > ")),
    {NewCount, NewTodos} =
	case string:tokens(Command, " ") of
	    ["add" | TaskWords] ->
		Task = string:join(TaskWords, " "),
		NewId = Count + 1,
		{NewId, [{ NewId, Task, false } | Todos]};
	    ["remove",  IdStr] ->
		{Id, _} = string:to_integer(IdStr),
		Filtered = lists:filter(fun({I, _, _}) -> I =/= Id end, Todos),
		{Count, Filtered};
	    ["complete", IdStr] ->
		{Id, _} = string:to_integer(IdStr),
		Updated = lists:map(
			    fun
				({I, T, _}) when I =:= Id -> {I, T, true};
				(Item) -> Item
			    end,
			    Todos
			   ),
		{Count, Updated};
	    _ ->
		io:format("invalid~n"),
		{Count, Todos}
	end,
    io:format("all tasks:~n"),
    lists:foreach(
      fun({Id, T, Done}) -> io:format(" - [~p] ~s (~p) ~n", [Id, T, Done]) end,
      lists:reverse(NewTodos)
     ),
    start({ NewCount, NewTodos }).

