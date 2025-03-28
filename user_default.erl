-module(user_default).
-export([start/0]).

save_todos({Count, Todos}) ->
    Bin = term_to_binary({Count, Todos}),
    ok = file:write_file("todos.db", Bin).

load_todos() ->
    case file:read_file("todos.db") of
        {ok, Bin} ->
            binary_to_term(Bin);
        {error, _} ->
            {0, []}  % default if file doesn't exist
    end.

start() ->
    io:format("Todo app started!~n"),
    State = load_todos(),
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
		Updated
		    = lists:map(
			fun
			    ({I, T, _}) when I =:= Id -> {I, T, true};
			    (Item) -> Item
			    end,
			Todos
		       ),
		{Count, Updated};
	    ["clear"] ->
		{Count, []};
	    _ ->
		io:format("invalid~n"),
		{Count, Todos}
	end,
    io:format("all tasks:~n"),
    lists:foreach(
      fun({Id, T, Done}) -> io:format(" - [~p] ~s (~p) ~n", [Id, T, Done]) end,
      lists:reverse(NewTodos)
     ),
    save_todos({NewCount, NewTodos}),
    start({ NewCount, NewTodos }).

