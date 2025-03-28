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
            {0, []}
    end.

format_todos([]) ->
    "[]";
format_todos(Todos) ->
    Items = lists:map(
	      fun({Id, Task, Done}) ->
		      EscapedTask = escape_string(Task),
		      io_lib:format("{\"id\":~p,\"task\":\"~s\",\"done\":~s}", [
										Id, 
										EscapedTask, 
										atom_to_list(Done)
									       ])
	      end,
	      lists:reverse(Todos)
	     ),
    "[" ++ lists:flatten(string:join(Items, ",")) ++ "]".

escape_string(String) ->
    lists:foldr(fun(C, Acc) ->
			case C of
			    $" -> "\\" ++ [$"] ++ Acc;
			    $\\ -> "\\" ++ [$\\] ++ Acc;
			    $\n -> "\\" ++ [$n] ++ Acc;
			    $\r -> "\\" ++ [$r] ++ Acc;
			    $\t -> "\\" ++ [$t] ++ Acc;
			    _ -> [C | Acc]
			end
		end, "", String).

extract_param(RequestStr, Param) ->
    Pattern = Param ++ "=([^&\r\n]+)",
    case re:run(RequestStr, Pattern, [{capture, [1], list}]) of
        {match, [Value]} -> 
            url_decode(Value);
        _ -> 
            ""
    end.
extract_task(RequestStr) ->
    case re:run(RequestStr, "task=([^&\r\n]+)", [{capture, [1], list}]) of
        {match, [Task]} -> 
            url_decode(Task);
        _ -> 
            "New task"
    end.

extract_id(RequestStr) ->
    case extract_param(RequestStr, "id") of
        "" -> 0;
        IdStr -> 
            {Id, _} = string:to_integer(IdStr),
            Id
    end.
url_decode(String) ->
    url_decode(String, []).

url_decode([], Acc) ->
    lists:reverse(Acc);
url_decode([$%, H1, H2 | Rest], Acc) ->
    Hex = list_to_integer([H1, H2], 16),
    url_decode(Rest, [Hex | Acc]);
url_decode([$+ | Rest], Acc) ->
    url_decode(Rest, [$ | Acc]);
url_decode([C | Rest], Acc) ->
    url_decode(Rest, [C | Acc]).

extract_path(RequestStr) ->
    case re:run(RequestStr, "^(GET|POST)\\s+(/[^\\s?]+)", [{capture, [2], list}]) of
        {match, [Path]} -> Path;
        _ -> "/"
    end.

extract_method(RequestStr) ->
    case re:run(RequestStr, "^(GET|POST)\\s+", [{capture, [1], list}]) of
        {match, [Method]} -> Method;
        _ -> "UNKNOWN"
    end.
start_rest_server() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [
					       binary,
					       {active, false},
					       {reuseaddr, true}
					      ]),
    io:format("REST server started on port 8080~n"),
    spawn(fun() -> accept_connections(ListenSocket) end).

accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept_connections(ListenSocket) end),
    case gen_tcp:recv(Socket, 0) of
        {ok, Request} ->
	    RequestStr = binary_to_list(Request),
	    Method = extract_method(RequestStr),
            Path = extract_path(RequestStr),
	    Response = 
		case {Method, Path} of
		    {"GET", "/list"} ->
			{_, Todos} = load_todos(),
			TodosJson = format_todos(Todos),
			ContentLength = integer_to_list(length(TodosJson)),
			"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " ++ ContentLength ++ "\r\n\r\n" ++ TodosJson;
		    {"POST", "/add"} ->
			{Count, Todos} = load_todos(),
			Task = extract_task(RequestStr),
			NewId = Count + 1,
                        NewTodos = [{NewId, Task, false} | Todos],
			save_todos({NewId, NewTodos}),
			TaskJson = io_lib:format("{\"id\":~p,\"task\":\"~s\",\"done\":false}", [NewId, escape_string(Task)]),
                        FlatJson = lists:flatten(TaskJson),
                        ContentLength = integer_to_list(length(FlatJson)),
			"HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: " ++ ContentLength ++ "\r\n\r\n" ++ FlatJson;
		    {"POST", "/remove"} ->
                        {Count, Todos} = load_todos(),
                        Id = extract_id(RequestStr),
                        NewTodos = lists:filter(fun({I, _, _}) -> I =/= Id end, Todos),
                        save_todos({Count, NewTodos}),
                        ResultJson = io_lib:format("{\"success\":true,\"id\":~p}", [Id]),
                        FlatJson = lists:flatten(ResultJson),
                        ContentLength = integer_to_list(length(FlatJson)),
                        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " ++ ContentLength ++ "\r\n\r\n" ++ FlatJson;
		    {"POST", "/complete"} ->
			{Count, Todos} = load_todos(),
                        Id = extract_id(RequestStr),
			UpdatedTodos = lists:map(
					 fun({I, T, _}) when I =:= Id -> {I, T, true};
					    (Item) -> Item
					 end, 
					 Todos
					),
			save_todos({Count, UpdatedTodos}),
			ResultJson = io_lib:format("{\"success\":true,\"id\":~p,\"done\":true}", [Id]),
                        FlatJson = lists:flatten(ResultJson),
                        ContentLength = integer_to_list(length(FlatJson)),
                        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " ++ ContentLength ++ "\r\n\r\n" ++ FlatJson;
		    {"POST", "/clear"} ->
			save_todos({0, []}),
			ResultJson = "{\"success\":true,\"message\":\"All tasks cleared\"}",
                        ContentLength = integer_to_list(length(ResultJson)),
                        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " ++ ContentLength ++ "\r\n\r\n" ++ ResultJson;
		    _ ->
			"HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found"
		end,
	    gen_tcp:send(Socket, Response);
        {error, closed} ->
            ok
    end,
    gen_tcp:close(Socket).

start() ->
    io:format("Todo app started!~n"),
    start_rest_server(),
    State = load_todos(),
    start(State).

start({ Count, Todos }) ->
    Command = string:trim(io:get_line("enter a command > ")),
    {NewCount, NewTodos} =
	case string:tokens(Command, " ") of
	    ["list"] ->
		{Count, Todos};
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

