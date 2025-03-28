-module(user_default).
-export([start/0]).

start() ->
    Todos = [],
    start(Todos).

start(Todos) ->
    io:format("Todo app started!"),
    Task = string:trim(io:get_line("enter a task > ")),
    io:format("new task: ~p~n", [Task]),
    NewTodos = [Task | Todos],
    io:format("all tasks: ~p~n", [NewTodos]),
    start(NewTodos).

