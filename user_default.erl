-module(user_default).
-export([start/0]).

start() ->
    io:format("Todo app started!~n"),
    State = {0, []},
    start(State).

start({ Count, Todos }) ->
    Task = string:trim(io:get_line("enter a task > ")),
    NewId = Count + 1,
    io:format("new task: ~p~n", [Task]),
    NewTodos = [{ NewId, Task, false } | Todos],
    io:format("all tasks:~n"),
    lists:foreach(
      fun({Id, T, _Done}) -> io:format(" - [~p] ~s~n", [Id, T]) end,
      lists:reverse(NewTodos)
     ),
    start({ NewId, NewTodos }).

