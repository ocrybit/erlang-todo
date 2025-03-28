# Erlang Todo App

compile & run the app.

```bash
erlc app.erl
```

run with shell

```bash
erl
```

then

```erlang
start() % start
q() % quit shell
```

run without shell

```bash
erl -noshell -s app start -s init stop
```

or compile & run the app

```bash
./todo.sh
```

### CL Commands

list tasks

```bash
list
```

add a task

```bash
add "task=Buy groceries"
```

remove a task

```bash
remove 3
```

complete a task

```bash
complete 2
```

clear tasks

```bash
clear
```

## REST API Server

list tasks

```bash
curl http://localhost:8080/list
```

add a task

```bash
curl -X POST -d "task=Buy groceries" http://localhost:8080/add
```

remove a task

```bash
curl -X POST -d "id=3" http://localhost:8080/remove
```

complete a task

```bash
curl -X POST -d "id=2" http://localhost:8080/complete
```

clear tasks

```bash
curl -X POST http://localhost:8080/clear
```
