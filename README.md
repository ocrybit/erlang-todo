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
