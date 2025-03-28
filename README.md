# Erlang Todo App

compile

```bash
erlc app.erl
```

run with shell

```bash
erl
```

then

```erlang
app:start() % start
q() % quit shell
```

run without shell

```bash
erl -noshell -s app start -s init stop
```
