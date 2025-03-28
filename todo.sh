#!/bin/bash
erlc user_default.erl && erl -noshell -eval "user_default:start()."
