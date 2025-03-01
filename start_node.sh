#!/bin/bash

if [ $# -lt 2 ]; then
  echo "Usage: $0 <node_name> <cookie>"
  echo "Example: $0 node1 mycookie"
  exit 1
fi

NODE_NAME=$1
COOKIE=$2

# Start Erlang with distributed node and explicit application loading
erl -sname $NODE_NAME -setcookie $COOKIE \
    -pa build/dev/erlang/orc/ebin \
    -pa build/dev/erlang/khepri/ebin \
    -pa build/dev/erlang/*/ebin \
    -pa _build/default/lib/*/ebin \
    -eval "
        % Explicitly start Khepri dependencies
        application:ensure_all_started(ra),
        application:ensure_all_started(khepri),
        % Verify Khepri is loaded
        case code:ensure_loaded(khepri) of
            {module, khepri} -> io:format(\"Khepri module loaded successfully~n\");
            {error, Reason} -> io:format(\"ERROR: Failed to load Khepri: ~p~n\", [Reason])
        end,
        % Check if the join_cluster function is exported
        case erlang:function_exported(khepri, join_cluster, 1) of
            true -> io:format(\"join_cluster function is available~n\");
            false -> io:format(\"WARNING: join_cluster function is NOT available~n\")
        end,
        % Start your application
        application:ensure_all_started(orc),
        io:format(\"Orc started~n\").
    "
