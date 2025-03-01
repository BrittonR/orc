-module(erlang_node).
-export([connect/1, nodes/0, node/0, ping/1, is_connected/1, set_cookie/1]).

%% Connect to a node
connect(NodeName) when is_binary(NodeName) ->
    Node = binary_to_atom(NodeName, utf8),
    case net_kernel:connect_node(Node) of
        true -> {ok, nil};
        false -> {error, <<"Failed to connect">>}
    end.

%% Get list of connected nodes
nodes() ->
    Nodes = erlang:nodes(),
    [atom_to_binary(Node, utf8) || Node <- Nodes].

%% Get local node name
node() ->
    atom_to_binary(erlang:node(), utf8).

%% Ping a node
ping(NodeName) when is_binary(NodeName) ->
    Node = binary_to_atom(NodeName, utf8),
    case net_adm:ping(Node) of
        pong -> {ok, nil};
        pang -> {error, <<"Node not reachable">>}
    end.

%% Check if a node is connected
is_connected(NodeName) when is_binary(NodeName) ->
    Node = binary_to_atom(NodeName, utf8),
    lists:member(Node, erlang:nodes()).

%% Set the cookie for distributed Erlang
set_cookie(Cookie) when is_binary(Cookie) ->
    erlang:set_cookie(node(), binary_to_atom(Cookie, utf8)),
    {ok, nil}.
