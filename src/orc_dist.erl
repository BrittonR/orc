-module(orc_dist).
-export([start_distributed/2, get_node_name/0, list_nodes/0]).

% Start the distributed Erlang system with the given node name and cookie
start_distributed(NodeName, Cookie) when is_binary(NodeName), is_binary(Cookie) ->
    try
        % Convert to atoms
        NodeAtom = binary_to_atom(NodeName, utf8),
        CookieAtom = binary_to_atom(Cookie, utf8),
        
        % Set cookie first (this works before distribution starts)
        erlang:set_cookie(node(), CookieAtom),
        
        % Then start distribution
        case net_kernel:start([NodeAtom, shortnames]) of
            {ok, _Pid} -> {ok, nil};
            {error, {already_started, _}} -> 
                % If already started, make sure cookie is still set
                erlang:set_cookie(node(), CookieAtom),
                {ok, nil};
            {error, Reason} -> {error, list_to_binary(io_lib:format("~p", [Reason]))}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("~p", [Error]))}
    end.

% Get the current node name
get_node_name() ->
    atom_to_binary(node(), utf8).

% Get list of connected nodes
list_nodes() ->
    [atom_to_binary(Node, utf8) || Node <- nodes()].
