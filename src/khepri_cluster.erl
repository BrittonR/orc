-module(khepri_cluster).
-export([join/1, leave/0, members/0, status/0]).

%% Join a Khepri cluster through another node
join(NodeStr) when is_binary(NodeStr) ->
    Node = binary_to_atom(NodeStr, utf8),
    try
        case khepri_cluster:join_cluster(Node) of
            ok -> 
                {ok, nil};
            {error, Reason} when is_atom(Reason) -> 
                {error, atom_to_binary(Reason, utf8)};
            {error, Reason} ->
                {error, list_to_binary(io_lib:format("~p", [Reason]))}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("Failed to join cluster: ~p", [Error]))}
    end.

%% Leave the current Khepri cluster
leave() ->
    try
        case khepri_cluster:leave_cluster() of
            ok -> 
                {ok, nil};
            {error, Reason} when is_atom(Reason) -> 
                {error, atom_to_binary(Reason, utf8)};
            {error, Reason} ->
                {error, list_to_binary(io_lib:format("~p", [Reason]))}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("Failed to leave cluster: ~p", [Error]))}
    end.

%% Get members of the Khepri cluster
members() ->
    try
        case khepri_cluster:cluster_members() of
            {ok, Members} -> 
                StringMembers = [atom_to_binary(M, utf8) || M <- Members],
                {ok, StringMembers};
            {error, Reason} when is_atom(Reason) -> 
                {error, atom_to_binary(Reason, utf8)};
            {error, Reason} ->
                {error, list_to_binary(io_lib:format("~p", [Reason]))}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("Failed to get cluster members: ~p", [Error]))}
    end.

%% Get status of the Khepri cluster
status() ->
    try
        case ra_system:info(khepri) of
            {ok, Info} -> 
                Status = list_to_binary(io_lib:format("~p", [Info])),
                {ok, Status};
            {error, Reason} ->
                {error, list_to_binary(io_lib:format("~p", [Reason]))}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("Failed to get cluster status: ~p", [Error]))}
    end.
