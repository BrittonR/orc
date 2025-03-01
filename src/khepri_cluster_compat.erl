-module(khepri_cluster_compat).
-export([join/1, leave/0, is_clustered/0, cluster_members/0]).

%% Join a Khepri cluster through another node
join(Node) when is_atom(Node) ->
    try
        %% Try different possible join function names
        case erlang:function_exported(khepri, join_cluster, 1) of
            true ->
                khepri_cluster:join_cluster(Node);
            false ->
                case erlang:function_exported(khepri, join, 1) of
                    true ->
                        khepri_cluster:join(Node);
                    false ->
                        case erlang:function_exported(ra, join_peer, 2) of
                            true ->
                                %% Try to join using underlying Ra
                                SystemId = get_ra_system(),
                                ra:join_peer(SystemId, Node);
                            false ->
                                {error, join_function_not_found}
                        end
                end
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% Leave the current Khepri cluster
leave() ->
    try
        %% Try different possible leave function names
        case erlang:function_exported(khepri, leave_cluster, 0) of
            true ->
                khepri_cluster:leave_cluster();
            false ->
                case erlang:function_exported(khepri, leave, 0) of
                    true ->
                        khepri_cluster:leave();
                    false ->
                        {error, leave_function_not_found}
                end
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% Check if node is in a cluster
is_clustered() ->
    try
        cluster_members() =/= [node()]
    catch
        _:_ ->
            false
    end.

%% Get members of the Khepri cluster
cluster_members() ->
    try
        case erlang:function_exported(khepri, cluster_members, 0) of
            true ->
                case khepri_cluster:cluster_members() of
                    {ok, Members} -> Members;
                    _ -> [node()]
                end;
            false ->
                %% Try to get cluster members using Ra
                SystemId = get_ra_system(),
                case ra:members(SystemId) of
                    {ok, Members, _Leader} -> [N || {_, N} <- Members];
                    _ -> [node()]
                end
        end
    catch
        _:_ ->
            [node()]
    end.

%% Helper to get the Ra system ID
get_ra_system() ->
    %% Best guess of Ra system ID - adjust if needed
    khepri.
