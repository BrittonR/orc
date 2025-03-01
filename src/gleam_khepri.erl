-module(gleam_khepri).
-export([start/0, put/2, get/1, delete/1, get_many/1, list_keys/1]).

%% Constant for metadata key
-define(METADATA_KEY, '_metadata').

% Helper function to convert Gleam path to Erlang path
convert_path(Path) ->
    lists:map(fun(Item) ->
        % Special case for wildcard and metadata
        case Item of
            <<"_">> -> '_';  % Convert string "_" to atom '_'
            <<"?">> -> '?';  % Convert string "?" to atom '?'
            <<"$">> -> '$';  % Convert string "$" to atom '$'
            <<"_metadata">> -> ?METADATA_KEY;
            _ ->
                % Try to convert string to atom first
                try 
                    binary_to_atom(Item, utf8)
                catch
                    _:_ -> Item
                end
        end
    end, Path).

% Convert Erlang term to Gleam-compatible string
convert_to_gleam(Term) when is_atom(Term) ->
    list_to_binary(atom_to_list(Term));
convert_to_gleam(Term) when is_binary(Term) ->
    Term;
convert_to_gleam(Term) ->
    % For other types, convert to binary string
    list_to_binary(io_lib:format("~p", [Term])).

% Start Khepri and convert result to Gleam format
start() ->
    case khepri:start() of
        ok -> {ok, nil};
        Error -> Error
    end.

% Put a value and update metadata
put(Path, Value) ->
    ErlangPath = convert_path(Path),
    Result = khepri:put(ErlangPath, Value),
    
    % If we're putting a value in a directory, update the metadata key list
    try
        case length(Path) > 1 andalso Result =:= ok of
            true ->
                % Get the parent directory
                ParentPath = lists:droplast(Path),
                % Get the key
                Key = lists:last(Path),
                % Update metadata for parent directory to track keys
                update_metadata(ParentPath, Key, add);
            false ->
                ok
        end
    catch
        _:_ -> ok % Ignore errors in metadata updates
    end,
    
    case Result of
        ok -> {ok, nil};
        Error -> Error
    end.

% Get a value
get(Path) ->
    ErlangPath = convert_path(Path),
    khepri:get(ErlangPath).

% List all keys under a specific path
list_keys(Path) ->
    ErlangPath = convert_path(Path),
    
    % Try to get the metadata keys list first
    MetaPath = ErlangPath ++ [?METADATA_KEY],
    
    case khepri:get(MetaPath) of
        {ok, KeysList} when is_list(KeysList) ->
            % Convert to binary strings for Gleam
            BinaryKeys = lists:map(fun convert_to_gleam/1, KeysList),
            {ok, BinaryKeys};
            
        _ ->
            % Metadata not available, try getting directories with wildcards
            Keys = discover_keys(ErlangPath),
            
            % Try to update metadata for future queries
            try
                case length(Keys) > 0 of
                    true -> 
                        BinaryKeys = lists:map(fun convert_to_gleam/1, Keys),
                        khepri:put(MetaPath, Keys),
                        {ok, BinaryKeys};
                    false -> 
                        {ok, []}
                end
            catch
                _:_ -> {ok, lists:map(fun convert_to_gleam/1, Keys)}
            end
    end.

% Discover keys using various methods
discover_keys(Path) ->
    % Try wild card approach first
    WildcardPath = Path ++ ['_'],
    case khepri:get_many(WildcardPath) of
        {ok, Map} when map_size(Map) > 0 ->
            % Extract keys from the map paths
            maps:fold(
                fun(K, _V, Acc) ->
                    % Only include direct children 
                    case length(K) of
                        Len when Len =:= length(Path) + 1 ->
                            LastComp = lists:last(K),
                            % Skip metadata entry
                            case LastComp of
                                ?METADATA_KEY -> Acc;
                                _ -> [LastComp | Acc]
                            end;
                        _ -> Acc
                    end
                end,
                [],
                Map
            );
        _ ->
            % Try iterating through common keys
            walk_tree(Path)
    end.

% Walk the tree to find keys (last resort approach)
walk_tree(Path) ->
    % Try a few common key patterns to see what's there
    CommonPatterns = ['_', '?', '#'],
    
    lists:foldl(
        fun(Pattern, Acc) ->
            PatternPath = Path ++ [Pattern],
            case khepri:get_many(PatternPath) of
                {ok, Map} when map_size(Map) > 0 ->
                    % Extract keys
                    Keys = maps:fold(
                        fun(K, _V, KeyAcc) ->
                            % Only consider direct children
                            case length(K) of
                                Len when Len =:= length(Path) + 1 ->
                                    LastComp = lists:last(K),
                                    case LastComp of
                                        ?METADATA_KEY -> KeyAcc;
                                        _ -> [LastComp | KeyAcc]
                                    end;
                                _ -> KeyAcc
                            end
                        end,
                        [],
                        Map
                    ),
                    Keys ++ Acc;
                _ -> Acc
            end
        end,
        [],
        CommonPatterns
    ).

% Get multiple values matching a path pattern
get_many(PathPattern) ->
    ErlangPath = convert_path(PathPattern),
    
    % Try the standard get_many first
    case khepri:get_many(ErlangPath) of
        {ok, Map} when map_size(Map) > 0 -> 
            % Convert to Gleam format
            Pairs = maps:fold(
                fun(K, V, Acc) ->
                    LastComp = lists:last(K),
                    % Skip metadata
                    case LastComp of
                        ?METADATA_KEY -> Acc;
                        _ -> 
                            KeyStr = convert_to_gleam(LastComp),
                            [{KeyStr, V} | Acc]
                    end
                end,
                [],
                Map
            ),
            {ok, Pairs};
            
        _ ->
            % Manual approach - get each entry individually
            case lists:last(ErlangPath) of
                '_' ->
                    % Get the parent path
                    ParentPath = lists:droplast(PathPattern),
                    
                    % Get the list of keys
                    {ok, Keys} = list_keys(ParentPath),
                    
                    % For each key, get the value
                    Pairs = lists:filtermap(
                        fun(Key) ->
                            FullPath = ParentPath ++ [Key],
                            case get(FullPath) of
                                {ok, Value} -> {true, {Key, Value}};
                                _ -> false
                            end
                        end,
                        Keys
                    ),
                    {ok, Pairs};
                    
                _ -> {ok, []}
            end
    end.

% Delete a value and update metadata
delete(Path) ->
    ErlangPath = convert_path(Path),
    
    % If deleting a value in a directory, update the metadata
    try
        case length(Path) > 1 of
            true ->
                % Get the parent directory and key
                ParentPath = lists:droplast(Path),
                Key = lists:last(Path),
                
                % Update metadata for parent directory
                update_metadata(ParentPath, Key, remove);
            false -> ok
        end
    catch
        _:_ -> ok % Ignore errors in metadata updates
    end,
    
    case khepri:delete(ErlangPath) of
        ok -> {ok, nil};
        Error -> Error
    end.

% Helper to update metadata
update_metadata(ParentPath, Key, Operation) ->
    % Convert parent path to Erlang format
    ErlangParentPath = convert_path(ParentPath),
    
    % Build metadata path
    MetaPath = ErlangParentPath ++ [?METADATA_KEY],
    
    % Get existing metadata or create new
    KeysList = case khepri:get(MetaPath) of
        {ok, ExistingKeys} when is_list(ExistingKeys) -> ExistingKeys;
        _ -> []
    end,
    
    % String or atom key to atom
    KeyAtom = case is_binary(Key) of
        true -> binary_to_atom(Key, utf8);
        false -> Key
    end,
    
    % Update the keys list based on operation
    UpdatedKeys = case Operation of
        add ->
            case lists:member(KeyAtom, KeysList) of
                false -> [KeyAtom | KeysList];
                true -> KeysList
            end;
        remove ->
            lists:delete(KeyAtom, KeysList)
    end,
    
    % Store updated metadata
    khepri:put(MetaPath, UpdatedKeys).
