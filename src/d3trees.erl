-module(d3trees).

-export([
        new/1
      , upsert/3
      , lookup/2
      , increment/3
    ]).
-export_type([
        name/0
      , value/0
      , path/0
      , tree/0
    ]).
-type name()  :: term().
-type value() :: term().
-type path()  :: [name()].
-type tree()  :: #{
        name     := name()
      , value    => value()
      , children => [tree()]
    }.

%%  %%% new/1 usage %%%
%% 1> Tree = d3trees:new("RootNode").
%% #{name => "RootNode"}
-spec new(Name::name()) -> Tree::tree().
new(Name) ->
    #{name=>Name}.

%% %%% upsert/3 usage %%%
%% 2> UpdatedTree = d3trees:upsert(["FirstNode", "SecondNode"], "SecondNodeValue", Tree).
%% #{
%%     name => "RootNode"
%%   , children => [#{
%%         name => "FirstNode"
%%       , children => [#{
%%             name => "SecondNode"
%%           , value => "SecondNodeValue"
%%         }] % end of "FirstNode" children
%%     }] % end of "RootNode" children
%% }
-spec upsert(
        PathBelowRoot::path(), Value::value(), Tree::tree()
    ) -> UpdatedTree::tree().
upsert([], Value, Tree) ->
    Tree#{value=>Value};
upsert([Name|Names], Value, Tree) ->
    Children0 = case Tree of
        #{children:=C} -> C;
        _ -> []
    end,
    Child0 = find_child(Name, Children0),
    Child1 = upsert(Names, Value, Child0),
    Children1 = upsert_child(Child1, Children0),
    Tree#{children=>Children1}.

%% %%% lookup/2 usage %%%
%% 3> d3trees:lookup(["FirstNode", "SecondNode"], UpdatedTree).
%% {value, "SecondNodeValue"}
%% 4> d3trees:lookup(["FirstNode"], UpdatedTree).
%% none
-spec lookup(
        PathBelowRoot::path(), Tree::tree()
    ) -> {value, Value::value()} | none.
lookup(PathBelowRoot, Tree) ->
    try lookup_(PathBelowRoot, Tree) of
        Res -> Res
    catch
        throw:none -> none
    end.
lookup_([], Tree) ->
    case Tree of
        #{value:=Value} -> {value, Value};
        _ -> throw(none)
    end;
lookup_([Name|Names], Tree) ->
    Children = case Tree of
        #{children:=C} -> C;
        _ -> throw(none)
    end,
    Child = find_child(Name, Children),
    lookup_(Names, Child).

%% %%% increment/3 usage %%%
%% 5> Tree.
%% #{name => "RootNode"}
%% 6> IntegerTree = d3trees:increment(["x", "y"], 10, Tree).
%% #{
%%     name => "RootNode"
%%   , children => [#{
%%         name => "x"
%%       , children => [#{
%%             name => "y"
%%           , value => 10
%%         }] % end of "x" children
%%     }] % end of "RootNode" children
%% }
%% 7> d3trees:increment(["x", "y"], 2, IntegerTree).
%% #{
%%     name => "RootNode"
%%   , children => [#{
%%         name => "x"
%%       , children => [#{
%%             name => "y"
%%           , value => 12
%%         }] % end of "x" children
%%     }] % end of "RootNode" children
%% }
-spec increment(
        PathBelowRoot::path(), Value::integer(), Tree::tree()
    ) -> UpdatedTree::tree().
increment(Path, Value, Tree) when is_integer(Value) ->
    UpdatedValue = case lookup(Path, Tree) of
        {value, V} when is_integer(V) ->
            V + Value;
        none ->
            Value;
        {value, V} ->
            error({non_integer_value, V, Path})
    end,
    upsert(Path, UpdatedValue, Tree);
increment(_Path, Value, _Tree) ->
    error({non_integer_value, Value}).

find_child(Name, []) ->
    #{name=>Name};
find_child(Name, [#{name:=Name}=Child|_]) ->
    Child;
find_child(Name, [_|Children]) ->
    find_child(Name, Children).
upsert_child(Child, Children) ->
    try upsert_child(Child, Children, []) of
        Res -> Res
    catch
        throw:Res -> Res
    end.
upsert_child(Child, [], Children) ->
    lists:reverse([Child|Children]);
upsert_child(#{name:=Name}=Child, [#{name:=Name}|Children0], Children1) ->
    throw(lists:reverse(Children1)++[Child|Children0]);
upsert_child(Child0, [Child1|Children0], Children1) ->
    upsert_child(Child0, Children0, [Child1|Children1]).



