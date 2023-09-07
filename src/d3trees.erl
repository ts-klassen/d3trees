-module(d3trees).

-export([
        new/1
      , upsert/3
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
        PathBelowRoot::path() , Value::value(), Tree::tree()
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



