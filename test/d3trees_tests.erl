-module(d3trees_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

% Test the new/1 function
new_test() ->
    % Act: Call the new/1 function
    Result = d3trees:new("RootNode"),
    
    % Assert: Check that the result is as expected
    Expected = #{name => "RootNode"},
    ?assertEqual(Expected, Result).

% Test the upsert/3 function
upsert_test() ->
    % Arrange: Create a sample tree
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the upsert/3 function
    UpdatedTree = d3trees:upsert(["FirstNode", "SecondNode"], "SecondNodeValue", InitialTree),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue", children => [#{name => "SecondNode", value => "SecondNodeValue"}]}]},
    ?assertEqual(ExpectedTree, UpdatedTree).

% Test when the tree is empty (should create a new tree with the specified value).
empty_tree_test() ->
    % Act: Call the upsert/3 function on an empty tree
    Result = d3trees:upsert(["FirstNode", "SecondNode"], "SecondNodeValue", #{name => "EmptyTree"}),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "EmptyTree", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "SecondNodeValue"}]}]},
    ?assertEqual(ExpectedTree, Result).

% Test when the specified path already exists in the tree (should update the value).
update_existing_path_test() ->
    % Arrange: Create a sample tree with an existing path
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "OldValue"}]}]},
    
    % Act: Call the upsert/3 function to update an existing path
    Result = d3trees:upsert(["FirstNode", "SecondNode"], "NewValue", InitialTree),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "NewValue"}]}]},
    ?assertEqual(ExpectedTree, Result).

% Test when the specified path doesn't exist, and it's a deep nested path (should create the path).
deep_nested_path_test() ->
    % Arrange: Create a sample tree with a different path
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the upsert/3 function with a deep nested path
    Result = d3trees:upsert(["FirstNode", "SecondNode", "ThirdNode"], "NewValue", InitialTree),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue", children => [#{name => "SecondNode", children => [#{name => "ThirdNode", value => "NewValue"}]}]}]},
    ?assertEqual(ExpectedTree, Result).

% Test when the specified path doesn't exist, and it's a shallow path (should create the path).
shallow_path_test() ->
    % Arrange: Create a sample tree with a different path
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the upsert/3 function with a shallow path
    Result = d3trees:upsert(["SecondNode"], "SecondNodeValue", InitialTree),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}, #{name => "SecondNode", value => "SecondNodeValue"}]},
    ?assertEqual(ExpectedTree, Result).

% Test when the specified path exists and has children (should update the value).
update_existing_path_with_children_test() ->
    % Arrange: Create a sample tree with an existing path that has children
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "OldValue"}]}]},
    
    % Act: Call the upsert/3 function to update an existing path
    Result = d3trees:upsert(["FirstNode", "SecondNode"], "NewValue", InitialTree),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "NewValue"}]}]},
    ?assertEqual(ExpectedTree, Result).

% Test when the specified path exists and has no children (should update the value).
update_existing_path_without_children_test() ->
    % Arrange: Create a sample tree with an existing path that has no children
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "SecondNodeValue"}]}]},
    
    % Act: Call the upsert/3 function to update an existing path
    Result = d3trees:upsert(["FirstNode"], "NewValue", InitialTree),
    
    % Assert: Check that the result is as expected
    ExpectedTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "NewValue", children => [#{name => "SecondNode", value => "SecondNodeValue"}]}]},
    ?assertEqual(ExpectedTree, Result).

