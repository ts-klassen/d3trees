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

% Test the lookup/2 function when the path exists in the tree.
lookup_existing_path_test() ->
    % Arrange: Create a sample tree with an existing path
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", value => "SecondNodeValue"}]}]},
    
    % Act: Call the lookup/2 function to retrieve a value
    Result = d3trees:lookup(["FirstNode", "SecondNode"], InitialTree),
    
    % Assert: Check that the result is as expected
    Expected = {value, "SecondNodeValue"},
    ?assertEqual(Expected, Result).

% Test the lookup/2 function when the path doesn't exist in the tree.
lookup_nonexistent_path_test() ->
    % Arrange: Create a sample tree with a different path
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the lookup/2 function to retrieve a value for a non-existent path
    Result = d3trees:lookup(["NonExistentNode"], InitialTree),
    
    % Assert: Check that the result is 'none'
    ?assertEqual(none, Result).

% Test the lookup/2 function when the tree is empty (should return 'none').
lookup_empty_tree_test() ->
    % Arrange: Create an empty tree
    EmptyTree = #{name => "EmptyTree"},
    
    % Act: Call the lookup/2 function on an empty tree
    Result = d3trees:lookup(["FirstNode", "SecondNode"], EmptyTree),
    
    % Assert: Check that the result is 'none'
    ?assertEqual(none, Result).

% Test the lookup/2 function when the specified path is partially present in the tree.
lookup_partial_path_test() ->
    % Arrange: Create a sample tree with a partial path
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the lookup/2 function with a path that is partially present
    Result = d3trees:lookup(["FirstNode", "NonExistentNode"], InitialTree),
    
    % Assert: Check that the result is 'none'
    ?assertEqual(none, Result).

% Test the lookup/2 function when the tree has multiple levels of nesting.
lookup_nested_tree_test() ->
    % Arrange: Create a sample tree with multiple levels of nesting
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", children => [#{name => "SecondNode", children => [#{name => "ThirdNode", value => "ThirdNodeValue"}]}]}]},
    
    % Act: Call the lookup/2 function to retrieve a value from a nested path
    Result = d3trees:lookup(["FirstNode", "SecondNode", "ThirdNode"], InitialTree),
    
    % Assert: Check that the result is as expected
    Expected = {value, "ThirdNodeValue"},
    ?assertEqual(Expected, Result).

% Test the lookup/2 function when the path is an empty list (should return 'none').
lookup_empty_path_test() ->
    % Arrange: Create a sample tree
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the lookup/2 function with an empty path
    Result = d3trees:lookup([], InitialTree),
    
    % Assert: Check that the result is 'none'
    ?assertEqual(none, Result).

% Test the lookup/2 function when the tree has multiple branches and the path leads to a leaf node.
lookup_leaf_node_test() ->
    % Arrange: Create a sample tree with multiple branches and a leaf node
    InitialTree = #{name => "RootNode", children => [
        #{name => "BranchA", children => [#{name => "LeafA1", value => "LeafA1Value"}]},
        #{name => "BranchB", children => [#{name => "LeafB1", value => "LeafB1Value"}]}
    ]},
    
    % Act: Call the lookup/2 function to retrieve a value from a leaf node
    Result = d3trees:lookup(["BranchA", "LeafA1"], InitialTree),
    
    % Assert: Check that the result is as expected
    Expected = {value, "LeafA1Value"},
    ?assertEqual(Expected, Result).

% Test the lookup/2 function when the path contains non-existent intermediate nodes.
lookup_nonexistent_intermediate_nodes_test() ->
    % Arrange: Create a sample tree with missing intermediate nodes
    InitialTree = #{name => "RootNode", children => [#{name => "FirstNode", value => "FirstNodeValue"}]},
    
    % Act: Call the lookup/2 function with a path containing non-existent intermediate nodes
    Result = d3trees:lookup(["FirstNode", "NonExistentNode", "LeafNode"], InitialTree),
    
    % Assert: Check that the result is 'none'
    ?assertEqual(none, Result).

% Test the lookup/2 function when the tree contains nodes with the same name at different levels.
lookup_same_named_nodes_test() ->
    % Arrange: Create a sample tree with nodes having the same name at different levels
    InitialTree = #{name => "RootNode", children => [
        #{name => "FirstNode", children => [
            #{name => "SecondNode", value => "SecondNodeValue"},
            #{name => "ThirdNode", value => "ThirdNodeValue"}
        ]}
    ]},
    
    % Act: Call the lookup/2 function to retrieve values from nodes with the same name
    Result1 = d3trees:lookup(["FirstNode", "SecondNode"], InitialTree),
    Result2 = d3trees:lookup(["FirstNode", "ThirdNode"], InitialTree),
    
    % Assert: Check that the results are as expected
    Expected1 = {value, "SecondNodeValue"},
    Expected2 = {value, "ThirdNodeValue"},
    ?assertEqual(Expected1, Result1),
    ?assertEqual(Expected2, Result2).

% Test the increment/3 function when the path exists in the tree.
increment_existing_path_test() ->
    % Arrange: Create a sample tree with an existing path and an integer value
    InitialTree = #{name => "RootNode", children => [#{name => "x", children => [#{name => "y", value => 10}]}]},
    
    % Act: Call the increment/3 function to increment the value
    UpdatedTree = d3trees:increment(["x", "y"], 5, InitialTree),
    
    % Assert: Check that the value is incremented as expected
    Result = d3trees:lookup(["x", "y"], UpdatedTree),
    Expected = {value, 15},
    ?assertEqual(Expected, Result).

% Test the increment/3 function when the path doesn't exist in the tree (should create the path).
increment_nonexistent_path_test() ->
    % Arrange: Create a sample tree with a different path
    InitialTree = #{name => "RootNode", children => [#{name => "a"}]},
    
    % Act: Call the increment/3 function on a non-existent path
    UpdatedTree = d3trees:increment(["x", "y"], 5, InitialTree),
    
    % Assert: Check that the path is created and the value is set correctly
    Result = d3trees:lookup(["x", "y"], UpdatedTree),
    Expected = {value, 5},
    ?assertEqual(Expected, Result).

% Test the increment/3 function when given non-integer values.
increment_non_integer_value_test() ->
    % Arrange: Create a sample tree with an existing path and a integer value
    InitialTree = #{name => "RootNode", children => [#{name => "x", children => [#{name => "y", value => 10}]}]},
    
    % Act: Call the increment/3 function on a path with a non-integer value
    {'EXIT', {Result, _}} = catch d3trees:increment(["x", "y"], "z", InitialTree),
    
    % Assert: Check that an error is returned
    ?assertMatch({non_integer_value, "z"}, Result).

% Test the increment/3 function when the path exists but the value is not an integer.
increment_existing_path_non_integer_value_test() ->
    % Arrange: Create a sample tree with an existing path and a non-integer value
    InitialTree = #{name => "RootNode", children => [#{name => "x", children => [#{name => "y", value => "String"}]}]},
    
    % Act: Call the increment/3 function on a path with a non-integer value
    {'EXIT', {Result, _}} = catch d3trees:increment(["x", "y"], 5, InitialTree),
    
    % Assert: Check that an error is returned
    ?assertMatch({non_integer_value, "String", ["x", "y"]}, Result).

