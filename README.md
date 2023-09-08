# D3Trees

An Erlang tree library for data-driven documents.

## Installation

You can include this library in your Erlang project by adding it as a dependency in your rebar.config file:

```erlang
{deps, [
    {d3trees, {git, "https://github.com/ts-klassen/d3trees.git", {tag, "0.1.0"}}}
]}.
```

Ensure that you have [Rebar3](https://www.rebar3.org/) installed to manage your dependencies.

## Usage

This library provides functionality for creating and manipulating tree structures. It's particularly useful for scenarios where tree-like data structures are needed, such as visualizations in [D3](https://d3js.org) like [Collapsible tree](https://observablehq.com/@d3/collapsible-tree?intent=fork), [Zoomable icicle](https://observablehq.com/@d3/zoomable-icicle?intent=fork), and [Zoomable sunburst](https://observablehq.com/@d3/zoomable-sunburst?intent=fork). Use libraries like [jsone](https://github.com/sile/jsone) to convert the Tree into JSON.

Here's a basic example of how to use it:


```erlang
%% Create a new tree node
1> Tree = d3trees:new("RootNode").
#{name => "RootNode"}

%% Upsert a value into the tree
2> UpdatedTree = d3trees:upsert(["FirstNode", "SecondNode"], "SecondNodeValue", Tree).
#{
    name => "RootNode"
  , children => [#{
        name => "FirstNode"
      , children => [#{
            name => "SecondNode"
          , value => "SecondNodeValue"
        }] % end of "FirstNode" children
    }] % end of "RootNode" children
}
%% lookup a value in the tree
3> d3trees:lookup(["FirstNode", "SecondNode"], UpdatedTree).
{value, "SecondNodeValue"}
4> d3trees:lookup(["FirstNode"], UpdatedTree).
none
%% increment a value in the tree
5> IntegerTree = d3trees:increment(["x", "y"], 10, Tree).
#{
    name => "RootNode"
  , children => [#{
        name => "x"
      , children => [#{
            name => "y"
          , value => 10
        }] % end of "x" children
    }] % end of "RootNode" children
}
6> d3trees:increment(["x", "y"], 2, IntegerTree).
#{
    name => "RootNode"
  , children => [#{
        name => "x"
      , children => [#{
            name => "y"
          , value => 12
        }] % end of "x" children
    }] % end of "RootNode" children
}
```


## License

This project is licensed under the Apache License - see the [LICENSE](LICENSE) file for details.

## Authors and Acknowledgments

- **ts-klassen** - Author

## Version History

- **0.1.0** (Initial Commit) - [Release Notes](https://github.com/ts-klassen/d3trees/releases/tag/0.1.0)
- **0.1.1** - [Release Notes](https://github.com/ts-klassen/d3trees/releases/tag/0.1.1)

## Testing

To run the provided EUnit tests, you can use Rebar3:

```bash
rebar3 eunit
```

## Contributing

Please note that there are no specific contributing guidelines, and support/contact information is not provided at the moment. If you have any questions or suggestions, feel free to open an issue on the [GitHub repository](https://github.com/ts-klassen/d3trees/issues).

