# D3Trees

[English](./README.md) | 日本語

An Erlang tree library for data-driven documents.

## インストール

このライブラリをErlangプロジェクトに含めるには、rebar.configファイルに依存関係として追加します:

```erlang
{deps, [
    {d3trees, {git, "https://github.com/ts-klassen/d3trees.git", {tag, "0.1.0"}}}
]}.
```

依存関係を管理するために[Rebar3](https://www.rebar3.org/)をインストールしていることを確認してください。

## 使用方法

このライブラリは、ツリー構造を作成および操作する機能を提供します。特に、[D3](https://d3js.org) のようなビジュアライゼーション（例: [Collapsible tree](https://observablehq.com/@d3/collapsible-tree?intent=fork)、[Zoomable icicle](https://observablehq.com/@d3/zoomable-icicle?intent=fork)、[Zoomable sunburst](https://observablehq.com/@d3/zoomable-sunburst?intent=fork)）でツリー状のデータ構造が必要なシナリオに役立ちます。[jsone](https://github.com/sile/jsone) のようなライブラリを使用してツリーをJSONに変換できます。

以下は、使用方法の基本的な例です:

```erlang
%% 新しいツリーノードを作成
1> Tree = d3trees:new("RootNode").
#{name => "RootNode"}

%% ツリーに値を追加
2> UpdatedTree = d3trees:upsert(["FirstNode", "SecondNode"], "SecondNodeValue", Tree).
#{
    name => "RootNode"
  , children => [#{
        name => "FirstNode"
      , children => [#{
            name => "SecondNode"
          , value => "SecondNodeValue"
        }] % "FirstNode"の終わり
    }] % "RootNode"の終わり
}
```

## ライセンス

このプロジェクトはApache Licenseの下でライセンスされています。詳細は[LICENSE](LICENSE)ファイルを参照してください。

## 著者と謝辞

- **ts-klassen** - 著者

## バージョン履歴

- **0.1.0** (初回コミット) - [リリースノート](https://github.com/ts-klassen/d3trees/releases/tag/0.1.0)

## テスト

提供されているEUnitテストを実行するには、Rebar3を使用できます:

```bash
rebar3 eunit
```

特定の貢献ガイドラインは提供されておらず、サポート/連絡情報は現時点では提供されていません。質問や提案がある場合は、[GitHubリポジトリ](https://github.com/ts-klassen/d3trees/issues)で issue を開いてください。

