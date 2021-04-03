
# elm-datagrid

An opinionated library for visualizing data in charts and grids.

The API is deliberately limited; for access to lower-level elements, try [`elm-visualization`](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/) for charts and [`elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) for UI (both used heavily by this library).

## Live Examples

- ChartGrid: [US Equities Market Summary](https://tarokuriyama.com/useq/)


## Build

### Quick

All Elm projects can be built with [`elm make`](https://elmprogramming.com/elm-make.html), such as:

```
elm make examples/Main.elm --optimize --output=elm.js
```

... which builds the example specified in `examples/Main.elm` and compiles it to `elm.js`.


### Full

To follow the "full" build for this repo, run the commands in the `all.do` script (`redo all` if you have [`redo`](https://redo.readthedocs.io/en/latest/), or run it as a shell script like `sh all.do`).

The full build includes a number of Elm-ecosystem dependencies, which can be installed with [`npm`](https://nodejs.org/en/) like so (omit the `-g` flag to install locally for the repo):

```
npm install -g elm-format
npm install -g elm-test
npm install -g elm-optimize-level-2
npm install -g elm-minify
```

Note that `elm-minify` is deprecated. For an alternative minification, see the [`terser` command recommended by `elm-optimize-level-2`](https://github.com/mdgriffith/elm-optimize-level-2/blob/HEAD/notes/minification.md).

## Structure

The intended "API" consists of the modules immediately under [`src/DataGrid`](https://github.com/tkuriyama/elm-datagrid/tree/master/src/DataGrid).

Most modules have a corresponding example of the same naem under [`examples/Examples`](https://github.com/tkuriyama/elm-datagrid/tree/master/examples/Examples). The repo ships with sample data for each example, under [`examples/SampleData`](https://github.com/tkuriyama/elm-datagrid/tree/master/examples/SampleData).


## See Also

- [FAQ](https://github.com/tkuriyama/elm-datagrid/blob/master/docs/faq.md)
- [Charts](https://github.com/tkuriyama/elm-datagrid/blob/master/docs/charts.md)
- [Grids](https://github.com/tkuriyama/elm-datagrid/blob/master/docs/grids.md)

