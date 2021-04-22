
# elm-datagrid

An opinionated library for visualizing data in charts and grids.

The API is deliberately limited; for access to lower-level elements, try [`elm-visualization`](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/) for charts and [`elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) for UI (both used heavily by this library).

## Live Examples

- ChartGrid: [US Equities Market Summary](https://tarokuriyama.com/useq/)


## Build

To follow the "full" build for this repo, run the commands in the `all.do` script (`redo all` if you have [`redo`](https://redo.readthedocs.io/en/latest/), or run it as a shell script like `sh all.do`).


See [this page](https://tkuriyama.github.io/general/2021/04/22/Building-Elm.html) for an explanation of the build steps and dependencies.



## Structure

- The intended "API" consists of the modules immediately under [`src/DataGrid`](https://github.com/tkuriyama/elm-datagrid/tree/master/src/DataGrid).

- Most modules have a corresponding example of the same name under [`examples/Examples`](https://github.com/tkuriyama/elm-datagrid/tree/master/examples/Examples). 

- The repo ships with sample data for each examplxe, under [`examples/SampleData`](https://github.com/tkuriyama/elm-datagrid/tree/master/examples/SampleData).


## Compatibility

### Browser Compatibility

| Browser | Compatibility                        |
|:--------|:-------------------------------------|
| Chrome  | Supported                            |
| Safari  | Tentatively Supported                |
| Firefox | Mostly Supportted (minor CSS issues) |
| Other   | Not Tested                           |

### Browser Extensions

In addition to general browser compabitility, some browser extensions may cause issues (if they also rewrite pages, like screen readers or dark modes).

For Dark Reader users: setting the Mode to `Filter` instead of `Dynamic`usually resolves issues (relating to the "Virtual DOM" when redrawing charts).

