# Contributing

## Quick Start

For an overview of Aquascope's structure, please refer yourself to the [ARCHITECTURE.md](/ARCHITECTURE.md). To manage style we use the committed rustfmt file and the [`pre-commit`](https://pre-commit.com/) utility. Please follow the installation instructions there and then run `pre-commit install` to locally install the git hook scripts.

### Testing

End-to-end testing is done with snapshot testing via [insta](https://github.com/mitsuhiko/insta). Use `cargo insta test` to run all tests. When possible, write local tests for functionality. New snapshot tests can be included by adding a `.test` file in the `crates/aquascope/tests/(steps|boundaries|interpreter)` directories.

### Debugging

We use the local playground to debug Aquascope. In one terminal, run `cargo run`, to run the local server. In a separate terminal, navigate to the `frontend` directory and run `depot --no-fullscreen build --watch`. The playground is live at `localhost:5173`, the server will log debug information about the programs you analyze.

> Note, the local playground **does not** run within a sandbox.

To see the full permissions analysis and get all debug output start a server with `cargo make watch`. Then, start an instance of the frontend by running `depot build -w` from the `frontend` directory. The page is served on `localhost:5173`.

## Asking questions

Please ask questions on our [discussion board](https://github.com/cognitive-engineering-lab/aquascope/discussions)! Or file an issue.

## Issue Labels

- [good first issue](https://github.com/cognitive-engineering-lab/aquascope/labels/good%20first%20issue) Great for dipping your toes in the water.
- [big project](https://github.com/cognitive-engineering-lab/aquascope/labels/big-project) These are changes or features that will take a bit of work. Many issues labeled with this also require some sort of interface design, so please start a discussion if you're interested.
- [bug](https://github.com/cognitive-engineering-lab/aquascope/labels/bug) An unexpected behavior according to the semantics of the permissions model (also commonly a UI bug). These vary in difficulty, tell us if you're interested and _ask questions_ if you get stuck.
