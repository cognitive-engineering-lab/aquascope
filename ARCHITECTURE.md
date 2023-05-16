# Architectural Overview

This document provides a high-level overview into the architecture of Aquascope. The goal is to provide enough information such that a potential contributor can orient themselves relatively quickly within the database. It _doesn't_ explain relevant analyses or related concepts, for that you should be looking at the [documentation](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/).

# TOC

1. [Runtime visualization](#runtime-visualization)
2. [Static visualization](#static-visualization)

## Runtime visualization

TODO

## Static visualization

Aquascope is a rustc plugin, utilizing the [`rustc_plugin`](https://github.com/willcrichton/flowistry) crate. Below is a rough diagram showing the control-flow of the Aquascope project, it's important to focus on the three components with gear icons in the top-right corner.

![lifecycle](https://github.com/cognitive-engineering-lab/aquascope/assets/20209337/005e10e6-d3d0-42dd-a048-8244aeab2961)

1. _The `front`_ (green box) reads a Rust crate and splits this up into bodies. You should be familiar with a MIR [`Body`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Body.html) but we really care about the [BodyWithBorrowckFacts](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_borrowck/struct.BodyWithBorrowckFacts.html) which contains the borrow check information provided by Polonius. The code for this is found in `aquascope_front`, it's a simple crate that gets these bodies from rustc and then serializes the output information post-analysis. There's not much else to see here.

2. The _Permissions analysis_ (orange box) produces a [`PermissionsCtxt`](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/analysis/permissions/struct.PermissionsCtxt.html).

   > :exclamation: The `PermissionsCtxt` is the core data structure for working with permissions. It provides conversion functions to (and from) rustc primitives, as well as accessors for the permissions on a `Path`. Please read its [documentation](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/analysis/permissions/struct.PermissionsCtxt.html).

   The permissions analysis works by translating the Polonius input/output facts into something permissions oriented, the translation is quite straightforward and there's a nice set of Datalog rules provided for ease-of-reading. The Datalog rules are written in `aquascope::analysis::permissions::output`, but this isn't necessary to understand, only for the curious. Because Aquascope stores more information about a `Body`, we use our own `AquascopeFacts` that parallel the [`RustcFacts`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_borrowck/consumers/struct.RustcFacts.html). As stated above, the `PermissionsCtxt` provides conversion functions two and _it's unsafe to use the raw facts from rustc_.

   After building the `PermissionsCtxt` the [`AquascopeAnalysis`](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/analysis/struct.AquascopeAnalysis.html) can be constructed. This struct provides helper functions for working with source spans. For example, it holds information about the live ranges of a borrow and the source spans for this range.

3. _Generating visualizations_ (blue boxes) are where we actually use the permissions facts to generate information for a visualization. Currently, we only have two analyses which are internally referred to as the _boundaries_ and _steps_ analysis. In the Aquascope diagrams, the boundaries analysis computes the expected vs actual permissions for a single path usage, which are shown as the stacked letter icons. The steps analysis computes the differences in permissions between control flow points, shown as the tables to the right hand side. The complete boundaries analysis lives at [`aquascope::analysis::boundaries`](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/analysis/boundaries/index.html) and the stepper correspondingly at [`aquascope::analysis::stepper`](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/analysis/stepper/index.html).

   > :warning: The stepper analysis is more complicated than you might think and it isn't recommended reading for newcomers.

   The module documentation for the boundaries analysis provides a detailed walk through of how it works and utilizes permissions and span information. If you yourself are looking to add a new analysis this is considered a must read.

## Frontend

We use [CodeMirror 6 (CM)](https://codemirror.net) for an interactive environment as well as visualizations rendering the visualizations. The static diagrams are represented as a CM [`Decoration`](https://codemirror.net/docs/ref/#view.Decoration).

**TODO**: talk about serializing and generating TS bindings.
