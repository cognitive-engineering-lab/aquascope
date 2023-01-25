# <p style=""><span>Aquascope</span><img src="https://user-images.githubusercontent.com/20209337/214093362-cb677ea0-8fe1-48b5-914b-839822dcf3ca.png" style="float: right; border: 1px solid #555;" height="300" /> </p>

[![tests](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml/badge.svg)](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml)

## Looking under the surface of rustc, _at a safe distance_.

Aquascope is a tool that generates visualizations aiming to further program understanding. It's distributed as a editor embedable into [mdBook](https://rust-lang.github.io/mdBook/) via a preprocessor or you can try it out [in the browser](https://gavinleroy.com/aquascope).

<!-- Borrow check information is reformulated in terms of _permissions_, a new pedagogy of ownership to provide learners with a notional machine for this kind of static analysis. Example visualizations and this new pedagogy are demonstrated in the [Rust Book Experiment](https://rust-book.cs.brown.edu/). -->

> :warning: Aquascope is experimental and under active development!

**TODO** insert a gif demonstrating visualizations

## Installation

### From crates.io

```sh
cargo install mdbook-aquascope
```

### From source

You will first need Cargo and [npm](https://www.npmjs.com/) installed, then you can run:

```sh
git clone https://github.com/cognitive-engineering-lab/aquascope.git
npm install -g graco
cargo make init-bindings
cd frontend && graco prepare
cargo miri setup
cargo install --path crates/mdbook-aquascope
```

## Usage

### Available Visualizations

Currently, Aquascope supports three types of visualizations:

- _Permission boundaries_: Aquascope will determine the permission expected for a path usage and display this along with the actual permissions on the path. Unsatisfied permissions provide additional information on hover to help explain the discrepancy.

<table>
    <tr>
        <td>
<code>
```aquascope,boundaries
fn main() {
    let mut x = 1;
    let y = &mut x;
    println!("{} = {}", x, *y);
}
```
</code>
        </td>
        <td>
        TODO: insert image
        </td>
    </tr>
</table>

- _Permission steps_: tracking how permissions change throughout a program is difficult, especially when factors such as [liveness](https://en.wikipedia.org/wiki/Live-variable_analysis) influence the static analysis. Aquascope will insert _steps_ that show how and _why_ permissions change.

<table>
    <tr>
        <td>
<code>
```aquascope,stepper
fn main() {
    let mut x = 1;
    let y = &mut x;
    println!("{} = {}", x, *y);
}
```
</code>
        </td>
        <td>
        TODO: insert image
        </td>
    </tr>
</table>

- _Runtime execution_: program state visualization is a well-known tool that visualizes the runtime execution of a program. With Aquascope, you can specify which states of a program you'd like to show, and even run programs that don't pass the borrow checker!

<table>
    <tr>
        <td>
<code>
```aquascope,interpreter,concreteTypes=true
fn main() {
    let s1 = String::from("Hello");
    let s3 = add_suffix(s1);
    println!("{s3}");
}
fn add_suffix(mut s2: String) -> String {
    s2.push_str(" world");
    s2
}
```
</code>
        </td>
        <td>
        TODO: insert image
        </td>
    </tr>

</table>

### Aquascope Annotations

Aquascope provides a set of annotations for each visualization to allow for simple customization. Similar to `mdBook`

#### Stepper Annotations

Visualizing permission steps can be quite intrusive but oftentimes you may want to just focus on a handful of lines, or even specific paths. This can be achieved by providing a _step annotation_ at the end of a line. For example, the annotation `` `(focus,paths:x)` `` indicates that this line should be focused (shown by default) and all paths except `x` are hidden.
These annotations are line specific. Note, that if no lines are specified to be focused then _all_ lines are focused by default, and a similar rule applies per-line for paths.

<table>
    <tr>
        <td>
<code>
```aquascope,stepper
fn main() {
    let mut x = 1;
    let y = &mut x; `(focus,paths:x)`
    println!("{} = {}", x, *y);
}
```
</code>
        </td>
        <td>
        TODO: insert image
        </td>
    </tr>
</table>

### Interpreter Annotations

TODO

## Limitations
