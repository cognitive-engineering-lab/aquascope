# <span>Aquascope: Look Beneath the Surface of Rust</span><img src="https://user-images.githubusercontent.com/20209337/214093362-cb677ea0-8fe1-48b5-914b-839822dcf3ca.png" style="float: right; border: 1px solid #555;" height="300" /></p>

[![tests](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml/badge.svg)](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml)
[![crates.io](https://img.shields.io/crates/v/mdbook-aquascope.svg)](https://crates.io/crates/mdbook-aquascope)
[![docs](https://img.shields.io/badge/docs-built-blue)](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/)

_Aquascope_ is a tool that generates interactive visualizations of Rust programs. These visualizations show how Rust's borrow checker "thinks" about a program, and how a Rust program actually executes. [Click here for a live demo.](https://cognitive-engineering-lab.github.io/aquascope/)

<!-- Borrow check information is reformulated in terms of _permissions_, a new pedagogy of ownership to provide learners with a notional machine for this kind of static analysis. Example visualizations and this new pedagogy are demonstrated in the [Rust Book Experiment](https://rust-book.cs.brown.edu/). -->

> :warning: Aquascope is experimental and under active development!

## Installation

We provide an [mdBook](https://rust-lang.github.io/mdBook/) preprocessor `mdbook-aquascope` that embeds Aquascope diagrams into an mdBook. You can install `mdbook-aquascope` either via [crates.io](https://crates.io) or from source.

### From crates.io

You can run `cargo install mdbook-aquascope` to install from [crates.io](https://crates.io). 

> Note, Aquascope is tied to a specific version of `rustc` and you will need to use toolchain `nightly-2022-12-07`.

To embed the visualizations you will also need the crate [`aquascope_front`](https://github.com/cognitive-engineering-lab/aquascope/tree/main/crates/aquascope_front). Due to unpublished dependencies this crate is not available from crates.io but can be installed via `cargo install --git https://github.com/cognitive-engineering-lab/aquascope aquascope_front`. 

<!--
```sh
cargo install mdbook-aquascope
``` -->

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

### Available visualizations

Currently, Aquascope supports three types of visualizations:

#### Permission boundaries

Aquascope will determine the permission expected for a path usage and display this along with the actual permissions on the path. Unsatisfied permissions provide additional information on hover to help explain the discrepancy.

<table>
    <tr>
        <td>
<pre>
<code style="display: block;">
```aquascope,boundaries
fn main() {
    let mut x = 1;
    let y = &mut x;
    println!("{} = {}", x, *y);
}```
</code>
</pre>
        </td>
        <td>
            <img src="https://user-images.githubusercontent.com/20209337/215321806-bba27857-70ed-4371-98bd-5e7b5dfd884f.png" />
        </td>
    </tr>
</table>

#### Permission steps

Tracking how permissions change throughout a program is difficult, especially when factors such as [liveness](https://en.wikipedia.org/wiki/Live-variable_analysis) influence the static analysis. Aquascope will insert _steps_ that show how and _why_ permissions change.

<table>
    <tr>
        <td>
<pre>
<code style="display: block;">
```aquascope,stepper
fn main() {
    let mut x = 1;
    let y = &mut x;
    println!("{} = {}", x, *y);
}```
</code>
</pre>
        </td>
        <td>
            <img src="https://user-images.githubusercontent.com/20209337/215321846-377f3adb-9e4b-4d9c-8223-fd344296b32d.png" />
        </td>
    </tr>
</table>

#### Runtime execution

Program state visualization is a well-known tool that visualizes the runtime execution of a program. With Aquascope, you can specify which states of a program you'd like to show, and even run programs that don't pass the borrow checker!

<table>
    <tr>
        <td>
<pre>
<code style="display: block;">
```aquascope,interpreter,concreteTypes=true
fn main() {
    let m1 = String::from("Hello");
    let m2 = String::from("world");`[]`
    greet(&m1, &m2); // note the ampersands
    let s = format!("{} {}", m1, m2);
}

fn greet(g1: &String, g2: &String) { // note the ampersands
`[]`println!("{} {}!", g1, g2);
}```
</code>

</pre>
        </td>
        <td>
            <img src="https://user-images.githubusercontent.com/20209337/215325005-6c613d98-8b69-45f3-879a-c68c86940f83.png" />
        </td>
    </tr>

</table>

### Aquascope annotations

Aquascope provides a set of annotations for simple customization. Similar to mdBook, any line of code with a preceding `#` is _hidden_. Additionally, each visualization may provide its own set of specific annotations, these are outlined below.

#### Permission steps

Visualizing permission steps can be quite intrusive but oftentimes you may want to just focus on a handful of lines, or even specific paths. This can be achieved by providing a _step annotation_ at the end of a line. For example, the annotation `` `(focus,paths:x)` `` indicates that this line should be focused (shown by default) and all paths except `x` are hidden in a dropdown.

> Note, these annotations are line specific. The default is to show _all_ lines and paths unless something is specified.

<table>
    <tr>
        <td>
<pre>
<code style="display: block;">
```aquascope,stepper
# fn main() {
  let mut x = 1;
  let y = &x; `(focus,paths:x)`
  let z = *y; `(focus,paths:x)`
  x += z;
# }```
</code>
</pre>
        </td>
        <td>
            <kbd>
                <img src="https://user-images.githubusercontent.com/20209337/215325679-5ffc4ea8-6246-4d2e-965c-3baddfc26ad4.gif" />
            </kbd>
        </td>
    </tr>
</table>

## Having trouble?

If you want to use Aquascope but are having trouble finding the relevant information, please leave an issue or get in touch!
