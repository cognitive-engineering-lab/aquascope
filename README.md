# <span>Aquascope: Look Beneath the Surface of Rust</span><img src="https://user-images.githubusercontent.com/663326/219528078-e8792f31-02b3-447f-97ed-f3c0fbb4f557.png" style="float: right; border: 1px solid #555;" height="250" /></p>

[![tests](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml/badge.svg)](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml)
[![crates.io](https://img.shields.io/crates/v/mdbook-aquascope.svg)](https://crates.io/crates/mdbook-aquascope)
[![docs](https://img.shields.io/badge/docs-built-blue)](https://cognitive-engineering-lab.github.io/aquascope/doc/aquascope/)

Aquascope is a tool that generates interactive visualizations of Rust programs. These visualizations show how Rust's borrow checker "thinks" about a program, and how a Rust program actually executes. Here is a sample output of Aquascope:

<img alt="Example Aquascope output" src="https://user-images.githubusercontent.com/663326/219532624-a9605540-dac9-4d9b-a5f2-8a3d1ca81f50.png" height="550" />

[Click here for a live demo.](https://cognitive-engineering-lab.github.io/aquascope/)

:warning: **Aquascope is research software and under active development!** :warning:

## Installation

We provide an [mdBook](https://rust-lang.github.io/mdBook/) preprocessor that embeds Aquascope diagrams into an mdBook. To use it, you need to install the `mdbook-aquascope` and `cargo-aquascope` binaries as follows:

```sh
cargo install mdbook-aquascope
rustup toolchain install nightly-2022-12-07 -c rust-src rustc-dev llvm-tools-preview miri
cargo +nightly-2022-12-07 install --git https://github.com/cognitive-engineering-lab/aquascope aquascope_front
cargo +nightly-2022-12-07 miri setup
```

Note that `cargo-aquascope` is installed via `aquascope_front` and must be installed via git and with a specific nightly toolchain. The `miri setup` command is a necessary prerequisite to running the Aquascope interpreter.

### From Source

If you want to install from source, you will first need Cargo and [npm](https://www.npmjs.com/) installed, then you can run:

```sh
git clone https://github.com/cognitive-engineering-lab/aquascope.git
cd aquascope
npm install -g graco
cargo make init-bindings
cd frontend && graco prepare
cargo install --path crates/aquascope_front
cargo install --path crates/mdbook-aquascope
```

## Usage

First, enable `mdbook-aquascope` in your mdBook's `book.toml` like so:

```toml
# book.toml
[preprocessor.aquascope]
```

Then add an Aquascope code block to one of your Markdown source files like this:

    ```aquascope,interpreter
    #fn main() {
    let mut s = String::from("hello ");`[]`
    s.push_str("world");`[]`
    #}
    ```

Further documentation on the syntax and configuration of Aquascope blocks will be provided once the interface is more stable.

<!--
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

-->

## Having trouble?

If you want to use Aquascope but are having trouble finding the relevant information, please leave an issue or email us at <wcrichto@brown.edu> and <gagray@ethz.ch>.
